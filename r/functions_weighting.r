


#****************************************************************************************************
#                functions for constraints ####
#****************************************************************************************************

nnz <- function(x, wt0) {(x != 0) * 1}
nz <- function(x, wt0) {(x == 0) * 1}
npos <- function(x, wt0) {(x > 0) * 1}
nneg <- function(x, wt0) {(x < 0) * 1}

sumnz <- function(x, wt0) {(x != 0) * x}
sumpos <- function(x, wt0) {(x > 0) * x}
sumneg <- function(x, wt0) {(x < 0) * x}



#****************************************************************************************************
#                functions for optimization ####
#****************************************************************************************************

calc_constraints <- function(weights, data, constraint_vars){
  # weights: numeric vector
  # data: df with 1 column per constraint variable (plus other info); 1 row per person
  colSums(weights * select(data, constraint_vars))
}

# pdiff <- function(weights, data, constraints) {
#   # percent difference between calculated constraints and targets
#   calculated_constraints <- calc_constraints(weights, data, names(constraints))
#   (calculated_constraints - constraints) / constraints * 100
# }
# 
# 
# objfn <- function(weights, data, constraints, p=2){
#   # this objfn scales the data and constraint values used in the obj calculation
#   calculated_constraints <- calc_constraints(weights, data, names(constraints))
#   # scale the constraints and the calculations by the constraint values
#   diff_to_p <- (calculated_constraints / constraints - constraints / constraints)^p
#   obj <- sum(diff_to_p)
#   return(obj)
# }



#****************************************************************************************************
#                mma wfs functions ####
#****************************************************************************************************
eval_f_wtfs <- function(w, inputs) {
  # objective function - evaluates to a single number
  
  # w is the vector of weights computed in this iteration
  
  # the inputs list is passed to ALL functions
  
  # inputs$recipe is a data frame where each row defines an element of the objective function, with columns:
  #   obj.element -- a short name for the element, giving the variable involved and the function to be applied to it
  #   var -- the variable involved -- e.g., c00100 or e00200
  #   fn -- the function to be applied -- one of n.sum, val.sum, n.pos, val.pos, n.neg, val.neg
  #   multiplier -- a weighting factor that can make this element more important than other elements
  #     default is 1; larger numbers make an element more important
  #   target -- the value for this calculation as applied to the target puf file
  
  # inputs$synsub is a dataframe that is a subset of the synfile that only includes the columns needed to
  #   calculate the objective function. 
  #     The wt column is special in that its value is set to 1, to make
  #     calculations involving a weight variable easy. (Multiplying this iteration's weight vector, w, by
  #     the synsub$wt weight variable (i.e., 1), gives the tentative weight for the file. The sum of this is
  #     of course the sum of weights. THis allows us to treat the weight like it is any other variable.)
  
  # obj -- calculated below is the objective function value; it is the sum across all elements of the:
  #   priority-weighted 
  #     squared differences of the 
  #       sum of values calculated for an element using the weight vector from this iteration, minus the
  #         corresponding target value in the puf calculated  using its weights
  # ratio <- function(target.calc, target) ifelse(target==0, 1, target.calc / target)
  
  # for serial
  # objdf <- inputs$recipe %>%
  #   rowwise() %>%
  #   # the mutate step applies this element's function to this element's variable, using this iteration's weights:
  #   mutate(target.calc=do.call(fn, list(inputs$synsub, vname, w))) %>% 
  #   ungroup %>%
  #   mutate(obj=priority.weight * {(target.calc / scale - target / scale)^2})
  
  # for parallel
  # apply this element's function to this element's variable, using this iteration's weights:
  objdf <- inputs$recipe
  objdf$target.calc <- NA_real_
  for(i in 1:nrow(objdf)){
    objdf$target.calc[i] <- get(objdf$fn[i])(df=inputs$synsub, var=objdf$vname[i], w)
  }
  
  objdf <- objdf %>%
    mutate(obj=priority.weight * {(target.calc / scale - target / scale)^2})
  
  obj <- sum(objdf$obj)
  
  return(obj)
}


eval_grad_f_wtfs <- function(w, inputs){
  # gradient of objective function - a vector length w
  # giving the partial derivatives of obj wrt each w[i]
  
  # ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  
  # inputs will include:
  #   for each target element in the objective function:
  #     the sum -- s1 for first target element, and so on, and
  #     the multiplier vector (e.g., wages, cap gains, etc.)
  
  # http://www.derivative-calculator.net/
  # https://www.symbolab.com/solver/partial-derivative-calculator
  # Example where we only have one target element in the objective function, and only have
  #   3 records and therefore 3 weights, and need to return a vector of 3 partial derivatives
  # Notation: w1, w2, and w3 are the first 3 weights in the vector of weights
  #           a1, a2, and a3 are the 3 constants they are multiplied by (e.g., wages1, wages2, wages3)
  #           t is the sum or target we are comparing to
  #           p is the priority weight of this element of the objective function
  #           calc is the calculated value of the target with the new weights  
  # The objective function to minimize is p*(w1*a1 + w2*a2 + w3*a3 - s)^2
  # The first partial derivatives of the objective function are:
  #   wrt w1:          2*a1*p*(w1*a1 + w2*a2 + w3*a3 - t) = 2*a1*p*(calc - t)
  #   wrt w2:          2*a2*p*(w1*a1 + w2*a2 + w3*a3 - t) = 2*a2*p*(calc - t)
  #   wrt w3:          2*a3*p*(w1*a1 + w2*a2 + w3*a3 - t) = 2*a3*p*(calc - t)
  # If we have multiple target elements, each vector element will have a more complex sum
  
  wdf <- dplyr::tibble(wt.iter=w, wtnum=1:length(w))
  
  grad <- inputs$coeffs %>% 
    dplyr::left_join(wdf, by="wtnum") %>%
    dplyr::group_by(obj.element) %>%
    dplyr::mutate(calc=sum(wt.iter*coeff)) %>%
    dplyr::mutate(grad={2 * coeff * priority.weight * (calc - target)} / {scale^2}) %>%
    dplyr::group_by(wtnum) %>%
    dplyr::summarise(grad=sum(grad)) %>% # sum across all of the object elements for this particular weight
    ungroup
  
  return(grad$grad)
}


#****************************************************************************************************
#                Ipopt functions ####
#****************************************************************************************************

define_jac_g_structure_dense <- function(n_constraints, n_variables){
  # list with n_constraints elements
  # each is 1:n_variables
  lapply(1:n_constraints, function(n_constraints, n_variables) 1:n_variables, n_variables)
} 
# eval_jac_g_structure_dense <- define_jac_g_structure_dense(n_constraints=2, n_variables=4)


eval_jac_g_dense <- function(x, inputs){
  # the Jacobian is the matrix of first partial derivatives of constraints (these derivatives may be constants)
  # this function evaluates the Jacobian at point x
  
  # This is the dense version that returns a vector with one element for EVERY item in the Jacobian (including zeroes)
  # Thus the vector has n_constraints * n_variables elements
  # first, all of the elements for constraint 1, then all for constraint 2, etc...
  
  # because constraints in this problem are linear, the derivatives are all constants
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  
  return(inputs$cc_dense)
}

eval_g_dense <- function(x, inputs){
  unname(calc_constraints(inputs$wt * x, inputs$data, inputs$constraint_vars))
}


ipopt_reweight <- function(weights, data, constraint_df, ugroup.in, ftype.in, scale.in){
  if(scale.in) scale_text <- "scaled" else scale_text <- "unscaled"
  outfile <- paste0("d:/temp/logs/synlog_", ftype.in, "_", ugroup.in, "_", scale_text, ".out")
  
  # scale all values in data and constraints by constants
  scale_it <- function(tbsdf, scaling_df){
    sdf <- as.vector(t(scaling_df))
    sweep(tbsdf, MARGIN = 2, sdf, FUN = "/")
  }
  
  if(scale.in){
    # create a scaling data frame with 1 of scale factors, and a column for every constraint
    scaling_df <- constraint_df %>%
      filter(variable=="constraint") %>%
      select(-variable) %>%
      mutate_all(list(~ifelse(.==0, 1, (abs(.) / nrow(data)) * 100)))
    
    # scaling_df <- data %>%
    #   summarise_all(list(~max(abs(.), na.rm=TRUE) / 100)) %>%
    #   mutate_all(list(~ifelse(.==0, 1, .)))
    
    data <- scale_it(data, scaling_df)
    constraint_df[, -1] <- scale_it(constraint_df[, -1], scaling_df)
  }
  
  # arguments for all functions passed to ipoptr must be x, inputs
  inputs <- list()
  inputs$p <- 2
  inputs$wt <- weights
  inputs$data <- data
  inputs$constraint_vars <- names(constraint_df %>% select(-variable))
  inputs$cc_dense <- c(as.matrix(data[, inputs$constraint_vars] * weights)) # flattens the cc matrix
  
  xlb <- rep(0, nrow(data))
  xub <- rep(10e3, nrow(data))
  x0 <- rep(1, nrow(data))
  xub <- pmax(xub, 2*x0)
  
  constraints <- constraint_df %>% filter(variable=="constraint") %>% select(-variable) # do I need this??
  
  clb <- constraint_df %>% filter(variable=="clb") %>% select(-variable) %>% t %>% as.vector
  cub <- constraint_df %>% filter(variable=="cub") %>% select(-variable) %>% t %>% as.vector
  
  eval_jac_g_structure_dense <- define_jac_g_structure_dense(
    n_constraints=ncol(data[, names(constraints)]), 
    n_variables=nrow(data[, names(constraints)]))
  
  eval_h_structure <- lapply(1:length(inputs$wt), function(x) x) # diagonal elements of our Hessian
  
  # ma86 was faster in one test I did
  opts <- list("print_level" = 0,
               "file_print_level" = 5, # integer
               "linear_solver" = "ma86", # mumps pardiso ma27 ma57 ma77 ma86 ma97
               "max_iter"=200,
               "nlp_scaling_max_gradient" = 100, # 1e-3, # default 100
               "obj_scaling_factor" = 1, # 1e7, # default 1
               # "derivative_test"="first-order",
               # "derivative_test_print_all"="yes",
               "output_file" = outfile)
  
  result <- ipoptr(x0 = weights,
                   lb = xlb,
                   ub = xub,
                   eval_f = eval_f_xtop, # arguments: x, inputs
                   eval_grad_f = eval_grad_f_xtop,
                   eval_g = eval_g_dense, # constraints LHS - a vector of values
                   eval_jac_g = eval_jac_g_dense,
                   eval_jac_g_structure = eval_jac_g_structure_dense,
                   eval_h = eval_h_xtop, # the hessian is essential for this problem
                   eval_h_structure = eval_h_structure,
                   constraint_lb = clb,
                   constraint_ub = cub,
                   opts = opts,
                   inputs = inputs)
  
  return(result)
}

# df <- opt_pre %>% filter(ugroup==1)
# groups(df)
# count(df, ugroup)
# all_constraints <- all_constraint_vals
# scale.in <- TRUE
# weights <- df$wt0
# data <- df[, convars]

getwts_ipopt <- function(df, all_constraints, bounds, scale.in=FALSE){
  # get weights using ipoptr
  ftype.in <- first(df$ftype)
  ugroup.in <- first(df$ugroup)
  
  # create a 3-record data frame from bounds, of constraints, clb, and cub
  # so that all are in exactly the same order
  constraint_df <- bounds %>%
    filter(ftype==ftype.in, ugroup==ugroup.in)%>%
    select(good_constraint, constraint=target, clb, cub) %>%
    gather(variable, value, -good_constraint) %>%
    spread(good_constraint, value)
  convars <- names(constraint_df %>% select(-variable))
  
  print(paste0("Starting group: ", ugroup.in))
  ipopt_result <- ipopt_reweight(weights=df$wt0, data=df[, convars], constraint_df, ugroup.in, ftype.in, scale=scale.in)
  
  if(ipopt_result$status != 0){
    print(ipopt_result$message)
    print("\n")
    fname_pre <- paste0("ipopt_bad_", ftype.in, "_", ugroup.in)
    
    fname <- paste0(fname_pre, ".rds")
    saveRDS(ipopt_result, paste0("d:/temp/", fname))
    
    # tmp <- read_file("syntarget.out")
    # fname <- paste0(fname_pre, ".out")
    # write_file(tmp, paste0("d:/temp/", fname))
  }
  
  # df$wt1 <- ipopt_result$solution * df$wt0
  df <- df %>%
    select(ftype, RECID, RECID_original, MARS, ugroup, wt0) %>%
    mutate(wt1 = ipopt_result$solution * df$wt0)
  return(df)
}



#****************************************************************************************************
#                Fast function to get the sparsity structure of a matrix ####
#****************************************************************************************************

make.sparse.structure <- function(A) {
  # return a list with indexes for nonzero constraint coefficients
  # this is much faster than the make.sparse function in ipoptr
  f <- function(x) which(x!=0, arr.ind=TRUE)
  rownames(A) <- NULL # just to be safe
  S <- apply(A, 1, f)
  S <- as.list(S)
  return(S)
}


#****************************************************************************************************
#                constraint evaluation and coefficient functions for ipoptr -- same for all ####
#****************************************************************************************************
eval_g <- function(x, inputs) {
  # constraints that must hold in the solution - just give the LHS of the expression
  # return a vector where each element evaluates a constraint (i.e., sum of (x * a ccmat column), for each column)
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  
  # inputs$constraint.coefficients.sparse has the fixed constraint coefficients in sparse form in a dataframe that has:
  #   i -- the FEASIBLE constraint number
  #   cnum -- constraint number (among all constraints -- simply an identifier)
  #   j -- index into x (i.e., the variable number)
  #   nzcc
  
  constraints <- inputs$constraint.coefficients.sparse %>%
    group_by(i, cnum) %>%
    summarise(constraint.value=sum(nzcc * x[j]) )
  
  return(constraints$constraint.value)
}


eval_jac_g <- function(x, inputs){
  # the Jacobian is the matrix of first partial derivatives of constraints (these derivatives may be constants)
  # this function evaluates the Jacobian at point x
  
  # return: a vector where each element gives a NONZERO partial derivative of constraints wrt change in x
  # so that the first m items are the derivs with respect to each element of first column of ccmat
  # and next m items are derivs with respect to 2nd column of ccmat, and so on
  # so that it returns a vector with length=nrows x ncolumns in ccmat
  
  # because constraints in this problem are linear, the derivatives are all constants
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  
  return(inputs$constraint.coefficients.sparse$nzcc)
}


#****************************************************************************************************
#                x^p + x^-p {xtop} -- functions for ipoptr ####
#****************************************************************************************************
eval_f_xtop <- function(x, inputs) {
  # objective function - evaluates to a single number
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  # here are the objective function, the 1st deriv, and the 2nd deriv
  # http://www.derivative-calculator.net/
  # w{x^p + x^(-p) - 2}                                 objective function
  # w{px^(p-1) - px^(-p-1)}                             first deriv
  # p*w*x^(-p-2)*((p-1)*x^(2*p)+p+1)                    second deriv
  
  # make it easier to read:
  p <- inputs$p
  w <- inputs$wt
  
  obj <- sum(w * {x^p + x^(-p) -2})
  
  return(obj)
}


eval_grad_f_xtop <- function(x, inputs){
  # gradient of objective function - a vector length x 
  # giving the partial derivatives of obj wrt each x[i]
  
  # ipoptr requires that ALL functions receive the same arguments, so I pass the inputs list to ALL functions
  
  # http://www.derivative-calculator.net/
  # w{x^p + x^(-p) - 2}                                 objective function
  # w{px^(p-1) - px^(-p-1)}                             first deriv
  # p*w*x^(-p-2)*((p-1)*x^(2*p)+p+1)                    second deriv
  
  # make it easier to read:
  p <- inputs$p
  w <- inputs$wt
  
  gradf <- w * (p * x^(p-1) - p * x^(-p-1))
  
  return(gradf)
}


eval_h_xtop <- function(x, obj_factor, hessian_lambda, inputs){
  # The Hessian matrix has many zero elements and so we set it up as a sparse matrix
  # We only keep the (potentially) non-zero values that run along the diagonal.
  
  # http://www.derivative-calculator.net/
  # w{x^p + x^(-p) - 2}                                 objective function
  # w{px^(p-1) - px^(-p-1)}                             first deriv
  # p*w*x^(-p-2)*((p-1)*x^(2*p)+p+1)                    second deriv
  
  # make it easier to read:
  p <- inputs$p
  w <- inputs$wt
  
  hess <- obj_factor * 
    { p*w*x^(-p-2) * ((p-1)*x^(2*p)+p+1) }
  
  return(hess)
}



#****************************************************************************************************
#                (x - 1)^2 {xm1sq} -- functions for ipoptr ####
#****************************************************************************************************
eval_f_xm1sq <- function(x, inputs) {
  # objective function - evaluates to a single number
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  # here are the objective function, the 1st deriv, and the 2nd deriv
  # http://www.derivative-calculator.net/
  # w*(x-1)^2                  objective function
  # 2*w*(x-1)                  first deriv
  # 2*w                        second deriv
  
  # make it easier to read:
  w <- inputs$wt
  
  obj <- sum(w * (x-1)^2)
  
  return(obj)
}


eval_grad_f_xm1sq <- function(x, inputs){
  # gradient of objective function - a vector length x 
  # giving the partial derivatives of obj wrt each x[i]
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  
  # http://www.derivative-calculator.net/
  # w*(x-1)^2                  objective function
  # 2*w*(x-1)                  first deriv
  # 2*w                        second deriv
  
  # make it easier to read:
  w <- inputs$wt
  
  gradf <- 2 * w * (x-1)
  
  return(gradf)
}


eval_h_xm1sq <- function(x, obj_factor, hessian_lambda, inputs){
  # The Hessian matrix has many zero elements and so we set it up as a sparse matrix
  # We only keep the (potentially) non-zero values that run along the diagonal.
  
  # http://www.derivative-calculator.net/
  # w*(x-1)^2                  objective function
  # 2*w*(x-1)                  first deriv
  # 2*w                        second deriv
  
  # make it easier to read:
  w <- inputs$wt
  
  hess <- obj_factor * 2 * w
  
  return(hess)
}



#****************************************************************************************************
#                Scaling of the optimization problem ####
#****************************************************************************************************
# https://projects.coin-or.org/Ipopt/wiki/HintsAndTricks
# When you formulate your optimizaton problem, you should try to make it "well-scaled" to make it easier
# for Ipopt (or any nonlinear optimization package) to solve it. For this, you should try to make the
# "typical" values of the non-zero first partial derivatives of the objective and constraint functions 
# to be on the order of, say, 0.01 to 100. For example, if you multiply a problem function by a
# number K, then the first partial derivatives for this function are also multiplied by K. On the other
# hand, if you replace a variable xi by a number K, then the partial derivatives with respect to this
# variable are divided by K.

# By default, Ipopt performs some very simple scaling of the problem functions, by looking at the 
# gradients of each function evaluated at the user-provided starting point. If any entry for a given function 
# is larger than 100, then this function is scaled down to make the largest entry in the gradient 100
# (see the Ipopt options nlp_scaling_method and nlp_scaling_max_gradient). Of course, if some of the
# gradient elements are huge and some are very small, the variables corresponding to the small entries
# are almost ignored. If you set the print_level to at least 5, then you can see by how much the functions
# are scaled. For sufficiently large print_level you can also see the individual scaling factors for each
# constraint, and also the values of the first derivatives, if you want to find out which derivatives are
# very large or very small.

