
#****************************************************************************************************
#                general utility functions ####
#****************************************************************************************************
ht <- function(df, nrecs=6){
  print(utils::head(df, nrecs))
  print(utils::tail(df, nrecs))
}

naz <- function(vec) {return(ifelse(is.na(vec), 0, vec))} # NAs to zero

ns <- function(df) {names(df) %>% sort}

pdiff <- function(weights, data, constraints) {
  # percent difference between calculated constraints and targets
  calculated_constraints <- calc_constraints(weights, data, names(constraints))
  (calculated_constraints - constraints) / constraints * 100
}


#****************************************************************************************************
#                general functions ####
#****************************************************************************************************

parens <- function(stringvec){
  # put parentheses around each element of a string vector
  paste0("(", stringvec, ")")
}

move <- function(vec, element, after.element){
  # element is a unique element of vec
  # after.element also is a unique element of vec
  # move element into the position right after after.element
  vec2 <- setdiff(vec, element)
  iafter <- which(vec2==after.element)
  vec3 <- c(vec2[1:iafter], element)
  if(iafter < length(vec2)) vec3 <- append(vec3, vec2[(iafter+1):length(vec2)])
  return(vec3)
}


#****************************************************************************************************
#                file manipulation functions ####
#****************************************************************************************************
get_puf_vnames <- function(){
  # get data frame with PUF variables names and labels (vdesc)
  # readRDS("./data/puf.vnames.rds")
  readRDS(file.path("C:/Users/donbo/Dropbox/RPrograms PC/OSPC/syndata/data", "puf.vnames.rds")) %>% arrange(vname)
}


#****************************************************************************************************
#                prep file for Tax-Calculator functions ####
#****************************************************************************************************
change_case <- function(oldnames, upper_case_vars=c("MARS", "XTOT", "DSI", "EIC", "FDED", "MIDR", "RECID")){
  # convert variable names to lower case, except for those that should
  # remain upper case
  newnames <- oldnames
  lower_case_indexes <- !newnames %in% upper_case_vars
  lower_case_names <- newnames[lower_case_indexes] %>% str_to_lower
  newnames[lower_case_indexes] <-lower_case_names
  return(newnames)
}


impose_variable_rules <- function(df){
  # Per https://pslmodels.github.io/Tax-Calculator/, Tax-Calculator requires:
  #    ordinary dividends (e00600) >= qualified dividends (e00650)
  #    total pension and annuity income (e01500) >= taxable pension and annuity income (e01700)
  
  # A few of the early synthesized files did not enforce this so we enforce it here by
  # forcing the e00600 and e01500 to be at least as large as their counterparts
  # This does NO ERROR CHECKING or reporting to let the user know of a potential problem.
  # Once we no longer use those early files we should stop running this function.
  # This should not be needed for synpuf5 and later.
  
  if("e00650" %in% names(df) & "e00600" %in% names(df)){
    df <- df %>%
      mutate(e00600=pmax(e00650, e00600))
  }
  if("e01500" %in% names(df) & "e01700" %in% names(df)){
    df <- df %>%
      mutate(e01500=pmax(e01500, e01700))
  }
  return(df)
}


prime_spouse_splits <- function(df){
  # Per https://pslmodels.github.io/Tax-Calculator/, Tax-Calculator requires the filing-unit total 
  # for each of several earnings-related variables to be split between the taxpayer and the spouse:
  #   e00200 = e00200p + e00200s  # wages
  #   e00900 = e00900p + e00900s  # business income or loss
  #   e02100 = e02100p + e02100s  # Schedule F net profit/loss
  
  # For now, create arbitrary prime-spouse splits of these variables so that we can
  # run Tax-Calculator
  prime.pct <- ifelse(df$MARS==2, .5, 1)
  spouse.pct <- ifelse(df$MARS==2, .5, 0)
  df <- df %>%
    mutate(e00200p=e00200 * prime.pct,
           e00900p=e00900 * prime.pct,
           e02100p=e02100 * prime.pct,
           
           e00200s=e00200 * spouse.pct,
           e00900s=e00900 * spouse.pct,
           e02100s=e02100 * spouse.pct)
  return(df)
}


#****************************************************************************************************
#                correlation and statistical functions ####
#****************************************************************************************************
cordf_new <- function(df){
  # get correlations among vars in a df
  # put in long format with 3 columns:
  #   var1  - text variable name
  #   var2  - text variable name
  #   value - correlation between var1 and var2
  # return as a data frame
  cordf <- cor(df, use="pairwise.complete.obs") %>%
    as_tibble(rownames = "vname1") %>%
    gather(vname2, value, -vname1) %>%
    filter(vname1!=vname2) %>% # remove self correlations
    # identify first alphabetically of the lower and upper triangle correlation pairs and keep only that one
    mutate(firstname=pmin(vname1, vname2),
           secondname=pmax(vname1, vname2),
           combo=paste0(firstname, "_", secondname)) %>%
    group_by(combo) %>%
    arrange(firstname) %>%
    filter(vname1==firstname)
  return(cordf)
}


ks.p <- function(ftype, value) {
  # return the Kolmogorov-Smirnov test p.value
  # null hypothesis that x and y were drawn from the same continuous distribution is performed.
  # reject null when D is large
  # the larger the test statistic D is, the greater the distance between the cdfs of the distributions for
  # x and y (puf and syn)
  # we want D to be small, and p to be high
  # https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test#Two-sample_Kolmogorov.E2.80.93Smirnov_test
  # http://www.aip.de/groups/soe/local/numres/bookcpdf/c14-3.pdf
  df <- tibble(ftype, value)
  x <- df %>% filter(ftype=="puf") %>% .[["value"]]
  y <- df %>% filter(ftype=="syn") %>% .[["value"]]
  p.value <- ks.test(x, y)$p.value
  return(p.value)
}

ks.D <- function(ftype, value) {
  # return the Kolmogorov-Smirnov test statistic D
  # null hypothesis that x and y were drawn from the same continuous distribution is performed.
  # reject null when D is large
  # the larger the D, the greater the distance between the cdfs of the distributions for
  # x and y (puf and syn)
  # we want D to be small, and p to be high
  # https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test#Two-sample_Kolmogorov.E2.80.93Smirnov_test
  # http://www.aip.de/groups/soe/local/numres/bookcpdf/c14-3.pdf
  df <- tibble(ftype, value)
  x <- df %>% filter(ftype=="puf") %>% .[["value"]]
  y <- df %>% filter(ftype=="syn") %>% .[["value"]]
  D <- ks.test(x, y)$statistic
  return(D)
}

mse <- function(y, yhat){
  sqerr <- (yhat - y)^2
  return(mean(sqerr))
}

rmse <- function(y, yhat){
  rmse <- sqrt(mse(y, yhat))
  return(rmse)
}


rsq <- function (x, y) {cor(x, y) ^ 2}

sse <- function(var1, var2) {
  error <- var1 - var2
  sqrt(sum(error^2))
}


utility <- function(mod){
  # mod is a glm logistic model
  # utility is a [0, 1] measure of the utility of the data, where 1=optimal
  
  pprobs <- mod$fitted.values # predicted probabilities
  # get mse of predicted probabilities -- 
  #   mse 0 would be optimal -- we can't predict whether record is syn -- they are all 0.5
  #   mse worst is 0.25
  pmse <- mse(rep(.5, length(pprobs)), pprobs)
  utility <- 1 - (pmse / .25)
  return(utility)
}


#****************************************************************************************************
#                graphing and plotting functions ####
#****************************************************************************************************


cdfplot2 <- function(var, stackdf=stack, vnames=puf.vnames){
  # print(var)
  vdesc <- vnames$vdesc[match(var, vnames$vname)]
  df <- stackdf %>%
    dplyr::select(ftype, value=var) %>%
    group_by(ftype) %>%
    arrange(value) %>%
    mutate(cum.pct=cumsum(value) / sum(value)) %>%
    ungroup
  
  if(nrow(df) < 100){
    p <- paste0(var, " has only ", nrow(df), " rows.")
    return(p)
  }
  
  # create a smaller version of the file because plots with the full file take a long time
  df_saved <- df
  # glimpse(df_saved)
  df <- df_saved %>%
    mutate(cum.pct=round(cum.pct, 5)) %>%
    group_by(ftype, cum.pct) %>%
    summarise(value=mean(value)) %>% 
    ungroup
  
  # find a good minimum x-axis value to start the plot on -- based on a desired cumulative percentage
  cum.pct.threshold <- .01
  iminval <- min(which(df$cum.pct[df$ftype=="puf"] > cum.pct.threshold))
  minval <- df$value[df$ftype=="puf"][iminval]
  # minval
  
  capt <- "- x-axis is log10 scale\n- For display purposes x-axis is truncated at left to start at puf's cum.pct=1%"
  gtitle <- paste0("Cumulative distribution of unweighted ", var, ": ", vdesc)
  gsubtitle <- "Aggregate records excluded"
  ylab <- paste0("Cumulative proportion of the sum of unweighted ", var)
  
  # define x scale break points and associated labels
  sq10 <- c(0, 1e3, 10e3, 25e3, 50e3, 100e3, 250e3, 500e3, 750e3, 1e6,
            1.5e6, 2e6, 3e6, 4e6, 5e6, 10e6, 25e6, 50e6, 100e6)
  xlabs <- scales::comma(sq10 / 1e3)
  xscale.l10 <- scale_x_log10(name=paste0(var, " in $ thousands"), breaks=sq10, labels=xlabs)
  
  p <- df %>%
    filter(value > minval) %>%
    mutate(ftype=factor(ftype, levels=c("puf", "cart", "rf"))) %>%
    ggplot(aes(value, cum.pct, colour=ftype)) + 
    geom_line(size=1.5)
  
  p2 <- p +
    scale_colour_manual(values=c("blue", "red", "darkgreen")) +
    theme_bw() +
    ggtitle(gtitle, subtitle=gsubtitle) +  labs(caption=capt) +
    scale_y_continuous(name=ylab, breaks=c(seq(0, .9, .05), seq(.92, 1, .02))) +
    xscale.l10 +
    theme(axis.text.x=element_text(angle=45, size=10, hjust=1, colour="black")) +
    theme(plot.caption = element_text(hjust=0, size=rel(.8)))
  
  print(p2)
  return(p2)
}
# system.time(cdfplot2(incvars[1]))


# var <- "e00200"
cdfplot.unwtd <- function(var, stackdf=stack, vnames=puf_vnames){
  # print(var)
  vdesc <- vnames$vdesc[match(var, vnames$vname)]
  df <- stackdf %>%
    dplyr::select(ftype, value=var) %>%
    group_by(ftype) %>%
    arrange(value) %>%
    mutate(cum.pct=cumsum(value) / sum(value))
  
  if(nrow(df) < 100){
    p <- paste0(var, " has only ", nrow(df), " rows.")
    return(p)
  }
  
  # find a good minimum x-axis value to start the plot on -- based on a desired cumulative percentage
  cum.pct.threshold <- .01
  iminval <- min(which(df$cum.pct[df$ftype=="puf"] > cum.pct.threshold))
  minval <- df$value[df$ftype=="puf"][iminval]
  # minval
  
  capt <- "- x-axis is log10 scale\n- For display purposes x-axis is truncated at left to start at puf's cum.pct=1%"
  gtitle <- paste0("Cumulative distribution of unweighted ", var, ": ", vdesc)
  gsub <- "Aggregate records excluded"
  ylab <- paste0("Cumulative proportion of the sum of unweighted ", var)
  
  # define x scale break points and associated labels
  sq10 <- c(0, 1e3, 10e3, 25e3, 50e3, 100e3, 250e3, 500e3, 750e3, 1e6,
            1.5e6, 2e6, 3e6, 4e6, 5e6, 10e6, 25e6, 50e6, 100e6)
  xlabs <- scales::comma(sq10 / 1e3)
  xscale.l10 <- scale_x_log10(name=paste0(var, " in $ thousands"), breaks=sq10, labels=xlabs)
  
  p <- df %>%
    filter(value > minval) %>%
    ggplot(aes(value, cum.pct, colour=ftype)) + 
    geom_line(size=1.5) +
    theme_bw() +
    ggtitle(gtitle, subtitle=gsub) +  labs(caption=capt) +
    scale_y_continuous(name=ylab, breaks=c(seq(0, .9, .05), seq(.92, 1, .02))) +
    xscale.l10 +
    theme(axis.text.x=element_text(angle=45, size=10, hjust=1, colour="black")) +
    theme(plot.caption = element_text(hjust=0, size=rel(.8)))
  return(p)
}


cdfplot <- function(var){
  # print(var)
  vdesc <- synvars$vdesc[match(var, synvars$vname)]
  df <- stack %>%
    dplyr::select(ftype, wt, value=var) %>%
    group_by(ftype) %>%
    arrange(value) %>%
    mutate(cum.pct=cumsum(wt * value) / sum(wt * value))
  
  if(nrow(df) < 100){
    p <- paste0(var, " has only ", nrow(df), " rows.")
    return(p)
  }
  
  # find a good minimum x-axis value to start the plot on -- based on a desired cumulative percentage
  cum.pct.threshold <- .01
  iminval <- min(which(df$cum.pct[df$ftype=="puf.full"] > cum.pct.threshold))
  minval <- df$value[df$ftype=="puf.full"][iminval]
  # minval
  
  capt <- "- x-axis is log10 scale\n- For display purposes x-axis is truncated at left to start at puf.full's cum.pct=1%"
  gtitle <- paste0("Cumulative distribution of weighted ", var, ": ", vdesc)
  gsub <- "Aggregate records excluded"
  ylab <- paste0("Cumulative proportion of the sum of weighted ", var)
  
  # define x scale break points and associated labels
  sq10 <- c(0, 1e3, 10e3, 25e3, 50e3, 100e3, 250e3, 500e3, 750e3, 1e6,
            1.5e6, 2e6, 3e6, 4e6, 5e6, 10e6, 25e6, 50e6, 100e6)
  xlabs <- scales::comma(sq10 / 1e3)
  xscale.l10 <- scale_x_log10(name=paste0(var, " in $ thousands"), breaks=sq10, labels=xlabs)
  
  p <- df %>%
    filter(value > minval) %>%
    ggplot(aes(value, cum.pct, colour=ftype)) + 
    geom_line(size=1.5) +
    theme_bw() +
    ggtitle(gtitle, subtitle=gsub) +  labs(caption=capt) +
    scale_y_continuous(name=ylab, breaks=c(seq(0, .9, .05), seq(.92, 1, .02))) +
    xscale.l10 +
    theme(axis.text.x=element_text(angle=45, size=10, hjust=1, colour="black")) +
    theme(plot.caption = element_text(hjust=0, size=rel(.8)))
  return(p)
}



