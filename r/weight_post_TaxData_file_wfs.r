# Goal: Prepare new weights for the taxdata-enhanced no-disclosures synthetic file


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

# library("readxl") # readxl, for .xls and .xlsx files
# library("haven") # haven, for SPSS, SAS and Stata files
# library("vctrs")
library("knitr")

library("ipoptr")

# devtools::install_github("donboyd5/btools")
library("btools") # library that I created (install from github)


#****************************************************************************************************
#                functions ####
#****************************************************************************************************
source("./r/functions_weighting.r")


#****************************************************************************************************
#                globals ####
#****************************************************************************************************
#.. the no disclosure files, enhanced ----
epuf_path <- "C:/Users/donbo/Dropbox/SLGF/taxdata_synpuf/puf_rottenpuf.csv"
esyn_path <- "C:/Users/donbo/Dropbox/SLGF/taxdata_synpuf/puf_synpuf.csv"


#****************************************************************************************************
#                get and stack data ####
#****************************************************************************************************
epuf <- read_csv(epuf_path, col_types = cols(.default= col_double()), n_max=-1)
esyn <- read_csv(esyn_path, col_types = cols(.default= col_double()), n_max=-1)

setdiff(names(epuf), names(esyn))
setdiff(names(esyn), names(epuf))

common_names <- intersect(names(epuf), names(esyn))
common_names %>% sort

estack <- bind_rows(epuf %>% select(common_names) %>% mutate(ftype="puf"),
                    esyn %>% select(common_names) %>% mutate(ftype="syn")) %>%
  select(-FLPDYR) %>%
  arrange(ftype, RECID) %>%
  mutate(RECID_original=RECID,
         RECID=row_number()) %>%
  # first fix the e00200 variables because 541 of these records on the synfile do not sum properly
  # within Tax-Calculator tolerances although most are extremely close so choose a simple fix
  mutate(e00200s=e00200 - e00200p) %>%
  # now fix the truly rotten variables
  mutate(e00900p=case_when(MARS==2 & e00900!=0 & e00200!=0 ~ e00200p / e00200 * e00900,
                           MARS==2 & e00900!=0 & e00200==0 ~ e00900 * .5,
                           MARS!=2 ~ e00900,
                           TRUE ~ e00900p),
         e00900s=e00900 - e00900p,
         e02100p=case_when(MARS==2 & e02100!=0 & e00200!=0 ~ e00200p / e00200 * e02100,
                           MARS==2 & e02100!=0 & e00200==0 ~ e02100 * .5,
                           MARS!=2 ~ e02100,
                           TRUE ~ e02100p),
         e02100s=e02100 - e02100p)
glimpse(estack)
count(estack, ftype)
estack %>%
  group_by(ftype) %>%
  summarise(n=n(), wtdn=sum(s006 / 100), minrec=min(RECID), maxrec=max(RECID))


#****************************************************************************************************
#                run it through 2013 tax-calculator to get agi and other key variables that we want to use in targeting ####
#****************************************************************************************************
djb <- "D:/tax_data/djb_presentation/"
epath <- paste0(djb, "estack.csv")
estack %>% write_csv(paste0(djb, "estack.csv"))

cmd1 <- "C:/ProgramData/Anaconda3/Scripts/tc"
args <- c(shQuote(epath), "2013",
          "--dump",
          "--outdir", djb)
cmd1
args

# test <- read_csv("C:/Windows/System32/test.csv")
# names(test)
# anyDuplicated(esr$RECID)
# ns(esr)

#.. run the command ----
a <- proc.time()
system2(cmd1, args) # CAUTION: this will overwrite any existing output file that was based on the same input filename!
b <- proc.time()
b - a  # it can easily take 5-10 minutes depending on the size of the input file


#****************************************************************************************************
#                merge in tax-calculator results and SAVE ####
#****************************************************************************************************
(tc_path <- paste0(djb, "estack-13-#-#-#.csv"))
tcvars <- c("RECID", "c00100", "c62100", "taxbc", "c09600", "c05800")

df2 <- read_csv(tc_path, col_types = cols(.default= col_double()), n_max=-1)
ns(df1)
ns(df2)

estack_tc <- estack %>%
  left_join(df2 %>% select(tcvars), by="RECID")
glimpse(estack_tc)

estack_tc %>% saveRDS(paste0(djb, "estack_tc.rds"))


#****************************************************************************************************
#                prepare to reweight ####
#****************************************************************************************************
estack_tc <- readRDS(paste0(djb, "estack_tc.rds"))

#.. 1. Define subsets that have approximately 500-1000 records ----
puf_from_tc <- estack_tc %>%
  filter(ftype=="puf")
count(puf_from_tc, MARS)
quantile(puf_from_tc$c00100)

ntarget <- 2000 # number of records we'd like in each group
nmin <- 1500 # minimum number we'd like
count(ngroups, aginzp, MARS)
count(ngroups, agige0, MARS)
sum(ngroups$n_final)
nrow(puf_from_tc)
ngroups %>%
  arrange(n_final)

ngroups <- puf_from_tc %>%
  mutate(agige0=ifelse(c00100 >= 0, 1, 0)) %>% # distinguish the negative agi recs from the >=0 agi recs
  mutate(agige0=case_when(c00100 < 0 ~ -9,
                          c00100 == 0 ~ 0,
                          c00100 > 0 ~ 9)) %>% # distinguish the negative agi recs from the >=0 agi recs
  select(agige0, MARS, c00100) %>%
  arrange(MARS, agige0, c00100) %>%
  group_by(MARS, agige0) %>%
  # within each agi group (negative, nonneg) and MARS group get # of recs / relative to target number
  mutate(# ngroups is # of groups we can make in this agege0-MARS combo that have target # of records (must have at least 1 group)
         ngroups=pmax(round(n() / ntarget), 1), 
         grp=ntile(n=ngroups[1])) %>% # assign each record in this agege0-MARS combo to one of the groups
  group_by(MARS, agige0, grp) %>%
  # get the agi cuts for each group
  summarise(n=n(), # number of records in each group
            agimin=min(c00100), 
            agimax=max(c00100)) %>%
  # collapse within MARS agige0 group if identical agimin agimax and then renumber groups
  group_by(MARS, agige0, agimin, agimax) %>%
  summarise(n_final=sum(n)) %>%
  group_by(MARS, agige0) %>%
  mutate(grp=row_number(), ngrps=max(grp)) %>% # ensure that grp numbers start at 1 and rise sequentially
  ungroup
ngroups %>% print(n = Inf) # 128 groups
quantile(ngroups$n_final) # smallest has only 53 records -- MARS 3, agi==0
# 0%  25%  50%  75% 100% 
# 53 1986 1986 2018 3748 
ngroups$n_final %>% sort

# put min max and lower upper agi ranges on the groups -- we want agilow_ge and agihigh_lt
group_agibreaks <- ngroups %>%
  arrange(MARS, agige0, agimin, agimax) %>%
  group_by(MARS, agige0) %>%
  mutate(agimax_prior=lag(agimax),
         agimin_next=lead(agimin),
         agilow_ge=case_when((agige0==-9) & (row_number()==1) ~ -Inf,
                             (agige0==0) & (row_number()==1) ~ 0,
                             (agige0==9) & (row_number()==1) ~ 0,
                             TRUE ~ NA_real_),
         agihigh_lt=case_when((agige0==-9) & (row_number()==n()) ~ 0,
                              (agige0==0) & (row_number()==n()) ~ 0,
                              (agige0==9) & (row_number()==n()) ~ Inf,
                              TRUE ~ NA_real_)) %>%
  mutate(agilow_ge=ifelse(is.na(agilow_ge), (agimin + agimax_prior) / 2, agilow_ge),
         agihigh_lt=ifelse(is.na(agihigh_lt), (agimax + agimin_next) / 2, agihigh_lt)) %>%
  ungroup %>%
  arrange(MARS, agilow_ge) %>%
  mutate(ugroup=row_number()) %>% # universal group (group within the entire universe)
  select(-agimin_next, -agimax_prior) %>%
  select(MARS, agige0, grp, ngrps, ugroup, everything())
# 128 groups

sum(group_agibreaks$n_final)

# checks:
group_agibreaks %>% 
  print(n = Inf) %>% 
  kable(digits=0, format.args = list(big.mark=","), format="rst")

# look at first and last rec in each agige0, MARS grouping
group_agibreaks %>% 
  filter(grp==1) %>% 
  kable(digits=0, format.args = list(big.mark=","), format="rst")

group_agibreaks %>% 
  filter(grp==ngrps) %>% 
  kable(digits=0, format.args = list(big.mark=","), format="rst")

group_agibreaks %>%
  group_by(MARS) %>%
  summarise(ngrps=n(), nsum=sum(n_final), nmin=min(n_final), nmax=max(n_final)) %>%
  mutate(totgrps=sum(ngrps), totn=sum(nsum))

#.. loop through the data and put ugroup on each record ----
count(estack_tc, ftype)

getgroup <- function(agivec, MARSval){
  # MARSval <- 1
  # agivec <- c(-Inf, -1000, 0, 1000, 20e3, Inf)
  
  gindex <- function(agival, agilow_ge_vec) {
    ifelse(agival < Inf, 
           min(which(agival < agilow_ge_vec)) - 1,
           length(agilow_ge_vec) - 1)
  }
  
  breaks <- group_agibreaks %>%
    filter(MARS==MARSval) %>%
    arrange(agilow_ge) %>%
    select(MARS, ugroup, agilow_ge, agihigh_lt)
  
  agilow_ge_vec <- c(breaks %>% .[["agilow_ge"]], Inf)
  
  indexes <- gindex(1.5e6, agilow_ge_vec)
  indexes <- sapply(agivec, gindex, agilow_ge_vec)
  ugroup <- breaks$ugroup[indexes]
  return(ugroup)
}

estack_tc %>%
  filter(c00100==0)

a <- proc.time()
estack_tc_groups <- estack_tc %>%
  group_by(MARS) %>%
  mutate(ugroup=getgroup(c00100, first(MARS)))
b <- proc.time()
b - a # 18 secs
glimpse(estack_tc_groups)

estack_tc_groups %>%
  group_by(ftype, ugroup) %>%
  summarise(n=n()) %>%
  group_by(ftype) %>%
  summarise(nsum=sum(n), ngroups=n(), nmin=min(n), nmedian=median(n), nmax=max(n))
# ftype   nsum ngroups  nmin nmedian  nmax
# <chr>  <int>   <int> <int>   <dbl> <int>
# 1 puf   248591     124   182   1990.  5767
# 2 syn   762749     124   836   5337  10309

#..2. Now that groups are defined, get constraints for each group ----
# define constraint variables
# pcatvars <- c("XTOT", "DSI", "EIC", "FDED", "MIDR", "n24", "f6251", "f2441") # don't include MARS as it is used for grouping
# (continuous_vars <- setdiff(change_case(syn_info$vname), c(pcatvars, "MARS")))


puf_vnames <- get_puf_vnames() %>% select(vname, vdesc)
tc_vnames <- tribble(
  ~vname, ~vdesc,
  "c00100", "Adjusted gross income (calculated)",
  "c17000", "Sch A: Medical expenses deducted (calculated)",
  "c18300", "Sch A: State and local taxes deducted (calculated)",
  "c21060", "Itemized deductions before phase-out (zero for non-itemizers) (calculated)",
  "standard", "standard Standard deduction (zero for itemizers) (calculated)",
  "c04800", "Regular taxable income (calculated)",
  "taxbc", "regular tax before credits (calculated)",
  "c62100", "Alternative Minimum Taxable Income (AMTI) (calculated)",
  "c09600", "Alternative Minimum Tax (AMT) liability (calculated)",
  "c05800", "taxbc plus AMT liability (calculated)"
)
var_vnames <- bind_rows(puf_vnames, tc_vnames)


#....2a) Define constraint variables
pvars <- c("p08000", "p22250", "p23250")
evars <- setdiff(names(estack_tc_groups)[str_sub(names(estack_tc_groups), 1, 1)=="e"], "elderly_dependents")
(epvars <- c(evars, pvars))

tcvars <- c("c00100", "taxbc", "c09600", "c05800", "c62100") # the subset of tax-calculator variables that we want to target
cbasevars <- c(tcvars, epvars)
cbasevars

#....2b) get constraint coefficients ----
ns(estack_tc_groups)
count(estack_tc_groups, ugroup)
ccoef <- estack_tc_groups %>%
  ungroup %>%
  mutate(wt0=s006 / 100) %>%
  select(ftype, RECID, RECID_original, MARS, ugroup, wt0, cbasevars) %>%
  mutate_at(vars(cbasevars), list(npos = ~npos(., wt0),
                                  nz = ~nneg(., wt0),
                                  sumpos = ~sumpos(., wt0),
                                  sumneg = ~sumneg(., wt0)))
glimpse(ccoef)


# create a vector of constraint variable names
all_constraint_vars <- ccoef %>%
  select(contains("_n"), contains("_sum")) %>%
  names(.) # length 260
all_constraint_vars %>% sort

# get the target values for each group of the data
a <- proc.time()
all_constraint_vals <- ccoef %>%
  # filter(ugroup %in% 1:2) %>%
  group_by(ftype, ugroup) %>%  
  do(calc_constraints(.$wt0, ., all_constraint_vars) %>% 
       enframe %>%
       spread(name, value)) %>%
  ungroup
b <- proc.time()
b - a # about a min
glimpse(all_constraint_vals) # 266 constraints plus ftype ugroup; 248 obs (2 ftypes x 124 groups)
count(all_constraint_vals, ftype)


# drop duplicate constraints that have identical concoefs (keep the first)
getgoodcols <- function(data){
  # get names of non-duplicated constraint coefficients
  dupcols <- which(duplicated(as.matrix(data), MARGIN = 2))
  df <- tibble(good_constraint=setdiff(names(data), names(dupcols)))
  return(df)
}

a <- proc.time()
good_con <- ccoef %>%
  filter(ftype != "puf") %>%
  group_by(ftype, ugroup) %>%
  do(getgoodcols(.[, all_constraint_vars])) %>%
  ungroup
b <- proc.time()
b - a # only 13 secs
glimpse(good_con)
length(unique(good_con$good_constraint)) # only 160 good constraints
count(good_con, ftype, ugroup)

# get sums within groups of constraint coefficients to help with setting tolerances
# str_extract(all_constraint_vars, "[^_]+")
ccsums <- all_constraint_vals %>%
  gather(constraint_var, file_value, -ftype, -ugroup) %>%
  group_by(ugroup, constraint_var) %>%
  mutate(target=file_value[ftype=="puf"],
         diff=file_value - target,
         pdiff=diff / target * 100) %>%
  left_join(group_agibreaks %>% select(ugroup, MARS, agilow_ge, agihigh_lt)) %>%
  select(ugroup, MARS, constraint_var, agilow_ge, agihigh_lt, ftype, target, file_value, diff, pdiff) %>%
  mutate(vdesc=var_vnames$vdesc[match(str_extract(constraint_var, "[^_]+"), var_vnames$vname)]) %>%
  ungroup
glimpse(ccsums)
count(ccsums, ftype)
ccsums %>% filter(ugroup==3, ftype=="syn")
# ccsums <- pdiffs %>%
#   group_by(ugroup, variable) %>%
#   summarise_at(vars(puf, syn, syn_nd, syn_diff, syn_nd_diff), ~sum(.))


# combine them
# targets <- ccsums %>% 
#   rename(good_constraint=constraint_var) %>%
#   right_join(good_con, by=c("ftype", "ugroup", "good_constraint"))
# targets
# targets %>% filter(ftype=="syn", ugroup==3)
# targets %>% filter(ftype=="syn", target==0, file_value!=0) # 1432 in the entire file
# targets %>% filter(ftype=="syn", target==!0, file_value==0) # none in the file



#.3. Prepare the constraint bounds ----
# e00400 is #20 on the list
# e09800 e58990 e03400 e07240 p08000 e07600 e24518
# create priority groupings and assign tolerances
# unique(str_extract(all_constraint_vars, "[^_]+"))
# tcvars # not in the group above
# tolerances <- tibble(vname=cbasevars) %>%
#   mutate(tol=case_when(vname %in% tcvars ~ .05,
#                        TRUE ~ Inf))
# tolerances
# tolerances %>% filter(vname %in% vars)
# cbasevars

# use the tolerances to put bounds around the targets
# bounds <- targets2 %>%
#   mutate(vname=str_extract(good_constraint, "[^_]+")) %>%
#   left_join(tolerances %>% select(vname, tol)) %>%
#   mutate(clb=target - tol * abs(target),
#          cub=target + tol * abs(target)) %>%
#   # ensure that logical inconsistencies cannot occur
#   # .. counts cannot be negative, and sumpos cannot be negative
#   mutate(clb=ifelse(str_detect(good_constraint, "_n") & (clb < 0), 0, clb),
#          cub=ifelse(str_detect(good_constraint, "_n") & (cub < 0), 0, cub),
#          clb=ifelse(str_detect(good_constraint, "_sumpos") & (clb < 0), 0, clb),
#          cub=ifelse(str_detect(good_constraint, "_sumneg") & (cub > 0), 0, cub)) %>%
#   select(-vdesc, everything(), vdesc)
# bounds
# bounds %>% filter(good_constraint=="taxbc_npos", ftype=="syn_nd", ugroup==7)
# bounds %>% filter(ftype=="syn_nd", ugroup==7) %>% print(n = Inf)
# count(bounds, ftype)

# bounds2 <- bounds %>%
#   filter(!())

# ONETIME (every once in a while) - save and/or load prep items ----
# save.image(file = "d:/temp/prep.RData")
# RELOAD load(file = "d:/temp/prep.RData") ----
# end ONETIME save ----


#.4. Run the optimization ----
# parallel version
library("multidplyr")
cluster <- new_cluster(6)
# parallel::stopCluster(cluster)

# conditional execution in a pipeline - for parallel or not
# https://community.rstudio.com/t/conditional-pipelines/6076/2
# mtcars %>% 
#   {if (FALSE) filter(., hp == 245) else .} %>% 
#   {if (FALSE) select(., cyl) else .} %>%
#   head(10)

# choose one of the following
parallel <- TRUE
# parallel <- FALSE

# djb ----
a <- proc.time()
if(parallel){
  # set the latest versions of functions, etc. up for the run
  cluster_copy(cluster, c("getwts_ipopt", "ipopt_reweight",
                          "define_jac_g_structure_dense", "eval_f_xtop", "eval_grad_f_xtop", "eval_g_dense",
                          "eval_jac_g_dense", "eval_h_xtop",
                          "calc_constraints",
                          "bounds"))
  cluster_library(cluster, c("dplyr", "tidyr", "ipoptr", "readr"))
}
opt_pre <- ccoef %>%
  filter(ftype=="syn") %>%
  filter(ugroup %in% 83) %>%
  group_by(ugroup) %>%
  {if (parallel) partition(., cluster) else .}

opt <- opt_pre %>%
  do(getwts_ipopt(., all_constraint_vals, bounds, scale=FALSE)) %>%
  {if (parallel) collect(.) else .} %>%
  ungroup
b <- proc.time()
b - a # seconds
(b - a) / 60 # minutes

# glimpse(opt)
# count(opt, ugroup)
# ccoef %>% group_by(ftype) %>% summarise(rmin=min(RECID), rmax=max(RECID))
# ffw2 %>% group_by(ftype) %>% summarise(n=n(), rmin=min(RECID), rmax=max(RECID), romin=min(RECID_original), romax=max(RECID_original))

fname <- paste0("d:/temp/opt_", ftype_opt, ".rds")
saveRDS(opt, fname)



# WFS ----
count(ccsums, constraint_var)
targets <- ccsums %>% 
  rename(good_constraint=constraint_var) %>%
  right_join(good_con %>% separate(col=good_constraint, into=c("vname", "fn"), remove=FALSE),
             by=c("ftype", "ugroup", "good_constraint"))
count(targets, ftype)

targets_use <- targets %>%
  filter(!(target==0 & file_value==0)) %>%
  filter(file_value!=0)

targets_use %>% filter(ugroup==1) # 129 targets
# we have 118 to 154 targets per group
targets_use %>%
  group_by(ugroup) %>%
  summarise(n=n()) %>%
  ungroup %>%
  summarise(ntargets=sum(n), ngroups=n(), n_min=min(n), n_mdn=median(n), n_max=max(n))
# ntargets ngroups n_min n_mdn n_max
# <int>   <int> <int> <dbl> <int>
#   1    17814     124   122  142.   158

count(targets_use, vname)

# test it out ----
grp <- 6 # 128 targets
targ_samp <- targets_use %>% filter(ugroup==grp)
syn_samp <- estack_tc_groups %>% filter(ftype=="syn", ugroup==grp) %>% ungroup
glimpse(syn_samp)
targ_samp %>% filter(str_detect(good_constraint, "c00100"))
targ_samp %>% filter(str_detect(good_constraint, "taxbc"))
# syn_samp %>% summarise(taxbc_sumpos=sum((taxbc>0) * taxbc * s006 / 100))

targ_samp %>% filter(str_detect(good_constraint, "e00400"))
syn_samp %>% summarise(taxbc_sumpos=sum((e00400>0) * e00400 * s006 / 100))
count(targ_samp, vname)
count(targ_samp, fn)

priority1_vars <- c("wt", "c00100", "e00200", "e00300", "e00400", "e01500", "p23250", "taxbc", "c62100", "c09600")
targ_samp_final <- targ_samp %>%
  mutate(priority_weight=case_when(vname %in% priority1_vars ~ 100,
                                   TRUE ~ 1))
ccoef_samp <- ccoef %>%
  filter(ftype=="syn", ugroup==grp)

tnames <- intersect(targ_samp$good_constraint, names(ccoef_samp))
tvec <- targ_samp_final$target
names(tvec) <- targ_samp_final$good_constraint
tvec <- tvec[tnames]

pweights <- targ_samp_final$priority_weight
names(pweights) <- targ_samp_final$good_constraint
pweights <- pweights[tnames]

convec <- calc_constraints(wts, ccoef_samp, tnames)

tscale <- ifelse(tvec==0, convec, tvec)
any(tscale==0)

wts <- syn_samp$s006 / 100

diff <- convec / tscale - tvec / tscale
diff2 <- diff^2
obj <- sum(diff2 * pweights)

# prepare for optim ----
get_named_vec <- function(vec, vec_names, target_names){
  names(vec) <- vec_names
  return(vec[target_names])
}

target_names <- intersect(targ_samp$good_constraint, names(ccoef_samp))
target_vec <- get_named_vec(vec = targ_samp_final$target,
                            vec_names=targ_samp_final$good_constraint,
                            target_names=target_names)
priority_weights <- get_named_vec(vec = targ_samp_final$priority_weight,
                                  vec_names = targ_samp_final$good_constraint,
                                  target_names = target_names)
fileval_vec <- get_named_vec(vec = targ_samp_final$file_value,
                             vec_names = targ_samp_final$good_constraint,
                             target_names = target_names)
scale_vec <- ifelse(target_vec==0, fileval_vec, target_vec)

inputs <- list()
inputs$target_names <- target_names
inputs$target_vec <- target_vec
inputs$priority_weights <- priority_weights
inputs$scale_vec <- scale_vec
inputs$ccoef <- ccoef_samp

ccmat[c(1:10, 100:110), c(1:10, 100:110)]
nrow(ccmat)
dim(ccmat)
length(wts)
x0 <- pmax(wts, 1)


ccmat <- as.matrix(inputs$ccoef[, inputs$target_names])
calc_vec <- colSums(ccmat * x0) # 1 sum per constraint
grad_vec_part <- {2 * inputs$priority_weights * (calc_vec - inputs$target_vec)} / {inputs$scale_vec^2}
grad_vec <- (ccmat %*% grad_vec_part)[, 1]
grad_vec[1:10]

djb3 <- grad_veci * ccmat
djb3 <- ccmat %*% grad_veci
djb3[1:10, 1]

djb3 <- grad_veci %*% t(ccmat)
dim(djb3)
colSums(djb3)
djb3[1, 1:10]

# 6.506512e-05 6.520154e-05 1.837564e-06

grad_vec <- rowSums(grad_mat)
grad_vec[1:10]
sum(grad_mat)
# gma <- 2 * ccmat * inputs$priority_weights
# gmb <- (calc_vec - inputs$target_vec) / {inputs$scale_vec^2}
# gmc <- gma * gmb
# gv <- rowSums(gmc)
# gv[1:10]

grad_vec <- rowSums(grad_mat)
grad_vec[1:10]
# grad={2 * coeff * priority.weight * (calc - target)} / {scale^2}

o1 <- obj_wfs(x0, inputs)
i <- 10
x1 <- x0; x1[i] <- x1[i] + 1
o2 <- obj_wfs(x1, inputs)
o2 - o1

# for a single constraint t we have
# p((w1*a1 + w2*a2 + w3*a3)/s - t/s)^2
# using symbols that work
# p((x*a + y*b + z*c)/s - t/s)^2
# g(x) = n / d
#   n <- 2ap(ax + yb + cz -t)
#   d <- s^2

# g(w1) 2*a1*p*(w1*a1 + w2*a2 + w3*a3 - t) = 2*a1*p*(calc - t)
# g(w2) 2*a2*p*(calc - t)
# g(x) p2(xa + yb +zc - t)a

m <- matrix(1:12, nrow=4)
v <- c(2, 4, 6, 8)
m * v
rowSums(m * v)

obj_wfs <- function(w, inputs){
  constraints_vec <- calc_constraints(w, inputs$ccoef, inputs$target_names)
  diff <- constraints_vec / inputs$scale_vec - inputs$target_vec / inputs$scale_vec
  obj <- sum(diff^2 * inputs$priority_weights)
  return(obj)
}

obj_wfs(wts, inputs)
# 628.3316

grad_wfs <- function(w, inputs){
  # gradient of objective function - a vector length w
  # giving the partial derivatives of obj wrt each w[i]
  ccmat <- as.matrix(inputs$ccoef[, inputs$target_names])
  calc_vec <- colSums(ccmat * w) # 1 sum per constraint
  grad_vec_part <- {2 * inputs$priority_weights * (calc_vec - inputs$target_vec)} / {inputs$scale_vec^2}
  grad_vec <- (ccmat %*% grad_vec_part)[, 1]
  return(grad_vec)
}

grad_wfs(wts, inputs)
grad_wfs(x0, inputs)[1:10] %>% t
grad_vec[1:10]
# starting point and bounds on the weights
xlb <- rep(1, length(wts))
x0 <- pmax(wts, 1)
xub <- pmax(wts*2, 10e3)

cbind(xlb, x0, xub)

which(xlb>x0)
which(xub<x0)


opts <- list("algorithm"="NLOPT_LD_MMA",
             "xtol_rel"=1.0e-8,
             "maxeval"=500)
result <- nloptr(x0, 
                 eval_f=obj_wfs,
                 eval_grad_f = grad_wfs,
                 lb = xlb, ub = xub,
                 opts = opts, inputs=inputs)
str(result)

# tmp <- check.derivatives(.x=x0, func=obj_wfs, func_grad=grad_wfs, 
#                    check_derivatives_print='all', inputs=inputs)


check <- tibble(cname=inputs$target_names,
                pweight=inputs$priority_weights,
                fileval=fileval_vec,
                target=inputs$target_vec,
                optval=calc_constraints(result$solution, inputs$ccoef, inputs$target_names),
                diff0=fileval - target,
                diff_opt=optval - target,
                pdiff=diff_opt / target * 100,
                apdiff=abs(pdiff))
glimpse(check)
check$pdiff %>% round(2)
check %>% arrange(-apdiff)
check %>% arrange(apdiff)

wcheck <- tibble(xlb, x0, sol=result$solution, xub)

v <- str_subset(names(d), "e00400")



rungroup <- function(ugroup.in, targets_use, estack_tc_groups, ccoef, maxiter=10){
  a <- proc.time()
  
  # set up the targets
  priority1_vars <- c("wt", "c00100", "e00200", "e00300", "e00400", "e01500", "p23250", "taxbc", "c62100", "c09600")
  targ_samp <- targets_use %>%
    filter(ugroup==ugroup.in) %>%
    mutate(priority_weight=case_when(vname %in% priority1_vars ~ 100,
                                     TRUE ~ 1))
  # set up the data subset
  syn_samp <- estack_tc_groups %>% 
    filter(ftype=="syn", ugroup==ugroup.in) %>% 
    ungroup

  # set up the constraint coefficients
  ccoef_samp <- ccoef %>%
    filter(ftype=="syn", ugroup==ugroup.in)
  
  # set up the inputs list
  get_named_vec <- function(vec, vec_names, target_names){
    names(vec) <- vec_names
    return(vec[target_names])
  }
  
  target_names <- intersect(targ_samp$good_constraint, names(ccoef_samp))
  
  target_vec <- get_named_vec(vec = targ_samp$target,
                              vec_names = targ_samp$good_constraint,
                              target_names = target_names)
  
  priority_weights <- get_named_vec(vec = targ_samp$priority_weight,
                                    vec_names = targ_samp$good_constraint,
                                    target_names = target_names)
  
  fileval_vec <- get_named_vec(vec = targ_samp$file_value,
                               vec_names = targ_samp$good_constraint,
                               target_names = target_names)
  
  scale_vec <- ifelse(target_vec==0, fileval_vec, target_vec)
  
  inputs <- list()
  inputs$target_names <- target_names
  inputs$target_vec <- target_vec
  inputs$priority_weights <- priority_weights
  inputs$scale_vec <- scale_vec
  inputs$ccoef <- ccoef_samp
  
  # starting point and bounds on the weights
  wts <- syn_samp$s006 / 6
  xlb <- rep(1, length(wts))
  x0 <- pmax(wts, 1)
  xub <- pmax(wts*2, 10e3)
  
  # call the optimizer
  opts <- list("algorithm"="NLOPT_LD_MMA",
               "xtol_rel"=1.0e-8,
               "maxeval"=maxiter)
  result <- nloptr(x0, 
                   eval_f=obj_wfs,
                   eval_grad_f = grad_wfs,
                   lb = xlb, ub = xub,
                   opts = opts, inputs=inputs)
  
  outputs <- list()
  outputs$inputs <- inputs
  outputs$result <- result

  b <- proc.time()
  # print(b - a)
  outputs$elapsed <- b - a
  return(outputs)
}

tmp <- rungroup(6, targets_use, estack_tc_groups, ccoef, maxiter=500)
names(tmp)
tmp$elapsed
tmp$result$message
tmp$result$objective
length(tmp$result$solution)

