
#****************************************************************************************************
#                Goal and tasks ####
#***************************************************************************************************
# Get the weighted approved synthetic file: synpuf20_no_disclosures_weighted.csv
# Discard or rename the variable S006, which is a weight variable, and then rename S006_rwt to S006
# run this through TaxData with your new weighting approach

# Per Yimeng:
# There are 8 missing variables in the synthetic puf:
#   
# puf['xocah']  "Exemptions for Children Living at Home"
# puf['xocawh'] "Exemptions for Children Living Away from Home"
# puf['xoodep']  "Exemptions for Other Dependents"
# puf['xopar']    "Exemptions for Parents Living at Home or Away from Home"
# puf['e03260'] "Deduction for self-employment tax"
# puf['e00100']  "Adjusted Gross Income (deficit)  (AGI)  (+/-)"
# puf['e02500'] "Social Security benefits in AGI"
# puf['e04800'] "Taxable income"

# I think the key missing variable is e00100 (AGI), which is used to construct the targets for wage/salary distribution (e00200). 

missvars <- c("XOCAH", "XOCAWH", "XOODEP", "XOPAR", "E03260", "E00100", "E02500", "E04800")


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

library("scales")
library("hms") # hms, for times.
library("lubridate") # lubridate, for date/times.
library("readxl") # readxl, for .xls and .xlsx files.
library("haven") # haven, for SPSS, SAS and Stata files.
library("vctrs")
# library("precis") # use precis2 from btools

library("tibbletime") # https://business-science.github.io/tibbletime/

library("grDevices")
library("knitr")

library("zoo") # for rollapply

library("btools") # library that I created (install from github)
library("bdata")


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
#               globals ####
#****************************************************************************************************
pufpath <- "C:/Users/donbo/Dropbox/OSPC - Shared/IRS_pubuse_2011/puf2011.csv"

syndir <- "C:/Users/donbo/Google Drive/synpuf/syntheses/"
synfn <-  "synpuf20_no_disclosures_weighted.csv"
syndanfn <- "synpuf20calc.csv"
synfn_td <-  "synpuf20_no_disclosures_weighted_for_taxdata.csv"

outdir <- "d:/temp/"


syndrop <- "C:/Users/donbo/Dropbox/synpuf/"
synpath <- paste0(syndrop, synfn)
danpath <- paste0(syndrop, syndanfn)
syntdpath <- paste0(syndrop, synfn_td)


#****************************************************************************************************
#               get true puf and verify that it has all of missvars ####
#****************************************************************************************************
puf <- read_csv(pufpath)
missvars %in% names(puf) # good
rm(puf)


#****************************************************************************************************
#                Run synpuf through Tax-Calculator ####
#****************************************************************************************************
# Run the synpuf through Tax-Calculator, get results, select key variables, and merge into the synfile

#.. define the Windows command to call the tc CLI ####
# This is an excerpt from a function I wrote
# Build a Windows system command that will call the Tax-Calculator CLI. See:
#   https://pslmodels.github.io/Tax-Calculator/
# CAUTION: must use full dir names, not relative to working directory
# CAUTION: any directory names that have spaces in them must be shQuoted
# CAUTION: when I updated Anaconda most recently, I had to add 
#   C:\Users\donbo\Anaconda3\Library\bin to the system path for Tax-Calculator to work with the system(cmd) approach
# CAUTION: 2013 is the FIRST possible tax year that Tax-Calculator will do

# Here is the tc CLI usage: 
# tc INPUT TAXYEAR [--help]
# [--baseline BASELINE] [--reform REFORM] [--assump  ASSUMP]
# [--exact] [--tables] [--graphs]
# [--dump] [--dvars DVARS] [--sqldb] [--outdir OUTDIR]
# [--test] [--version]  

# Use system2 command: here is an example:
# cmd1 <- "C:/Users/donbo/Anaconda3/Scripts/tc"
# args <- c("D:/tcdir/synth10syn20.csv", "2013", 
#           "--reform", "D:/Dropbox/RPrograms PC/OSPC/syndata4/tax_plans/brk4_1k_2013.json", 
#           "--dump", 
#           "--outdir", "D:/tcdir/")
# system2(cmd1, args)

# Do not include --reform and its location if this is a baseline run


# cmd1 <- "C:/ProgramData/Anaconda3/Scripts/tc"
# args <- c(shQuote(synpath), "2018",
#           "--dump",
#           "--outdir", outdir)
# cmd1
# args

#.. run the command ----
# a <- proc.time()
# system2(cmd1, args) # CAUTION: this will overwrite any existing output file that had same input filename!
# # consider system2
# b <- proc.time()
# b - a  # it can easily take 5-10 minutes depending on the size of the input file

# tc will name the output file synpuf20_no_disclosures_weighted-13-#-#-#.csv
# inspect this in explorer to be sure all is good  

#****************************************************************************************************
#               retrieve results, rename as needed, merge with synpuf, save merged file ####
#****************************************************************************************************

# syntc_fn <- "synpuf20_no_disclosures_weighted-13-#-#-#.csv"
# 
# syn <- read_csv(synpath)
# glimpse(syn)
# ns(syn)
# 
# syntc <- read_csv(paste0(outdir, syntc_fn))
# glimpse(syntc)
# ns(syntc)
# 
# ht(syn %>% select(RECID, S006, S006_rwt))
# ht(syntc %>% select(RECID, s006)) # why does tc zero-out s006??
# 
# ht(syn %>% select(RECID, E00200))
# ht(syntc %>% select(RECID, c00100, data_source, e00200, e00200p, e00200s))
# 
# ht(syn %>% select(RECID, XTOT, MARS))
# ht(syntc %>% select(RECID, XTOT, MARS, c00100))
# 
# 
# syntc %>% filter(taxbc > 0) %>% head


#****************************************************************************************************
#               get synpuf and Dan's results, merge, save merged file ####
#****************************************************************************************************
syn <- read_csv(synpath)
glimpse(syn)
ns(syn)

# danpath <- "D:/tax_data/synpuf20calc.csv" #
syndan <- read_csv(danpath)
glimpse(syndan)
ns(syndan)

syn_taxdata <- syn %>%
  left_join(syndan %>% select(RECID, E00100=c00100)) %>%
  select(-S006) %>%
  rename(S006=S006_rwt) %>%
  mutate(XOCAH=0)
glimpse(syn_taxdata)

syn_taxdata %>% write_csv(syntdpath)
sum(syn_taxdata$S006) /100 / 1e6

sum(puf %>% filter(RECID < 999996) %>% .[["S006"]]) / 100 / 1e6

sum(syn_taxdata$S006 * syn_taxdata$E00100) /100 / 1e9
puf %>% filter(RECID < 999996) %>% summarise(agi=sum(S006 / 100 * E00100) / 1e9)

sum(syn_taxdata$S006 * syn_taxdata$E00200) /100 / 1e9
puf %>% filter(RECID < 999996) %>% summarise(wages=sum(S006 / 100 * E00200) / 1e9) %>% as.data.frame


