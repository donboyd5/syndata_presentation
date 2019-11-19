
# apply growfactors for year to data frame df
# arguments:
#   year  integer
#   df    data frame
# return:
#   df as grown
# note that e00900p, e00900s, and e02000 have different growfactors for positive and negative amounts
# and that therefore e00900 will have to be recomputed after growing prime and spouse separately

# This is based on the python code from Tax-Calculator function _extrapolate in records.py,
# which is copied below.


# C:\ProgramData\Anaconda3\Lib\site-packages\taxcalc\growfactors.csv is where my growfactors are
growpath <- "C:/ProgramData/Anaconda3/Lib/site-packages/taxcalc/growfactors.csv"
gflink_path <- "C:/Users/donbo/Dropbox/RPrograms PC/OSPC/syndata_presentation/data/apply_growfactors.xlsm"

gf <- read_csv(growpath)
gflink <- read_excel(gflink_path, sheet="growfactors_link") %>%
  filter(!is.na(vname))

gflong <- gflink %>%
  left_join(gf %>% rename(year=YEAR) %>% pivot_longer(-year, names_to="gfname", values_to="gfactor"))

df <- estack_rotten
year_grow <- 2018

growvars <- intersect(names(df), unique(gflong$vname))
dontgrowvars <- setdiff(names(df), growvars)
dontgrowvars %>% sort
gflong %>% filter(year==year_grow, vname %in% growvars)




#> ns(estack_rotten)
# [1] "age_head"           "age_spouse"         "agi_bin"            "cmbtp"              "DSI"                "e00200"             "e00200p"           
# [8] "e00200s"            "e00300"             "e00400"             "e00600"             "e00650"             "e00700"             "e00800"            
# [15] "e00900"             "e00900p"            "e00900s"            "e01100"             "e01200"             "e01400"             "e01500"            
# [22] "e01700"             "e02000"             "e02100"             "e02100p"            "e02100s"            "e02300"             "e02400"            
# [29] "e03150"             "e03210"             "e03220"             "e03230"             "e03240"             "e03270"             "e03290"            
# [36] "e03300"             "e03400"             "e03500"             "e07240"             "e07260"             "e07300"             "e07400"            
# [43] "e07600"             "e09700"             "e09800"             "e09900"             "e11200"             "e17500"             "e18400"            
# [50] "e18500"             "e19200"             "e19800"             "e20100"             "e20400"             "e24515"             "e24518"            
# [57] "e26270"             "e27200"             "e32800"             "e58990"             "e62900"             "e87521"             "e87530"            
# [64] "EIC"                "elderly_dependents" "f2441"              "f6251"              "fips"               "ftype"              "g20500"            
# [71] "k1bx14p"            "k1bx14s"            "MARS"               "MIDR"               "n1820"              "n21"                "n24"               
# [78] "nu05"               "nu13"               "nu18"               "p08000"             "p22250"             "p23250"             "RECID"             
# [85] "RECID_original"     "s006"               "XTOT"              

gf

# wagevars <- c("e00200", "e00200p", "e00200s", "e01500", "e01700")

# def _extrapolate(self, year):
#   """
#         Apply to variables the grow factor values for specified calendar year.
#         """
# # pylint: disable=too-many-statements,no-member
# # put values in local dictionary
# gfv = dict()
# for name in GrowFactors.VALID_NAMES:
#   gfv[name] = self.gfactors.factor_value(name, year)
#   # apply values to Records variables
#   self.e00200 *= gfv['AWAGE']
#   self.e00200p *= gfv['AWAGE']
#   self.e00200s *= gfv['AWAGE']
#   self.pencon_p *= gfv['AWAGE']
#   self.pencon_s *= gfv['AWAGE']
#   self.e00300 *= gfv['AINTS']
#   self.e00400 *= gfv['AINTS']
#   self.e00600 *= gfv['ADIVS']
#   self.e00650 *= gfv['ADIVS']
#   self.e00700 *= gfv['ATXPY']
#   self.e00800 *= gfv['ATXPY']
#   self.e00900s[:] = np.where(self.e00900s >= 0,
#                              self.e00900s * gfv['ASCHCI'],
#                              self.e00900s * gfv['ASCHCL'])
#   self.e00900p[:] = np.where(self.e00900p >= 0,
#                              self.e00900p * gfv['ASCHCI'],
#                              self.e00900p * gfv['ASCHCL'])
#   self.e00900[:] = self.e00900p + self.e00900s
#   self.e01100 *= gfv['ACGNS']
#   self.e01200 *= gfv['ACGNS']
#   self.e01400 *= gfv['ATXPY']
#   self.e01500 *= gfv['ATXPY']
#   self.e01700 *= gfv['ATXPY']
#   self.e02000[:] = np.where(self.e02000 >= 0,
#                             self.e02000 * gfv['ASCHEI'],
#                             self.e02000 * gfv['ASCHEL'])
#   self.e02100 *= gfv['ASCHF']
#   self.e02100p *= gfv['ASCHF']
#   self.e02100s *= gfv['ASCHF']
#   self.e02300 *= gfv['AUCOMP']
#   self.e02400 *= gfv['ASOCSEC']
#   self.e03150 *= gfv['ATXPY']
#   self.e03210 *= gfv['ATXPY']
#   self.e03220 *= gfv['ATXPY']
#   self.e03230 *= gfv['ATXPY']
#   self.e03270 *= gfv['ACPIM']
#   self.e03240 *= gfv['ATXPY']
#   self.e03290 *= gfv['ACPIM']
#   self.e03300 *= gfv['ATXPY']
#   self.e03400 *= gfv['ATXPY']
#   self.e03500 *= gfv['ATXPY']
#   self.e07240 *= gfv['ATXPY']
#   self.e07260 *= gfv['ATXPY']
#   self.e07300 *= gfv['ABOOK']
#   self.e07400 *= gfv['ABOOK']
#   self.p08000 *= gfv['ATXPY']
#   self.e09700 *= gfv['ATXPY']
#   self.e09800 *= gfv['ATXPY']
#   self.e09900 *= gfv['ATXPY']
#   self.e11200 *= gfv['ATXPY']
#   # ITEMIZED DEDUCTIONS
#   self.e17500 *= gfv['ACPIM']
#   self.e18400 *= gfv['ATXPY']
#   self.e18500 *= gfv['ATXPY']
#   self.e19200 *= gfv['AIPD']
#   self.e19800 *= gfv['ATXPY']
#   self.e20100 *= gfv['ATXPY']
#   self.e20400 *= gfv['ATXPY']
#   self.g20500 *= gfv['ATXPY']
#   # CAPITAL GAINS
#   self.p22250 *= gfv['ACGNS']
#   self.p23250 *= gfv['ACGNS']
#   self.e24515 *= gfv['ACGNS']
#   self.e24518 *= gfv['ACGNS']
#   # SCHEDULE E
#   self.e26270 *= gfv['ASCHEI']
#   self.e27200 *= gfv['ASCHEI']
#   self.k1bx14p *= gfv['ASCHEI']
#   self.k1bx14s *= gfv['ASCHEI']
#   # MISCELLANOUS SCHEDULES
#   self.e07600 *= gfv['ATXPY']
#   self.e32800 *= gfv['ATXPY']
#   self.e58990 *= gfv['ATXPY']
#   self.e62900 *= gfv['ATXPY']
#   self.e87530 *= gfv['ATXPY']
#   self.e87521 *= gfv['ATXPY']
#   self.cmbtp *= gfv['ATXPY']
#   # BENEFITS
#   self.other_ben *= gfv['ABENOTHER']
#   self.mcare_ben *= gfv['ABENMCARE']
#   self.mcaid_ben *= gfv['ABENMCAID']
#   self.ssi_ben *= gfv['ABENSSI']
#   self.snap_ben *= gfv['ABENSNAP']
#   self.wic_ben *= gfv['ABENWIC']
#   self.housing_ben *= gfv['ABENHOUSING']
#   self.tanf_ben *= gfv['ABENTANF']
#   self.vet_ben *= gfv['ABENVET']
#   # remove local dictionary
#   del gfv