



# Here is the python extrapolation code in Tax-Calculator/taxcalc/records.py

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

# interest income adjustment in Tax-Calculator/taxcalc/records.py
# def _adjust(self, year):
#   """
#         Adjust value of income variables to match SOI distributions
#         Note: adjustment must leave variables as numpy.ndarray type
#         """
# # pylint: disable=no-member
# if self.ADJ.size > 0:
#     # Interest income
#     self.e00300 *= self.ADJ['INT{}'.format(year)][self.agi_bin].values

