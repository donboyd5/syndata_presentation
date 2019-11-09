
kable(mtcars[1:5, ], "html") %>%
  kable_styling("striped") %>%
  row_spec(1, color = "red") %>%
  save_kable("/results/test.pdf")

kable(mtcars, "latex") %>%
  kable_styling(latex_options = "striped") %>%
  save_kable("results/test.png")


xcheck %>%
  filter(ftype=="puf", RECID < 999996) %>%
  gather(xvar, value, starts_with("X")) %>%
  group_by(MARS, xvar, value) %>%
  summarise(n=n()) %>%
  mutate(value=paste0("numexempt_", value)) %>%
  spread(value, n) %>%
  mutate_at(vars(starts_with("num")), list(~naz(.))) %>%
  kable(digits=0, format = "rst", format.args = list(big.mark=",")) 


xcheck %>%
  filter(ftype=="puf", RECID < 999996) %>%
  gather(xvar, numexempt, starts_with("X"), -XTOT) %>%
  group_by(MARS, XTOT, xvar, numexempt) %>%
  summarise(n=n()) %>%
  mutate(numexempt=paste0("numexempt_", numexempt)) %>%
  spread(numexempt, n) %>%
  mutate_at(vars(starts_with("num")), list(~naz(.))) %>%
  filter(MARS %in% 2) %>%
  kable(digits=0, format = "rst", format.args = list(big.mark=",")) 
