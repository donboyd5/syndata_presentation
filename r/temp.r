set.seed(3951824)
# Table of counts (contingency)
X <- sample(letters[1:3], 100, 1)
Y <- sample(letters[4:5], 100, 1)
t  <- table(X,Y)
tp  <- t/100 # proportions
tn  <- tp/sum(tp)     # normalized, joints
p_x <- rowSums(tn)    # marginals
p_y <- colSums(tn)

P <- tn 
Q <- p_x %o% p_y 

# P(X, Y)   : bin frequency: P_i
# P(X) P(Y) : bin frequency, Q_i 
mi <- sum(P*log(P/Q))
library(entropy)
mi.empirical(t) == mi



library(psycho)

x <- rnorm(100, 1, 0.5)
y <- rnorm(100, 0, 1)
overlap(x, y)








library(FNN)
set.seed(1000)
X<- rexp(1000, rate=0.2)
Y<- rexp(10000, rate=0.4)

KL.divergence(X, Y, k=5)



install.packages("magick")
install.packages("webshot")
webshot::install_phantomjs()


kable(mtcars[1:5, ], "html") %>%
  kable_styling("striped") %>%
  row_spec(1, color = "red") %>%
  save_kable("/results/test.pdf")

kable(mtcars, "latex") %>%
  kable_styling(latex_options = "striped") %>%
  save_kable("test.png")

mtcars %>%
  kable() %>%
  kable_styling() %>%
  save_kable("test.pdf")

mtcars %>%
  kable() %>%
  kable_styling() %>%
  save_kable("test.html", self_contained = F)

webshot::webshot("test.html")

library(flextable)
ft <- flextable( head( mtcars ) )
ft <- autofit(ft)
ft
save_as_image(ft, path = "name.png")

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


p <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point(aes(colour = factor(cyl)))

p + scale_colour_manual(values = c("blue", "darkgreen", "red"))

# It's recommended to use a named vector
cols <- c("8" = "red", "4" = "blue", "6" = "darkgreen", "10" = "orange")
p + scale_colour_manual(values = cols) # look at legend

p + scale_colour_manual(
  values = cols,
  breaks = c("4", "6", "8"),
  labels = c("four", "six", "eight")
)
