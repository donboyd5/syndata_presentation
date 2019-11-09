
# in addition to the libraries loaded below you will also need installed:
#   here -- utilities that allow us to reference file locations in the project directory consistently
#           (rmarkdown/knitr will change the working directory depending on how it is used - this works around the issue)
#   tigris -- for a mapping of state FIPS codes and state abbreviations

library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

library("scales")
library("hms") # hms, for times
library("lubridate") # lubridate, for date/times
library("vctrs")

library("btools")

# tables
library("knitr")
library("kableExtra")

library("flextable")
# library(officer)

# graphics
library("grDevices")
library("grid")
library("gridExtra")
library("gtable")
library("egg")

library("ggrepel") # for moving label text away from points
# library("ggalt") # for encircling groups of points
# library("ggpubr")