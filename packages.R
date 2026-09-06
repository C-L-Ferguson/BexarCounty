# Run this once to install all required packages
install.packages(c(
  "tidyverse",
  "arrow",
  "broom",
  "fixest",   # fast FE logit for M3/M5 (strongly recommended)
  "zoo",      # rolling averages in individual trajectory plots
  "scales",   # percent formatting in figures
  "purrr"     # map functions (included in tidyverse)
))
