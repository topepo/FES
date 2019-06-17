# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(tidymodels)

# using base R -----------------------------------------------------------------

values <- c("low", "medium", "high")
dat <- data.frame(x = ordered(values, levels = values))

# https://bookdown.org/max/FES/encodings-for-ordered-data.html#tab:categorical-ordered-table
model.matrix(~ x, dat)

# using recipes ----------------------------------------------------------------

# https://bookdown.org/max/FES/encodings-for-ordered-data.html#tab:categorical-ordered-table
recipe(~ x, data = dat) %>% 
  step_dummy(x) %>% 
  prep() %>% 
  juice()

# ------------------------------------------------------------------------------

sessionInfo()

