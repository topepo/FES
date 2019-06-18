# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 8.2 at
# https://bookdown.org/max/FES/models-that-are-resistant-to-missing-values.html
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(caret)
library(rpart)
library(partykit)

# Data used --------------------------------------------------------------------

data(scat)


# Figure 8.6 -------------------------------------------------------------------
# https://bookdown.org/max/FES/models-that-are-resistant-to-missing-values.html#fig:missing-rpart

rpart_mod <- rpart(Species ~ ., data=scat)
rpart_party <- as.party(rpart_mod)
# plot(rpart_party)

# ------------------------------------------------------------------------------

sessionInfo()
