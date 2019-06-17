library(caret)
library(sessioninfo)
library(ranger)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores() - 2)
registerDoParallel(cl)

# ------------------------------------------------------------------------------

# Load pre-made data
load("../../Data_Sets/Chicago_trains/chicago.RData")

# ------------------------------------------------------------------------------

set.seed(4194)
rf_date_only <- train(y = training$s_40380,
                      x = training[,  var_sets$dates],
                      method = "ranger",
                      num.trees = 2000, 
                      verbose = FALSE,
                      tuneLength = 4,
                      metric = "RMSE",
                      maximize = FALSE,
                      trControl = ctrl,
                      num.threads = 1)
save(rf_date_only, file = "rf_date_only.RData")

# ------------------------------------------------------------------------------

session_info()

if(!interactive()) 
  q("no")