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

pred_vars <- c(var_sets$dates, var_sets$lag14, var_sets$holidays)

set.seed(4194)
rf_date_lag14_hol <- train(y = training$s_40380,
                           x = training[, pred_vars],
                           method = "ranger",
                           num.trees = 2000, 
                           verbose = FALSE,
                           tuneLength = 50,
                           metric = "RMSE",
                           maximize = FALSE,
                           trControl = ctrl,
                           num.threads = 1) 
save(rf_date_lag14_hol, file = "rf_date_lag14_hol.RData")

# ------------------------------------------------------------------------------

session_info()

if(!interactive()) 
  q("no")