library(caret)
library(sessioninfo)
library(xgboost)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores() - 2)
registerDoParallel(cl)

# ------------------------------------------------------------------------------

# Load pre-made data
load("../../Data_Sets/Chicago_trains/chicago.RData")

# ------------------------------------------------------------------------------


rand_ctrl <- ctrl
rand_ctrl$search <- "random"

# ------------------------------------------------------------------------------

set.seed(4194)
boost_date_only <- train(s_40380 ~ .,
                         data = training[, c("s_40380", var_sets$dates)],
                         method = "xgbTree",
                         tuneLength = 50,
                         metric = "RMSE",
                         maximize = FALSE,
                         trControl = rand_ctrl)
save(boost_date_only, file = "boost_date_only.RData")

# ------------------------------------------------------------------------------

session_info()

if(!interactive()) 
  q("no")