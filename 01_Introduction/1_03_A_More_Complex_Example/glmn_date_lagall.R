library(caret)
library(sessioninfo)
library(glmnet)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores() - 2)
registerDoParallel(cl)

# ------------------------------------------------------------------------------

# Load pre-made data
load("../../Data_Sets/Chicago_trains/chicago.RData")

# ------------------------------------------------------------------------------


glmn_grid <- expand.grid(alpha = c(0.05, seq(.1, 1, by = 0.05)),
                         lambda = 2^(-2:5))

# ------------------------------------------------------------------------------

pred_vars <- c(var_sets$dates, var_sets$lag14, var_sets$other_lag)
  
set.seed(4194)
glmn_date_lagall <- train(s_40380 ~ .,
                          data = training[, c("s_40380", pred_vars)],
                          method = "glmnet",
                          preProc = c("center", "scale"),
                          tuneGrid = glmn_grid,
                          metric = "RMSE",
                          maximize = FALSE,
                          trControl = ctrl)
save(glmn_date_lagall, file = "glmn_date_lagall.RData")

# ------------------------------------------------------------------------------

session_info()

if(!interactive()) 
  q("no")