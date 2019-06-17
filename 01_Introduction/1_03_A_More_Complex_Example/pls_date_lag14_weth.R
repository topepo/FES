library(caret)
library(sessioninfo)
library(pls)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores() - 2)
registerDoParallel(cl)

# ------------------------------------------------------------------------------

# Load pre-made data
load("../../Data_Sets/Chicago_trains/chicago.RData")

# ------------------------------------------------------------------------------

pred_vars <- c(var_sets$dates, var_sets$lag14, var_sets$weather)

set.seed(4194)
pls_date_lag14_weth <- train(s_40380 ~ .,
                             data = training[, c("s_40380", pred_vars)],
                             method = "simpls",
                             preProc = c("center", "scale"),
                             tuneLength = 30,
                             metric = "RMSE",
                             maximize = FALSE,
                             trControl = ctrl)

attr(pls_date_lag14_weth$finalModel$terms, ".Environment") <- emptyenv()
pls_date_lag14_weth$finalModel$model <- NULL

save(pls_date_lag14_weth, file = "pls_date_lag14_weth.RData")

# ------------------------------------------------------------------------------

session_info()

if(!interactive()) 
  q("no")