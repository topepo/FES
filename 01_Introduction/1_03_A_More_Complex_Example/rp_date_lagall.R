library(caret)
library(sessioninfo)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores() - 2)
registerDoParallel(cl)

# ------------------------------------------------------------------------------

# Load pre-made data
load("../../Data_Sets/Chicago_trains/chicago.RData")

# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------

pred_vars <- c(var_sets$dates, var_sets$lag14, var_sets$other_lag)
  
set.seed(4194)
rp_date_lagall <- train(y = training$s_40380,
                        x = training[, pred_vars],
                        method = "rpart",
                        tuneLength = 20,
                        metric = "RMSE",
                        maximize = FALSE,
                        trControl = ctrl)

attr(rp_date_lagall$finalModel$terms, ".Environment") <- emptyenv()
rp_date_lagall$call <- NULL

save(rp_date_lagall, file = "rp_date_lagall.RData")

# ------------------------------------------------------------------------------

session_info()

if(!interactive()) 
  q("no")