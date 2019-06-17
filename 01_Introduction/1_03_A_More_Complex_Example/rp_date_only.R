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

set.seed(4194)
rp_date_only <- train(y = training$s_40380,
                      x = training[,  var_sets$dates],
                      method = "rpart",
                      tuneLength = 20,
                      metric = "RMSE",
                      maximize = FALSE,
                      trControl = ctrl)

attr(rp_date_only$finalModel$terms, ".Environment") <- emptyenv()
rp_date_only$call <- NULL

save(rp_date_only, file = "rp_date_only.RData")

# ------------------------------------------------------------------------------

session_info()

if(!interactive()) 
  q("no")