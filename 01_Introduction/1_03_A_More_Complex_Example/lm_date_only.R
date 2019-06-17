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
lm_date_only <- train(s_40380 ~ .,
                      data = training[, c("s_40380", var_sets$dates)],
                      method = "lm",
                      metric = "RMSE",
                      maximize = FALSE,
                      model = FALSE,
                      trControl = ctrl)

attr(lm_date_only$finalModel$terms, ".Environment") <- emptyenv()

save(lm_date_only, file = "lm_date_only.RData")

# ------------------------------------------------------------------------------

session_info()

if(!interactive()) 
  q("no")