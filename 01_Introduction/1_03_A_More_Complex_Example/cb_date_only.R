library(caret)
library(sessioninfo)
library(Cubist)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores() - 2)
registerDoParallel(cl)

# ------------------------------------------------------------------------------

# Load pre-made data
load("../../Data_Sets/Chicago_trains/chicago.RData")

# ------------------------------------------------------------------------------


cb_grid <- expand.grid(committees = c((1:10)*2 -1, 20, 50),
                       neighbors = c(0, 1, 3, 7, 9))

# ------------------------------------------------------------------------------

set.seed(4194)
cb_date_only <- train(s_40380 ~ .,
                      data = training[, c("s_40380", var_sets$dates)],
                      method = "cubist",
                      tuneGrid = cb_grid,
                      metric = "RMSE",
                      maximize = FALSE,
                      trControl = ctrl)
save(cb_date_only, file = "cb_date_only.RData")

# ------------------------------------------------------------------------------

session_info()

if(!interactive()) 
  q("no")