library(caret)
library(sessioninfo)
library(earth)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores() - 2)
registerDoParallel(cl)

# ------------------------------------------------------------------------------

# Load pre-made data
load("../../Data_Sets/Chicago_trains/chicago.RData")

# ------------------------------------------------------------------------------

set.seed(4194)
mars_date_only <- train(s_40380 ~ .,
                       data = training[, c("s_40380", var_sets$dates)],
                       method = "earth",
                       tuneLength = 20,
                       metric = "RMSE",
                       maximize = FALSE,
                       trControl = ctrl)

mars_date_only$finalModel$call <- NULL
mars_date_only$finalModel$x <- NULL

save(mars_date_only, file = "mars_date_only.RData")

# ------------------------------------------------------------------------------

session_info()

if(!interactive()) 
  q("no")