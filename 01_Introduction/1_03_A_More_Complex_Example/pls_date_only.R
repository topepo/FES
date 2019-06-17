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

set.seed(4194)
pls_date_only <- train(s_40380 ~ .,
                       data = training[, c("s_40380", var_sets$dates)],
                       method = "simpls",
                       preProc = c("center", "scale"),
                       tuneLength = 20,
                       metric = "RMSE",
                       maximize = FALSE,
                       trControl = ctrl)

attr(pls_date_only$finalModel$terms, ".Environment") <- emptyenv()
pls_date_only$finalModel$model <- NULL

save(pls_date_only, file = "pls_date_only.RData")

# ------------------------------------------------------------------------------

session_info()

if(!interactive()) 
  q("no")