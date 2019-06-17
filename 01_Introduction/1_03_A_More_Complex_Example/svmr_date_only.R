library(caret)
library(sessioninfo)
library(kernlab)

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
svmr_date_only <- train(s_40380 ~ .,
                        data = training[, c("s_40380", var_sets$dates)],
                        method = "svmRadial",
                        preProc = c("center", "scale"),
                        tuneLength = 50,
                        metric = "RMSE",
                        maximize = FALSE,
                        trControl = rand_ctrl)
save(svmr_date_only, file = "svmr_date_only.RData")

# ------------------------------------------------------------------------------

session_info()

if(!interactive()) 
  q("no")