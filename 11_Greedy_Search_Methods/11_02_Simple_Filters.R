# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 12.2 at
# https://bookdown.org/max/FES/greedy.html#simple-filters
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(caret)
library(tidymodels)
library(pROC)
library(doParallel)

cl <- makeForkCluster(nnodes = parallel::detectCores() - 1)
registerDoParallel(cl)

# ------------------------------------------------------------------------------

load("../Data_Sets/Parkinsons_Disease/pd_split.RData")

# Create an initial recipe -----------------------------------------------------

pd_rec <-
  recipe(Class ~ ., data = pd_tr) %>%
  step_YeoJohnson(all_predictors())  %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

basic_pred <-
  prep(pd_rec, training = pd_tr, retain = TRUE) %>%
  juice(all_predictors(), composition = "data.frame")

ncol(basic_pred)

# ------------------------------------------------------------------------------

many_stats <-
  function(data, lev = levels(data$obs), model = NULL) {
    c(
      twoClassSummary(data = data, lev = levels(data$obs), model),
      prSummary(data = data, lev = levels(data$obs), model),
      mnLogLoss(data = data, lev = levels(data$obs), model),
      defaultSummary(data = data, lev = levels(data$obs), model)
    )
  }

# Make the overall model -------------------------------------------------------

tr_ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 5,
  classProbs = TRUE,
  summaryFunction = many_stats
)

set.seed(184)
full_model <- train(
  pd_rec,
  data = pd_tr,
  method = "widekernelpls",
  tuneLength = 20,
  trControl = tr_ctrl,
  metric = "ROC"
)

# Setup the filtering options and control objects ------------------------------

roc_filter <- caretSBF

roc_filter$summary <- many_stats

roc_filter$score <- function(x, y) {
  # Get the areas under the ROC curve for each predictor
  roc_vals <- filterVarImp(x, y)
  # return a named vector:
  res <- roc_vals$PD
  names(res) <- rownames(roc_vals)
  rownames(res) <- NULL
  res
}

roc_filter$filter <- function(score, x, y)  {
  res <- score >= 0.80
  # But always keep at least two
  if (sum(res) < 2) {
    ord_by_best <- seq_along(score)[order(score, decreasing = TRUE)]
    res[head(ord_by_best, 2)] <- TRUE
  }
  res
}

sbf_ctrl <- sbfControl(
  method = "repeatedcv",
  repeats = 5,
  functions = roc_filter,
  multivariate = TRUE
)

internal_ctrl <- trainControl(
  method = "boot",
  number = 20,
  classProbs = TRUE,
  summaryFunction = many_stats,
  allowParallel = FALSE
)

# Filtering and models ---------------------------------------------------------

set.seed(184)
pls_filtered <- sbf(
  pd_rec,
  data = pd_tr,
  sbfControl = sbf_ctrl,
  # options for `train()`:
  method = "widekernelpls",
  tuneLength = 20,
  trControl = internal_ctrl,
  metric = "ROC"
)

# Random subset size validation ------------------------------------------------

subset_size <- length(pls_filtered$optVariables)
iter <- 100
vars <- names(basic_pred)

subset_ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 5,
  index = tr_ctrl$index,
  indexOut = tr_ctrl$indexOut,
  classProbs = TRUE,
  summaryFunction = many_stats
)

for (i in 1:iter) {
  set.seed(14 + i)
  rand_subset <- sample(vars, subset_size)

  subset_rec <-
    recipe(Class ~ ., data = pd_tr[, c("Class", rand_subset)]) %>%
    step_YeoJohnson(all_predictors())  %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())

  subset_model <- train(
    pd_rec,
    data = pd_tr,
    method = "widekernelpls",
    tuneLength = 20,
    trControl = subset_ctrl,
    metric = "ROC"
  )
  subset_perf <- getTrainPerf(subset_model)
  subset_perf$ncomp <- subset_model$bestTune$ncomp
  if (i == 1) {
    size_check <- subset_perf
  } else {
    size_check <- bind_rows(size_check, subset_perf)
  }
  rm(rand_subset, subset_rec, subset_model, subset_perf)
}

subset_size
mean(size_check$TrainROC <= pls_filtered$results$ROC)

# ------------------------------------------------------------------------------

save(pls_filtered, full_model, size_check, file = "pls_filtered.RData")

# ------------------------------------------------------------------------------

sessionInfo()

if (!interactive())
  q("no")

