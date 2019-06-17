# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 12.3 at
# https://bookdown.org/max/FES/greedy.html#recursive-feature-elimination
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(caret)
library(tidymodels)
library(pROC)
library(randomForest)
library(doParallel)

cl <- makeForkCluster(nnodes = parallel::detectCores() - 1)
registerDoParallel(cl)

# ------------------------------------------------------------------------------

load("../Data_Sets/Parkinsons_Disease/pd_split.RData")

# RFE using ROC Curve Ranking --------------------------------------------------

many_stats <-
  function(data, lev = levels(data$obs), model = NULL) {
    c(
      twoClassSummary(data = data, lev = levels(data$obs), model),
      prSummary(data = data, lev = levels(data$obs), model),
      mnLogLoss(data = data, lev = levels(data$obs), model),
      defaultSummary(data = data, lev = levels(data$obs), model)
    )
  }

rfe_funcs <- caret::rfFuncs

rfe_funcs$summary <- many_stats

# Use the ROC AUC values for feature ranking
rfe_funcs$rank <- function(object, x, y) {
  roc_vals <- filterVarImp(x, y)
  roc_vals$var <- rownames(roc_vals)
  names(roc_vals)[1] <- "Overall"
  rownames(roc_vals) <- NULL
  roc_vals$control <- NULL
  roc_vals[order(-roc_vals$Overall),, drop = FALSE]
}

# Outer control for RFE; same resamples as before
rfe_ctrl <- rfeControl(
  method = "repeatedcv",
  repeats = 5,
  functions = rfe_funcs,
  returnResamp = "all",
  verbose = TRUE
)

pd_rec <- 
  recipe(Class ~ ., data = pd_tr) %>% 
  step_corr(all_predictors(), threshold = 0.5)

# ------------------------------------------------------------------------------

sizes <- unique(floor(10^seq(0, 2.87, length = 50)))

# When running the RFE procedures in parallel, each worker will require about
# 600MB of memory. 

set.seed(184)
rf_roc_filter <- rfe(
  pd_rec,
  data = pd_tr,
  sizes = sizes,
  rfeControl = rfe_ctrl,
  metric = "ROC",
  ntree = 10000
)

set.seed(184)
rf_roc_all <- rfe(
  Class ~ .,
  data = pd_tr,
  sizes = sizes,
  rfeControl = rfe_ctrl,
  metric = "ROC",
  ntree = 10000
)

# RFE with Random Forest Ranking -----------------------------------------------

imp_ctrl <- rfe_ctrl
imp_ctrl$functions$rank <- caret::rfFuncs$rank

set.seed(184)
rf_imp_rfe_filter <- rfe(
  pd_rec,
  data = pd_tr,
  sizes = sizes,
  rfeControl = imp_ctrl,
  metric = "ROC",
  ntree = 10000
)

set.seed(184)
rf_imp_rfe_all <- rfe(
  Class ~ .,
  data = pd_tr,
  sizes = sizes,
  rfeControl = imp_ctrl,
  metric = "ROC",
  ntree = 10000
)

rf_res <- 
  rf_roc_filter %>% 
  pluck("results") %>% 
  mutate(predictors = "Correlation Filter", Importance = "ROC") %>% 
  bind_rows(
    rf_roc_all %>% pluck("results") %>% mutate(predictors = "All Predictors", Importance = "ROC", Num_Resamples = 20),
    rf_imp_rfe_filter %>% pluck("results") %>% mutate(predictors = "Correlation Filter", Importance = "RF"),
    rf_imp_rfe_all %>% pluck("results") %>% mutate(predictors = "All Predictors", Importance = "RF", Num_Resamples = 20),
  ) %>% 
  dplyr::filter(Num_Resamples >= 10) 


# Random Subset Size Validation ------------------------------------------------

subset_size <- length(rf_roc_all$optVariables)
iter <- 100

vars <- 
  pd_tr %>% 
  dplyr::select(-Class) %>%
  names()

subset_ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 5,
  index = rf_roc_all$index,
  indexOut = rf_roc_all$indexOut,
  classProbs = TRUE,
  summaryFunction = many_stats
)

for (i in 1:iter) {
  set.seed(14 + i)
  rand_subset <- sample(vars, subset_size)

  subset_model <- train(
    Class ~ .,
    data = pd_tr[, c("Class", rand_subset)],
    method = "rf",
    tuneGrid = data.frame(mtry = floor(sqrt(subset_size))),
    trControl = subset_ctrl,
    metric = "ROC",
    ntree = 10000
  )
  subset_perf <- getTrainPerf(subset_model)
  if (i == 1) {
    rf_size_check <- subset_perf
  } else {
    rf_size_check <- bind_rows(rf_size_check, subset_perf)
  }
  rm(rand_subset, subset_model, subset_perf)
}

# ------------------------------------------------------------------------------
# Make the objects smaller by removing redundant fields and converting to factor

rf_imp_rfe_filter$variables <-
  rf_imp_rfe_filter$variables %>%
  mutate(var = as.factor(var),
         Resample = as.factor(Resample))

rf_imp_rfe_all$variables <-
  rf_imp_rfe_all$variables %>%
  mutate(var = as.factor(var),
         Resample = as.factor(Resample))

rf_roc_filter$variables <-
  rf_roc_filter$variables %>%
  mutate(var = as.factor(var),
         Resample = as.factor(Resample))

rf_roc_all$variables <-
  rf_roc_all$variables %>%
  mutate(var = as.factor(var),
         Resample = as.factor(Resample))

save(rf_imp_rfe_filter, rf_imp_rfe_all, rf_roc_filter, rf_roc_all, 
     rf_res, rf_size_check, file = "pd_rf_rfe.RData")

# ------------------------------------------------------------------------------

sessionInfo()

if (!interactive())
  q("no")

