library(caret)
library(recipes)

library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores() - 2)
registerDoParallel(cl)

# ------------------------------------------------------------------------------

load("../../../Data_Sets/Pharmaceutical_Manufacturing_Monitoring/baseline_corr.RData")
load("../../../Data_Sets/Pharmaceutical_Manufacturing_Monitoring/resample_ind.RData")

# ------------------------------------------------------------------------------

rec <- 
  recipe(Glucose ~ ., data = small_baseline) %>% 
  update_role(Reactor, new_role = "ID") %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())

# ------------------------------------------------------------------------------

ctrl <- trainControl(method = "cv", index = resample_ind, savePredictions = TRUE)

# ------------------------------------------------------------------------------

set.seed(3523)
mod_fit <- 
  train(
    rec, 
    data = small_baseline,
    method = "widekernelpls",
    tuneGrid = data.frame(ncomp = 1:25),
    trControl = ctrl
  )

test_pred <- 
  large_baseline %>% 
  mutate(Predicted = predict(mod_fit, large_baseline)) %>% 
  dplyr::select(Predicted, Glucose, Reactor, Day)

# Trim model objects to make smaller -------------------------------------------

mod_fit$recipe$template <- NULL
n_steps <- length(mod_fit$recipe$steps)
for (i in 1:n_steps) {
  n_terms <- length(mod_fit$recipe$steps[[i]]$terms)
  for (j in 1:n_terms)
    attr(mod_fit$recipe$steps[[i]]$terms[[j]], ".Environment") <- emptyenv()
}
mod_fit$control$index <- NULL
mod_fit$control$indexOut <- NULL
attr(mod_fit$finalModel$terms, ".Environment") <- emptyenv()
mod_fit$finalModel$model <- NULL

# ------------------------------------------------------------------------------

model <- "PLS"
predictors <- "baseline"

save(mod_fit, test_pred, model, predictors, file = "pls_baseline.RData")

# ------------------------------------------------------------------------------

sessionInfo()

# ------------------------------------------------------------------------------

if (!interactive())
  q("no")

