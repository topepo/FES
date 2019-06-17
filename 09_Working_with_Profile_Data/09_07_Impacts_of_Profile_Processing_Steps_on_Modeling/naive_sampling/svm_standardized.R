library(caret)
library(recipes)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores() - 2)
registerDoParallel(cl)

# ------------------------------------------------------------------------------

load("../../../Data_Sets/Pharmaceutical_Manufacturing_Monitoring/standardized.RData")


# ------------------------------------------------------------------------------

rec <- 
  recipe(Glucose ~ ., data = small_standardized) %>% 
  update_role(Reactor, new_role = "ID") %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors()) %>%
  step_corr(all_predictors()) %>%
  step_nzv(all_predictors())

# ------------------------------------------------------------------------------

ctrl <- trainControl(method = "cv",
                     savePredictions = TRUE,
                     search = "random",
                     trim = TRUE)

# ------------------------------------------------------------------------------

set.seed(3523)
mod_fit <- 
  train(
    rec, 
    data = small_standardized,
    method = "svmRadial",
    tuneLength = 50,
    trControl = ctrl
  )

test_pred <- 
  large_standardized %>% 
  mutate(Predicted = predict(mod_fit, large_standardized)) %>% 
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

# ------------------------------------------------------------------------------

model <- "svm"
predictors <- "standardized"

save(mod_fit, test_pred, model, predictors, file = "svm_standardized.RData")

# ------------------------------------------------------------------------------

sessionInfo()

# ------------------------------------------------------------------------------

if (!interactive())
  q("no")

