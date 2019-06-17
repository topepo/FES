library(caret)
library(recipes)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores() - 2)
registerDoParallel(cl)

# ------------------------------------------------------------------------------

load("../../../Data_Sets/Pharmaceutical_Manufacturing_Monitoring/baseline_corr.RData")


# ------------------------------------------------------------------------------

rec <- 
  recipe(Glucose ~ ., data = small_baseline) %>% 
  update_role(Reactor, new_role = "ID") %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors()) 

# ------------------------------------------------------------------------------

ctrl <- trainControl(method = "cv",
                     savePredictions = TRUE,
                     trim = TRUE)

# ------------------------------------------------------------------------------

cubist_grid <- expand.grid(committees = c(1:9, (1:10)*10),
                           neighbors = c(0, 1, 3, 5, 7, 9))

set.seed(3523)
mod_fit <- 
  train(
    rec, 
    data = small_baseline,
    method = "cubist",
    tuneGrid = cubist_grid,
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

# ------------------------------------------------------------------------------

model <- "cubist"
predictors <- "baseline"

save(mod_fit, test_pred, model, predictors, file = "cubist_baseline.RData")

# ------------------------------------------------------------------------------

sessionInfo()

# ------------------------------------------------------------------------------

if (!interactive())
  q("no")

