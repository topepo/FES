# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 13.2 at
# https://bookdown.org/max/FES/global.html#simulated-annealing
#
# This file is for the SA search where the initial model has 50% of the features
# when the categorical predictors are encoded as dummy variables. 
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(caret)
library(tidymodels)
library(doParallel)
cl <- makeForkCluster(nnodes = parallel::detectCores() - 1)
registerDoParallel(cl)

# ------------------------------------------------------------------------------

load("../Data_Sets/OkCupid/okc.RData")
load("../Data_Sets/OkCupid/okc_other.RData")
load("../Data_Sets/OkCupid/okc_binary.RData")
load("../Data_Sets/OkCupid/okc_features.RData")

# ------------------------------------------------------------------------------

okc_train <-
  okc_train %>%
  full_join(okc_train_binary, by = "profile") %>%
  full_join(basic_features_train, by = "profile") %>%
  arrange(profile) %>%
  select(-profile, -where_state)

# Determine which columns have 2 values and are numeric
numero_two <- function(x) length(unique(x)) == 2 & is.numeric(x)

is_dummy <- map_lgl(okc_train, numero_two)
dummies <- names(is_dummy)[is_dummy]

okc_rec <-
  recipe(Class ~ ., data = okc_train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_bin2factor(!!! dummies) %>%
  step_bin2factor(starts_with("body_type"), starts_with("diet"),
                  starts_with("drinks"), starts_with("drugs"),
                  starts_with("education"), starts_with("income"),
                  starts_with("offspring"), starts_with("orientation"),
                  starts_with("pets"), starts_with("religion"), starts_with("sign"),
                  starts_with("smokes"), starts_with("status"),
                  starts_with("where_town"), starts_with("religion_modifer"),
                  starts_with("sign_modifer")) %>%
  step_zv(all_predictors())


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

# ------------------------------------------------------------------------------

sa_funcs <- caretSA
sa_funcs$fitness_extern <- many_stats
sa_funcs$initial <- function(vars, prob = 0.50, ...)
  sort(sample.int(vars, size = floor(vars * prob) + 1))

nb_grid <- data.frame(usekernel = TRUE, fL = 0, adjust = 1)

# Inner control for each model, use a random 10% validation set
ctrl_rs <- trainControl(
  method = "LGOCV",
  p = 0.90,
  number = 1,
  summaryFunction = many_stats,
  classProbs = TRUE,
  allowParallel = FALSE
)

# Outer control for SA
sa_ctrl <- safsControl(
  method = "cv",
  metric = c(internal = "ROC", external = "ROC"),
  maximize = c(internal = TRUE, external = TRUE),
  functions = sa_funcs,
  improve = 10,
  returnResamp = "all",
  verbose = TRUE
)

options(digits = 3)

# Run simulated annealing
set.seed(325)
sim_anneal_50_pct_bin <- safs(
  okc_rec,
  data = okc_train,
  iters = 1000,
  safsControl = sa_ctrl,
  method = "nb",
  tuneGrid = nb_grid,
  trControl = ctrl_rs,
  metric = "ROC"
)

# ------------------------------------------------------------------------------
# Remove some objects to make the file size smaller

sim_anneal_50_pct_bin$fit$control$index <- NULL
sim_anneal_50_pct_bin$fit$control$indexOut <- NULL
sim_anneal_50_pct_bin$fit$finalModel$x <- NULL
sim_anneal_50_pct_bin$fit$trainingData <- NULL
sim_anneal_50_pct_bin$sa$fit$trainingData <- NULL
sim_anneal_50_pct_bin$sa$fit$control$index <- NULL
sim_anneal_50_pct_bin$sa$fit$control$indexOut <- NULL
sim_anneal_50_pct_bin$sa$fit$finalModel$x <- NULL
sim_anneal_50_pct_bin$control$index <- NULL
sim_anneal_50_pct_bin$control$indexOut <- NULL
sim_anneal_50_pct_bin$recipe$template <- NULL
sim_anneal_50_pct_bin$recipe$template <- NULL
n_steps <- length(sim_anneal_50_pct_bin$recipe$steps)
for (i in 1:n_steps) {
  n_terms <- length(sim_anneal_50_pct_bin$recipe$steps[[i]]$terms)
  for (j in 1:n_terms)
    attr(sim_anneal_50_pct_bin$recipe$steps[[i]]$terms[[j]], ".Environment") <- emptyenv()
}
n_vars <- length(sim_anneal_50_pct_bin$recipe$steps[[1]]$levels)
for (i in 1:n_vars) {
  attr(sim_anneal_50_pct_bin$recipe$steps[[1]]$levels[[i]], ".Environment") <- emptyenv()
}

# ------------------------------------------------------------------------------

save(sim_anneal_50_pct_bin, file = "sim_anneal_50_pct_bin.RData")

# ------------------------------------------------------------------------------

stopCluster(cl)

if (!interactive())
  q("no")
