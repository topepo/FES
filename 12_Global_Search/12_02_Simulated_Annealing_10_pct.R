# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 13.2 at
# https://bookdown.org/max/FES/global.html#simulated-annealing
#
# This file is for the SA search where the initial model has 10% of the features
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
  dplyr::select(-profile, -where_state) 

# Determine which columns have 2 values and are numeric
numero_two <- function(x) length(unique(x)) == 2 & is.numeric(x)

is_dummy <- map_lgl(okc_train, numero_two)
dummies <- names(is_dummy)[is_dummy]

okc_rec <- 
  recipe(Class ~ ., data = okc_train) %>%
  step_bin2factor(!!! dummies) %>%
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
sa_funcs$initial <- function(vars, prob = 0.10, ...) 
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
sim_anneal_10_pct <- safs(
  okc_rec,
  data = okc_train,
  iters = 500,
  safsControl = sa_ctrl,
  method = "nb",
  tuneGrid = nb_grid,
  trControl = ctrl_rs,
  metric = "ROC"
)

# ------------------------------------------------------------------------------
# Remove some objects to make the file size smaller

sim_anneal_10_pct$fit$control$index <- NULL
sim_anneal_10_pct$fit$control$indexOut <- NULL
sim_anneal_10_pct$fit$finalModel$x <- NULL
sim_anneal_10_pct$sa$fit$trainingData <- NULL
sim_anneal_10_pct$sa$fit$control$index <- NULL
sim_anneal_10_pct$sa$fit$control$indexOut <- NULL
sim_anneal_10_pct$sa$fit$finalModel$x <- NULL
sim_anneal_10_pct$control$index <- NULL
sim_anneal_10_pct$control$indexOut <- NULL
sim_anneal_10_pct$recipe$template <- NULL
attr(sim_anneal_10_pct$recipe$steps[[2]]$terms[[1]], ".Environment") <- emptyenv()

# ------------------------------------------------------------------------------

save(sim_anneal_10_pct, file = "sim_anneal_10_pct.RData")

# ------------------------------------------------------------------------------

stopCluster(cl)

if (!interactive())
  q("no")
