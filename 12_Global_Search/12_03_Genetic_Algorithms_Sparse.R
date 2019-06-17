# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 13.3 at
# https://bookdown.org/max/FES/global.html#genetic-algorithms
#
# This file is the analysis to encourage sparsity
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(caret)
library(tidymodels)
library(desirability)
library(doParallel)
cl <- makeForkCluster(nnodes = parallel::detectCores() - 1)
registerDoParallel(cl)

# ------------------------------------------------------------------------------

load("../Data_Sets/OkCupid/okc.RData")
load("../Data_Sets/OkCupid/okc_other.RData")
load("../Data_Sets/OkCupid/okc_binary.RData")
load("../Data_Sets/OkCupid/okc_features.RData")

# ------------------------------------------------------------------------------

okc_train <- okc_train %>%
  full_join(okc_train_binary) %>%
  full_join(basic_features_train) %>%
  arrange(profile) %>%
  select(-profile, -where_state) 

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

ga_funcs <- caretGA
ga_funcs$fitness_intern <- function(object, x, y, maximize, p) {
  perf_val <- getTrainPerf(object)
  
  d_roc  <- dMax(0.5, 1, scale = 2)
  d_vars <- dMin(10, 100)
  d_both <- dOverall(d_roc, d_vars)
  d_data <- data.frame(
    roc = perf_val[1, "TrainROC"], 
    p = length(object$finalModel$xNames)
  )
  perf_val <- getTrainPerf(object)
  perf_val$D <- predict(d_both, d_data)
  
  perf_val <- perf_val[names(perf_val) != "method"]
  perf_val <- unlist(perf_val)
  names(perf_val) <- gsub("Train", "", names(perf_val))
  perf_val
}

ga_funcs$fitness_extern <- many_stats

ga_funcs$initial <- function(vars, popSize, ...)  {
  x <- matrix(NA, nrow = popSize, ncol = vars)
  probs <- seq(0.1, 0.90, length = popSize)
  for (i in 1:popSize) {
    x[i, ] <- 
      sample(0:1, replace = TRUE, size = vars, prob = c(probs[i], 1 - probs[i]))
  }
  var_count <- apply(x, 1, sum)
  if (any(var_count == 0)) {
    for (i in which(var_count == 0)) {
      p <- sample(1:length(vars), size = 2)
      x[i, p] <- 1
    }
  }
  x
}

ctrl_rs <- trainControl(
  method = "LGOCV", 
  p = 0.90,
  number = 1,
  summaryFunction = many_stats, 
  classProbs = TRUE,
  allowParallel = FALSE
)

ga_ctrl <- gafsControl(
  method = "cv",
  metric = c(internal = "D", external = "ROC"),
  maximize = c(internal = TRUE, external = TRUE), 
  functions = ga_funcs, 
  returnResamp = "all",
  verbose = TRUE
)

options(digits = 3)

numero_two <- function(x) length(unique(x)) == 2 & is.numeric(x)

is_dummy <- map_lgl(okc_train, numero_two)
dummies <- names(is_dummy)[is_dummy]

okc_rec <- 
  recipe(Class ~ ., data = okc_train) %>%
  step_bin2factor(!!! dummies) %>%
  step_zv(all_predictors())

nb_grid <- data.frame(usekernel = TRUE, fL = 0, adjust = 1)

# Conduct the search -----------------------------------------------------------

set.seed(325)
gen_algo_sparse <- gafs(
  okc_rec,
  data = okc_train,
  iters = 15,
  gafsControl = ga_ctrl,
  method = "nb",
  tuneGrid = nb_grid,
  trControl = ctrl_rs,
  metric = "ROC"
)

# ------------------------------------------------------------------------------
# Remove some objects to make the file size smaller

gen_algo_sparse$fit$control$index <- NULL
gen_algo_sparse$fit$control$indexOut <- NULL
gen_algo_sparse$fit$finalModel$x <- NULL
gen_algo_sparse$ga$fit$trainingData <- NULL
gen_algo_sparse$ga$fit$control$index <- NULL
gen_algo_sparse$ga$fit$control$indexOut <- NULL
gen_algo_sparse$ga$fit$finalModel$x <- NULL
gen_algo_sparse$control$index <- NULL
gen_algo_sparse$control$indexOut <- NULL
gen_algo_sparse$recipe$template <- NULL
attr(gen_algo_sparse$recipe$steps[[2]]$terms[[1]], ".Environment") <- emptyenv()

# ------------------------------------------------------------------------------

save(gen_algo_sparse, file = "gen_algo_sparse.RData")

# ------------------------------------------------------------------------------

if (!interactive())
  q("no")
