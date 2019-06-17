# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 12.3 at
# https://bookdown.org/max/FES/global.html#genetic-algorithms
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
  metric = c(internal = "ROC", external = "ROC"),
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

set.seed(325)
gen_algo <- gafs(
  okc_rec,
  data = okc_train,
  iters = 15,
  gafsControl = ga_ctrl,
  method = "nb",
  tuneGrid = nb_grid,
  trControl = ctrl_rs,
  metric = "ROC"
)

# Random Subset Size Validation ------------------------------------------------

ga_subset_size <- length(gen_algo$optVariables)
iter <- 100
vars <- names(okc_train)
vars <- vars[vars != "Class"]

subset_ctrl <- trainControl(
  method = "cv",
  classProbs = TRUE,
  summaryFunction = many_stats
)

for (i in 1:iter) {
  set.seed(7883 + i)
  rand_subset <- sample(vars, ga_subset_size)
  subset_dat <- okc_train %>% dplyr::select(Class, !!!rand_subset)
  
  is_dummy <- map_lgl(subset_dat, numero_two)
  dummies <- names(is_dummy)[is_dummy]
  
  subset_rec <- 
    recipe(Class ~ ., data = subset_dat) %>%
    step_bin2factor(!!! dummies) %>%
    step_zv(all_predictors())
  
  set.seed(325)
  subset_model <- train(
    subset_rec,
    data = subset_dat,
    method = "nb",
    tuneGrid = nb_grid,
    trControl = subset_ctrl,
    metric = "ROC"
  )
  subset_perf <- getTrainPerf(subset_model)
  
  if (i == 1) {
    ga_size_check <- subset_perf
  } else {
    ga_size_check <- bind_rows(ga_size_check, subset_perf)
  }
  rm(rand_subset, subset_rec, subset_model, subset_perf, is_dummy, dummies, subset_dat)
}

# ------------------------------------------------------------------------------
# Remove some objects to make the file size smaller

gen_algo$fit$control$index <- NULL
gen_algo$fit$control$indexOut <- NULL
gen_algo$fit$finalModel$x <- NULL
gen_algo$ga$fit$trainingData <- NULL
gen_algo$ga$fit$control$index <- NULL
gen_algo$ga$fit$control$indexOut <- NULL
gen_algo$ga$fit$finalModel$x <- NULL
gen_algo$control$index <- NULL
gen_algo$control$indexOut <- NULL
gen_algo$recipe$template <- NULL
attr(gen_algo$recipe$steps[[2]]$terms[[1]], ".Environment") <- emptyenv()

# ------------------------------------------------------------------------------

save(gen_algo, ga_size_check, file = "gen_algo.RData")

# ------------------------------------------------------------------------------

if (!interactive())
  q("no")
