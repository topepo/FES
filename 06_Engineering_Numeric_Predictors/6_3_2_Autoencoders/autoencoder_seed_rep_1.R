# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 6.3.2 at
# https://bookdown.org/max/FES/numeric-many-to-many.html#autoencoders
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(keras)
library(tidymodels)
library(caret)
library(QSARdata)
library(kknn)

theme_set(theme_bw())

# ------------------------------------------------------------------------------

# The memory requirements are roughly 600MG for the autoencoder calculations and
# the K-nearest neighbors fits require about 700MB.

# ------------------------------------------------------------------------------

data(MeltingPoint)

dat <- 
  MP_Descriptors %>% 
  mutate(mp = MP_Outcome)

# ------------------------------------------------------------------------------

# Split the data to create a test set
set.seed(1344)
split_1 <- initial_split(dat, p = 1 - (75/nrow(dat)))
split_1

unlabeled <- analysis(split_1)
labeled   <- assessment(split_1)

set.seed(164)
split_2 <- initial_split(labeled, p = 2/3)

training_raw <- analysis(split_2)
testing_raw  <- assessment(split_2)

# ------------------------------------------------------------------------------

pp <- 
  recipe(mp ~ ., data = unlabeled) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_predictors(), threshold = .75) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  prep(training = unlabeled)

unlabeled <- bake(pp, new_data = unlabeled, all_predictors(), composition = "matrix")
training <- bake(pp, new_data = training_raw, all_predictors(), composition = "matrix")
testing  <- bake(pp, new_data = testing_raw, all_predictors(), composition = "matrix")

# ------------------------------------------------------------------------------

use_session_with_seed(4477)

set.seed(1763)
final_model <- keras_model_sequential()
final_model %>%
  layer_dense(
    units = 512,
    activation = "tanh",
    input_shape = ncol(unlabeled),
    kernel_initializer = keras::initializer_glorot_uniform(seed = 3494)
  ) %>%
  layer_dense(
    units = 512,
    activation = "tanh",
    kernel_initializer = keras::initializer_glorot_uniform(seed = 3925)
  )  %>%
  layer_dense(
    units = ncol(unlabeled),
    activation = "linear",
    kernel_initializer = keras::initializer_glorot_uniform(seed = 3175)
  ) 

summary(final_model)

final_model %>% 
  compile(
    loss = "mean_squared_error", 
    optimizer = "adam"
  )

rehistory <- 
  final_model %>% 
  fit(
    x = unlabeled, 
    y = unlabeled, 
    epochs = 100, 
    batch_size = 100,
    verbose = 0,
    validation_split = 0.2
  )

# ------------------------------------------------------------------------------

pred_train <- predict(final_model, training)
pred_test  <- predict(final_model, testing)

train_data <- 
  pred_train %>% 
  as.data.frame() %>% 
  mutate(mp = training_raw$mp)

test_data <- 
  pred_test %>% 
  as.data.frame() %>% 
  mutate(mp = testing_raw$mp)

# ------------------------------------------------------------------------------

ctrl <- trainControl(method = "repeatedcv", repeats = 10)

knn_grid <- expand.grid(kmax = 1:15,
                        distance = 2,
                        kernel = c("rectangular"))

set.seed(633)
knn_orig <- train(mp ~ ., data = training_raw,
                  method = "kknn",
                  preProc = c("center", "scale", "zv"),
                  tuneGrid = knn_grid,
                  trControl = ctrl)

set.seed(633)
knn_auto <- train(mp ~ ., data = train_data,
                  method = "kknn",
                  preProc = c("center", "scale", "zv"),
                  tuneGrid = knn_grid,
                  trControl = ctrl)

# ------------------------------------------------------------------------------

getTrainPerf(knn_orig)
getTrainPerf(knn_auto)
compare_models(knn_orig, knn_auto)

# ------------------------------------------------------------------------------

sessionInfo()

q("no")






