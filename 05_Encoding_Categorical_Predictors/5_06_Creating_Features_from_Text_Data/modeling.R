# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 5.6 at
# https://bookdown.org/max/FES/encoding-categorical-predictors.html#text-data
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(caret)
library(tidymodels)
library(keras)
library(doParallel)

# ------------------------------------------------------------------------------

# During modeling using keras, the estimated memory usage in the main R session
# peaked at 1.8 GB. When parallel processing is used for glm and knn models, 
# each worker process also required about 1.8 GB. 

# ------------------------------------------------------------------------------

load("../../Data_Sets/OkCupid/okc.RData")
load("../../Data_Sets/OkCupid/okc_other.RData")
load("../../Data_Sets/OkCupid/okc_binary.RData")
load("../../Data_Sets/OkCupid/okc_features.RData")

# ------------------------------------------------------------------------------

okc_train <-
  okc_train %>%
  full_join(okc_train_binary) %>%
  full_join(basic_features_train) %>%
  arrange(profile) %>%
  dplyr::select(-profile)

# create feature sets

basic_feat <- names(okc_test)
basic_feat <- basic_feat[!(basic_feat %in% c("Class", "profile", "essay_length", "where_state"))]

# keywords only

keyword_feat <- names(okc_train_binary)
keyword_feat <- keyword_feat[keyword_feat != "profile"]

text_feat <-
  c("n_urls", "n_hashtags", "n_mentions", "n_commas", "n_digits",
    "n_exclaims", "n_extraspaces", "n_lowers", "n_lowersp", "n_periods",
    "n_words", "n_puncts", "n_charsperword")

sent_feat <-
  c("sent_afinn", "sent_bing", "n_polite", "n_first_person", "n_first_personp",
    "n_second_person", "n_second_personp", "n_third_person", "n_prepositions")

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

okc_ctrl <- trainControl(
  method = "cv",
  classProbs = TRUE,
  summaryFunction = many_stats,
  returnData = FALSE,
  trim = TRUE,
  sampling = "down"
)

# ------------------------------------------------------------------------------
## First run a tensorflow model (since it cannot be run in parallel
## via foreach)

## Recipe for the keywords that normalizes the predictors at the end

keyword_norm <-
  recipe(Class ~ .,
         data = okc_train[, c("Class", basic_feat, keyword_feat)]) %>%
  step_YeoJohnson(all_numeric()) %>%
  step_other(where_town) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

okc_ctrl_rand <- okc_ctrl
okc_ctrl_rand$search <- "random"

set.seed(49)
mlp_keyword <-
  train(
    keyword_norm,
    data = okc_train,
    method = "mlpKerasDropout",
    metric = "ROC",
    tuneLength = 20,
    trControl = okc_ctrl_rand,
    verbose = 0,
    epochs = 500
  )

# Clean out some objects that are not needed to reduce file size
mlp_keyword$recipe$template <- NULL
n_steps <- length(mlp_keyword$recipe$steps)
for (i in 1:n_steps) {
  n_terms <- length(mlp_keyword$recipe$steps[[i]]$terms)
  for (j in 1:n_terms)
    attr(mlp_keyword$recipe$steps[[i]]$terms[[j]], ".Environment") <- emptyenv()
}
n_vars <- length(mlp_keyword$recipe$steps[[3]]$levels)
for (i in 1:n_vars) {
  attr(mlp_keyword$recipe$steps[[3]]$levels[[i]], ".Environment") <- emptyenv()
}
mlp_keyword$control$index <- NULL
mlp_keyword$control$indexOut <- NULL


# For reasons related to tensorflow, the mlp model may not be completely reproducible.
# Previous runs of the model selected the same optimal parameters but performance
# was slightly different:
#
# Run  TrainAccuracy TrainKappa  TrainROC
# 1        0.7752847  0.4123751 0.8444257
# 2        0.7785812  0.4154827 0.8442332
# 3        0.7732222  0.4118858 0.8447137

# ------------------------------------------------------------------------------

library(doParallel)
cl <- makeForkCluster(nnodes = parallel::detectCores() - 1)
registerDoParallel(cl)

# ------------------------------------------------------------------------------
# Model with basic profile information. Many count variables so
# we estimate possible transfomrations (e.g. sqrt) that might help.

basic_rec <-
  recipe(Class ~ ., data = okc_train[, c("Class", basic_feat)]) %>%
  step_YeoJohnson(all_numeric()) %>%
  step_other(where_town) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors())

set.seed(49)
glm_basic <- train(
  basic_rec,
  data = okc_train,
  method = "glm",
  metric = "ROC",
  trControl = okc_ctrl
)

basic_pred <- ncol(glm_basic$recipe$template) - 1

# Clean out some objects that are not needed to reduce file size
glm_basic$recipe$template <- NULL
n_steps <- length(glm_basic$recipe$steps)
for (i in 1:n_steps) {
  n_terms <- length(glm_basic$recipe$steps[[i]]$terms)
  for (j in 1:n_terms)
    attr(glm_basic$recipe$steps[[i]]$terms[[j]], ".Environment") <- emptyenv()
}
n_vars <- length(glm_basic$recipe$steps[[3]]$levels)
for (i in 1:n_vars) {
  attr(glm_basic$recipe$steps[[3]]$levels[[i]], ".Environment") <- emptyenv()
}
glm_basic$control$index <- NULL
glm_basic$control$indexOut <- NULL


# ------------------------------------------------------------------------------
## Now add basic text features

text_rec <-
  recipe(Class ~ .,
         data = okc_train[, c("Class", basic_feat, text_feat)]) %>%
  step_YeoJohnson(all_numeric()) %>%
  step_other(where_town) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors())

set.seed(49)
glm_text <- train(
  text_rec,
  data = okc_train,
  method = "glm",
  metric = "ROC",
  trControl = okc_ctrl
)

text_pred <- ncol(glm_text$recipe$template) - 1

# Clean out some objects that are not needed to reduce file size
glm_text$recipe$template <- NULL
n_steps <- length(glm_text$recipe$steps)
for (i in 1:n_steps) {
  n_terms <- length(glm_text$recipe$steps[[i]]$terms)
  for (j in 1:n_terms)
    attr(glm_text$recipe$steps[[i]]$terms[[j]], ".Environment") <- emptyenv()
}
n_vars <- length(glm_text$recipe$steps[[3]]$levels)
for (i in 1:n_vars) {
  attr(glm_text$recipe$steps[[3]]$levels[[i]], ".Environment") <- emptyenv()
}
glm_text$control$index <- NULL
glm_text$control$indexOut <- NULL


# ------------------------------------------------------------------------------
## Add the derived keyword fields

keyword_rec <-
  recipe(Class ~ .,
         data = okc_train[, c("Class", basic_feat, keyword_feat)]) %>%
  step_YeoJohnson(all_numeric()) %>%
  step_other(where_town) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors())

okc_ctrl_keep <- okc_ctrl
okc_ctrl_keep$savePredictions <- "final"

set.seed(49)
glm_keyword <- train(
  keyword_rec,
  data = okc_train,
  method = "glm",
  metric = "ROC",
  trControl = okc_ctrl_keep
)

keyword_pred <- ncol(glm_keyword$recipe$template) - 1

# Clean out some objects that are not needed to reduce file size
glm_keyword$recipe$template <- NULL
n_steps <- length(glm_keyword$recipe$steps)
for (i in 1:n_steps) {
  n_terms <- length(glm_keyword$recipe$steps[[i]]$terms)
  for (j in 1:n_terms)
    attr(glm_keyword$recipe$steps[[i]]$terms[[j]], ".Environment") <- emptyenv()
}
n_vars <- length(glm_keyword$recipe$steps[[3]]$levels)
for (i in 1:n_vars) {
  attr(glm_keyword$recipe$steps[[3]]$levels[[i]], ".Environment") <- emptyenv()
}
glm_keyword$control$index <- NULL
glm_keyword$control$indexOut <- NULL

# ------------------------------------------------------------------------------
## and the sentiment analysis

sent_rec <-
  recipe(Class ~ .,
         data = okc_train[, c("Class", basic_feat, keyword_feat, sent_feat)]) %>%
  step_YeoJohnson(all_numeric()) %>%
  step_other(where_town) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors())

set.seed(49)
glm_sent <- train(
  sent_rec,
  data = okc_train,
  method = "glm",
  metric = "ROC",
  trControl = okc_ctrl
)

sent_pred <- ncol(glm_sent$recipe$template) - 1

# Clean out some objects that are not needed to reduce file size
glm_sent$recipe$template <- NULL
n_steps <- length(glm_sent$recipe$steps)
for (i in 1:n_steps) {
  n_terms <- length(glm_sent$recipe$steps[[i]]$terms)
  for (j in 1:n_terms)
    attr(glm_sent$recipe$steps[[i]]$terms[[j]], ".Environment") <- emptyenv()
}
n_vars <- length(glm_sent$recipe$steps[[3]]$levels)
for (i in 1:n_vars) {
  attr(glm_sent$recipe$steps[[3]]$levels[[i]], ".Environment") <- emptyenv()
}
glm_sent$control$index <- NULL
glm_sent$control$indexOut <- NULL

# ------------------------------------------------------------------------------
## collect the models and visualize the results

feat_groups <-
  c("Basic Profile Info", "+ Simple Text Features", "+ Keywords", "+ Keywords\n+ Sentiment")

glm_resamples <-
  glm_basic$resample %>%
  mutate(Features = "Basic Profile Info") %>%
  bind_rows(
    glm_text$resample %>%
      mutate(Features = "+ Simple Text Features"),
    glm_keyword$resample %>%
      mutate(Features = "+ Keywords"),
    glm_sent$resample %>%
      mutate(Features = "+ Keywords\n+ Sentiment")
  ) %>%
  mutate(Features = factor(Features, levels = feat_groups))

glm_resamples_mean <-
  glm_resamples %>%
  group_by(Features) %>%
  summarize(ROC = mean(ROC))

# ------------------------------------------------------------------------------
## K-NN

okc_ctrl_rs <- okc_ctrl
okc_ctrl_rs$returnResamp <- "all"

set.seed(49)
knn_keyword <-
  train(
    keyword_norm,
    data = okc_train,
    method = "knn",
    metric = "ROC",
    tuneGrid = data.frame(k = seq(1, 201, by = 4)),
    trControl = okc_ctrl_rs
  )

# Clean out some objects that are not needed to reduce file size
knn_keyword$recipe$template <- NULL
n_steps <- length(knn_keyword$recipe$steps)
for (i in 1:n_steps) {
  n_terms <- length(knn_keyword$recipe$steps[[i]]$terms)
  for (j in 1:n_terms)
    attr(knn_keyword$recipe$steps[[i]]$terms[[j]], ".Environment") <- emptyenv()
}
n_vars <- length(knn_keyword$recipe$steps[[3]]$levels)
for (i in 1:n_vars) {
  attr(knn_keyword$recipe$steps[[3]]$levels[[i]], ".Environment") <- emptyenv()
}
knn_keyword$control$index <- NULL
knn_keyword$control$indexOut <- NULL


# ------------------------------------------------------------------------------
# Compare two models

okc_dff <- compare_models(mlp_keyword, glm_keyword)

# ------------------------------------------------------------------------------

save(glm_keyword, file = "okc_glm_keyword.RData")
save(knn_keyword, file = "okc_knn_keyword.RData")
save(mlp_keyword, file = "okc_mlp_keyword.RData")
save(glm_basic, file = "okc_glm_basic.RData")
save(glm_text, file = "okc_glm_text.RData")
save(glm_sent, file = "okc_glm_sent.RData")
save(glm_resamples, glm_resamples_mean, basic_pred, text_pred,
     keyword_pred, sent_pred,
     file = "okc_glm_rs.RData")
save(okc_dff, file = "okc_dff.RData")

# ------------------------------------------------------------------------------

sessionInfo()

# ------------------------------------------------------------------------------

if (!interactive())
  q("no")



