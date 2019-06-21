# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 2.4 at
# https://bookdown.org/max/FES/predictive-modeling-across-sets.html
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(caret)
library(tidymodels)
library(utils)
library(pROC)
library(plotly)

theme_set(theme_bw())

# ------------------------------------------------------------------------------

load("../Data_Sets/Ischemic_Stroke/stroke_data.RData")
load("interactions.RData")

VC_preds <- 
  c("CALCVol", "CALCVolProp", "MATXVol", "MATXVolProp", "LRNCVol", 
    "LRNCVolProp", "MaxCALCArea", "MaxCALCAreaProp", "MaxDilationByArea", 
    "MaxMATXArea", "MaxMATXAreaProp", "MaxLRNCArea", "MaxLRNCAreaProp", 
    "MaxMaxWallThickness", "MaxRemodelingRatio", "MaxStenosisByArea", 
    "MaxWallArea", "WallVol", "MaxStenosisByDiameter")

risk_preds <-
  c("age", "sex", "SmokingHistory", "AtrialFibrillation", "CoronaryArteryDisease", 
    "DiabetesHistory", "HypercholesterolemiaHistory", "HypertensionHistory")

risk_train <- 
  stroke_train %>%
  dplyr::select(one_of(risk_preds), Stroke)

image_train <- 
  stroke_train %>%
  dplyr::select(one_of(VC_preds), Stroke)

# ------------------------------------------------------------------------------

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
internal_ctrl = trainControl(method = "none", classProbs = TRUE,
                             allowParallel = FALSE)
lrFuncsNew <- caretFuncs  
lrFuncsNew$summary <- fiveStats
rfeCtrl <- rfeControl(functions = lrFuncsNew,
                      method = "repeatedcv",
                      repeats = 5,
                      rerank = FALSE,
                      returnResamp = "all",
                      saveDetails = TRUE,
                      verbose = FALSE)

# ------------------------------------------------------------------------------
# Here I'll run in parallel using doMC. For Windows, a different package can
# be used.

library(doMC)
registerDoMC(cores = parallel::detectCores(logical = FALSE) - 1)

# ------------------------------------------------------------------------------
# RFE procedure using risk predictors

# All pair-wise interactions
risk_int_filtered_recipe <-
  recipe(Stroke ~ ., data = risk_train) %>%
  step_interact(~ all_predictors():all_predictors()) %>%
  step_corr(all_predictors(), threshold = 0.75) %>% 
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
risk_int_filtered_rfe <- rfe(
  risk_int_filtered_recipe,
  data = risk_train,
  sizes = 1:36,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

# Main effects
risk_main_filtered_recipe <-
  recipe(Stroke ~ ., data = risk_train) %>%
  step_corr(all_predictors(), threshold = 0.75) %>% 
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
risk_main_filtered_rfe <- rfe(
  risk_main_filtered_recipe,
  data = risk_train,
  sizes = 1:8,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

# ------------------------------------------------------------------------------
# RFE procedure using imaging predictors

img_int_filtered_recipe <-
  recipe(Stroke ~ ., data = image_train) %>%
  step_interact(int_form)  %>%
  step_corr(all_predictors(), threshold = 0.75) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
img_int_filtered_rfe <- rfe(
  img_int_filtered_recipe,
  data = image_train,
  sizes = 1:35,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

img_main_filtered_recipe <-
  recipe(Stroke ~ ., data = image_train)  %>%
  step_corr(all_predictors(), threshold = 0.75) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
img_main_filtered_rfe <- rfe(
  img_main_filtered_recipe,
  data = image_train,
  sizes = 1:19,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

# ------------------------------------------------------------------------------

both_int_filtered_recipe <-
  recipe(Stroke ~ ., data = stroke_train) %>%
  step_interact(int_form)  %>%
  step_corr(all_predictors(), threshold = 0.75) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
both_int_filtered_rfe <- rfe(
  both_int_filtered_recipe,
  data = stroke_train,
  sizes = 1:44,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

both_main_filtered_recipe <-
  recipe(Stroke ~ ., data = stroke_train)  %>%
  step_corr(all_predictors(), threshold = 0.75) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
both_main_filtered_rfe <- rfe(
  both_main_filtered_recipe,
  data = stroke_train,
  sizes = 1:28,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

# ------------------------------------------------------------------------------

risk_int_recipe <-
  recipe(Stroke ~ ., data = risk_train) %>%
  step_interact(~ all_predictors():all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
risk_int_rfe <- rfe(
  risk_int_recipe,
  data = risk_train,
  sizes = 1:36,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

risk_main_recipe <-
  recipe(Stroke ~ ., data = risk_train) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
risk_main_rfe <- rfe(
  risk_main_recipe,
  data = risk_train,
  sizes = 1:8,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

# ------------------------------------------------------------------------------

img_int_recipe <-
  recipe(Stroke ~ ., data = image_train) %>%
  step_interact(int_form)  %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
img_int_rfe <- rfe(
  img_int_recipe,
  data = image_train,
  sizes = 1:35,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

img_main_recipe <-
  recipe(Stroke ~ ., data = image_train)  %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
img_main_rfe <- rfe(
  img_main_recipe,
  data = image_train,
  sizes = 1:19,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

# ------------------------------------------------------------------------------

both_int_recipe <-
  recipe(Stroke ~ ., data = stroke_train) %>%
  step_interact(int_form)  %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
both_int_rfe <- rfe(
  both_int_recipe,
  data = stroke_train,
  sizes = 1:44,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

both_main_recipe <-
  recipe(Stroke ~ ., data = stroke_train)  %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
both_main_rfe <- rfe(
  both_main_recipe,
  data = stroke_train,
  sizes = 1:28,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

# ------------------------------------------------------------------------------

format_data <- function(x, lab, int = FALSE) {
  dat <- 
    x %>% 
    pluck("results") %>% 
    mutate(Predictors = !!lab) %>% 
    dplyr::select(ROC, Variables, Predictors, Variables, Num_Resamples) %>% 
    mutate(Model = "Main Effects")
  if (int)
    dat$Model <- "Interactions"
  dat
  
}

filtered_dat <- 
  format_data(risk_main_filtered_rfe, lab = "Risk Predictors") %>% 
  bind_rows(
    format_data(risk_int_filtered_rfe, lab = "Risk Predictors", TRUE),
    format_data(img_main_filtered_rfe, lab = "Imaging Predictors"),
    format_data(img_int_filtered_rfe, lab = "Imaging Predictors", TRUE),
    format_data(both_main_filtered_rfe, lab = "All Predictors"),
    format_data(both_int_filtered_rfe, lab = "All Predictors", TRUE)
  ) %>% 
  mutate(
    Predictors = factor(
      Predictors, 
      levels = c("Risk Predictors", "Imaging Predictors", "All Predictors")
    ),
    Model = factor(Model, levels = c("Main Effects", "Interactions")),
    Filtering = "Correlation Filter"
  )

unfiltered_dat <- 
  format_data(risk_main_rfe, lab = "Risk Predictors") %>% 
  bind_rows(
    format_data(risk_int_rfe, lab = "Risk Predictors", TRUE),
    format_data(img_main_rfe, lab = "Imaging Predictors"),
    format_data(img_int_rfe, lab = "Imaging Predictors", TRUE),
    format_data(both_main_rfe, lab = "All Predictors"),
    format_data(both_int_rfe, lab = "All Predictors", TRUE)
  ) %>% 
  mutate(
    Predictors = factor(
      Predictors, 
      levels = c("Risk Predictors", "Imaging Predictors", "All Predictors")
    ),
    Model = factor(Model, levels = c("Main Effects", "Interactions")),
    Filtering = "No Filter"
  )

rfe_data <- 
  bind_rows(filtered_dat, unfiltered_dat) %>% 
  mutate(
    Filtering = factor(Filtering, levels = c("No Filter", "Correlation Filter"))
  )

# ------------------------------------------------------------------------------

# https://bookdown.org/max/FES/predictive-modeling-across-sets.html#fig:stroke-rfe-res
ggplot(rfe_data, aes(x = Variables, y = ROC, col = Model)) +
  geom_point(size = 0.75) + 
  geom_line() + 
  facet_grid(Filtering ~ Predictors) + 
  scale_color_manual(values = c("#6A3D9A", "#CAB2D6"))

# ------------------------------------------------------------------------------

# https://bookdown.org/max/FES/predictive-modeling-across-sets.html#tab:stroke-rfe-tab
rfe_tab <-
  img_main_filtered_rfe %>% 
  pluck("variables") %>% 
  filter(Variables == img_main_filtered_rfe$optsize) %>% 
  group_by(var) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate(final = ifelse(var %in% img_main_filtered_rfe$optVariables, "Yes", "No")) %>% 
  ungroup()

# ------------------------------------------------------------------------------

save(both_int_filtered_rfe, both_int_rfe, both_main_filtered_rfe, both_main_rfe, 
     img_int_filtered_rfe, img_int_rfe, img_main_filtered_rfe, img_main_rfe, 
     risk_int_filtered_rfe, risk_int_rfe, risk_main_filtered_rfe, risk_main_rfe, 
     file = "stroke_rfe.RData")

# ------------------------------------------------------------------------------

sessionInfo()

