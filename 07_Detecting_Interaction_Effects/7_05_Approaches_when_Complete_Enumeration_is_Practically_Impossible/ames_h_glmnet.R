# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 7.5.3 at
# https://bookdown.org/max/FES/detecting-interaction-effects.html#approaches-when-complete-enumeration-is-practically-impossible
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(caret)
library(glmnet)
library(tidymodels)
library(AmesHousing)

# ------------------------------------------------------------------------------

ames <- make_ames()

set.seed(955)
ames_split <- initial_split(ames)
ames_train <- training(ames_split)

set.seed(24873)
ames_folds <- vfold_cv(ames_train)

ames_ind <- rsample2caret(ames_folds)

# ------------------------------------------------------------------------------

main_rec <-
  recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built +
           Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
           Central_Air + Longitude + Latitude + MS_SubClass +
           Alley + Lot_Frontage + Pool_Area + Garage_Finish + 
           Foundation + Land_Contour + Roof_Style,
         data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_BoxCox(Lot_Area, Gr_Liv_Area, Lot_Frontage) %>%
  step_other(Neighborhood, threshold = 0.05) %>% 
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_bs(Longitude, Latitude, options = list(df = 5))

# ------------------------------------------------------------------------------

load("ames_h_stats.RData")

int_vars <- 
  h_stats %>% 
  dplyr::filter(Estimator == "Bootstrap" & H > 0.001) %>% 
  pull(Predictor)

interactions <- t(combn(as.character(int_vars), 2))
colnames(interactions) <- c("var1", "var2")

interactions <- 
  interactions %>% 
  as_tibble() %>% 
  mutate(
    term = 
      paste0(
        "starts_with('",
        var1,
        "'):starts_with('",
        var2,
        "')"
      )
  ) %>% 
  pull(term) %>% 
  paste(collapse = "+")

interactions <- paste("~", interactions)
interactions <- as.formula(interactions)

int_rec <-
  recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built +
           Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
           Central_Air + Longitude + Latitude + MS_SubClass +
           Alley + Lot_Frontage + Pool_Area + Garage_Finish + 
           Foundation + Land_Contour + Roof_Style,
         data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_BoxCox(Lot_Area, Gr_Liv_Area, Lot_Frontage) %>%
  step_other(Neighborhood, threshold = 0.05) %>% 
  step_dummy(all_nominal()) %>%
  step_interact(interactions) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_bs(Longitude, Latitude, options = list(df = 5))

# ------------------------------------------------------------------------------

ctrl <- 
  trainControl(
    method = "cv",
    index = ames_ind$index,
    indexOut = ames_ind$indexOut
  )

glmn_grid <- expand.grid(alpha = seq(.2, 1, by = .2), lambda = 10^seq(-4, -1, by = 0.1))

main_glmn_h <- 
  train(main_rec,
        data = ames_train, 
        method = "glmnet",
        tuneGrid = glmn_grid,
        trControl = ctrl
  )

int_glmn_h <- 
  train(int_rec,
        data = ames_train, 
        method = "glmnet",
        tuneGrid = glmn_grid,
        trControl = ctrl
        )

p <- 
  ggplot(int_glmn_h) + 
  scale_x_log10() + 
  theme_bw() + 
  theme(legend.position = "top")

# ------------------------------------------------------------------------------

main_info_h <- 
  list(
    all = 
      main_rec %>% 
      prep(ames_train) %>% 
      juice(all_predictors()) %>% 
      ncol(),
    main = length(predictors(main_glmn_h)),
    perf = getTrainPerf(main_glmn_h)
  )

int_info_h <- 
  list(
    all = 
      int_rec %>% 
      prep(ames_train) %>% 
      juice(all_predictors()) %>% 
      ncol(),
    main = sum(!grepl("_x_", predictors(int_glmn_h))),
    int = sum(grepl("_x_", predictors(int_glmn_h))),
    perf = getTrainPerf(int_glmn_h)
  )

save(main_info_h, int_info_h, int_glmn_h, file = "ames_glmnet_h.RData")

int_glmn_h$bestTune
int_info_h$main
int_info_h$int

# ------------------------------------------------------------------------------

sessionInfo()

if (!interactive())
  q("no")
