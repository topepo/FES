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

library(tidymodels)
library(AmesHousing)
library(furrr)
library(stringr)
library(cli)
library(crayon)
library(stringr)

source("fsa_functions.R")
source("../clean_value.R")

# ------------------------------------------------------------------------------

ames <- make_ames()

set.seed(955)
ames_split <- initial_split(ames)
ames_train <- training(ames_split)

set.seed(24873)
ames_folds <- vfold_cv(ames_train)

# ------------------------------------------------------------------------------

ames_rec <-
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
  step_bs(Longitude, Latitude, options = list(df = 5))  %>%
  step_zv(all_predictors())

# ------------------------------------------------------------------------------

multi_metric <- metric_set(rmse, mae, rsq)

lr_spec <- linear_reg() %>% set_engine("lm")

set.seed(236)
ames_search <- fsa_two_way(ames_folds, ames_rec, lr_spec, multi_metric)

# for parallel processing
plan(multiprocess)

# https://bookdown.org/max/FES/approaches-when-complete-enumeration-is-practically-impossible.html#tab:interactions-fsa-ames-results
ames_search %>% 
  arrange(perf) %>% 
  dplyr::filter(perf < ames_search %>% slice(1) %>% pull(perf)) %>% 
  dplyr::select(-iter, -seeds, -change, -swaps, RMSE = perf) %>% 
  distinct() %>% 
  mutate(
    var_1 = clean_value(var_1),
    var_2 = clean_value(var_2)
  ) %>% 
  group_by(var_1, var_2) %>% 
  summarize(
    pval = pval[which.min(RMSE)],
    RMSE = RMSE[which.min(RMSE)]
  ) %>% 
  ungroup() %>% 
  arrange(RMSE)

# ------------------------------------------------------------------------------

save(ames_search, file = "ames_fsa.RData")

# ------------------------------------------------------------------------------

sessionInfo()

if (!interactive())
  q("no")

