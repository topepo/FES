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
library(gridExtra)
library(stringr)

source("../clean_value.R")

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
  step_bs(Longitude, Latitude, options = list(df = 5)) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

# ------------------------------------------------------------------------------

int_vars <- 
  main_rec %>% 
  pluck("var_info") %>% 
  dplyr::filter(role == "predictor") %>% 
  pull(variable)

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
  step_bs(Longitude, Latitude, options = list(df = 5)) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

# ------------------------------------------------------------------------------

ctrl <- 
  trainControl(
    method = "cv",
    index = ames_ind$index,
    indexOut = ames_ind$indexOut
  )

glmn_grid <- expand.grid(alpha = seq(.2, 1, by = .2), lambda = 10^seq(-4, -1, by = 0.1))

main_glmn <- 
  train(main_rec,
        data = ames_train, 
        method = "glmnet",
        tuneGrid = glmn_grid,
        trControl = ctrl
  )

int_glmn <- 
  train(int_rec,
        data = ames_train, 
        method = "glmnet",
        tuneGrid = glmn_grid,
        trControl = ctrl
  )

int_glmn$bestTune

tune_plot <- 
  int_glmn %>%
  pluck("results") %>%
  mutate(alpha = factor(alpha)) %>%
  ggplot(aes(x = lambda, y = RMSE, col = alpha)) +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  ggtitle("(a)") +
  theme(
    legend.position = c(0.50, 0.75), 
    legend.background = element_blank()
  ) +
  labs(
    colour = "Mixing Percentage", 
    x = "Regularization Parameter", 
    y = "RMSE (Cross-Validation)"
  )

top_interaction <- 
  ggplot(ames_train, aes(x = Year_Built*Gr_Liv_Area, y = Sale_Price)) + 
  geom_point(alpha = 0.5) + 
  scale_y_log10() + 
  scale_x_log10() + 
  geom_smooth(se = FALSE) +
  ylab("Sale_Price") +
  xlab("Year Built * Living Area") +
  ggtitle("(b)")

# https://bookdown.org/max/FES/complete-enumeration.html#fig:interactions-lasso
# grid.arrange(tune_plot, top_interaction, nrow = 1)

clean_value <- function(x) {
  x <- str_replace(x, "Neighborhood_", "Neighborhood: ")
  x <- str_replace(x, "MS_SubClass_", "MS SubClass: ")
  x <- str_replace(x, "Land_Contour_", "Land Contour: ")
  x <- str_replace(x, "Roof_Style_", "Roof Style: ")
  x <- str_replace(x, "Foundation_", "Foundation: ")
  x <- str_replace(x, "Garage_Finish_", "Garage Finish: ")
  x <- str_replace(x, "Central_Air", "Central Air:")
  x <- str_replace(x, "Bldg_Type", "Building Type")
  x <- str_replace(x, "Alley", "Alley:")
  x <- str_replace(x, "Gr_Liv_Area", "Living Area")
  x <- str_replace(x, "_bs_1", " (spline)")   
  x <- str_replace(x, "_bs_2", " (spline)")
  x <- str_replace(x, "_bs_3", " (spline)")
  x <- str_replace_all(x, "_", " ")
  x
}

# https://bookdown.org/max/FES/complete-enumeration.html#tab:interactions-lasso-top
lasso_int_coefs <- 
  int_glmn %>% 
  pluck("finalModel") %>% 
  coef(s = int_glmn %>% pluck("bestTune") %>% pluck("lambda")) %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  setNames("Coefficient") %>% 
  rownames_to_column("Predictor") %>% 
  dplyr::filter(grepl("_x_", Predictor)) %>% 
  arrange(desc(abs(Coefficient))) %>% 
  dplyr::filter(Predictor != "(Intercept)") %>% 
  slice(1:15) %>% 
  mutate(
    split = str_split(Predictor, "_x_"),
    split = map(split, sort),
    var_1 = map_chr(split, pluck, 1),
    var_2 = map_chr(split, pluck, 2),
    var_1 = map_chr(var_1, clean_value),
    var_2 = map_chr(var_2, clean_value)  
  ) %>% 
  dplyr::select(`Predictor 1` = var_1, `Predictor 2` = var_2, Coefficient)

# ------------------------------------------------------------------------------

main_info <- 
  list(
    all = 
      main_rec %>% 
      prep(ames_train) %>% 
      juice(all_predictors()) %>% 
      ncol(),
    main = length(predictors(main_glmn)),
    perf = getTrainPerf(main_glmn)
  )

int_info <- 
  list(
    all = 
      int_rec %>% 
      prep(ames_train) %>% 
      juice(all_predictors()) %>% 
      ncol(),
    main = sum(!grepl("_x_", predictors(int_glmn))),
    int = sum(grepl("_x_", predictors(int_glmn))),
    perf = getTrainPerf(int_glmn)
  )

save(main_info, int_info, int_glmn, file = "ames_glmnet.RData")

# ------------------------------------------------------------------------------

# Determine predictors involved with main effects

main_vars <- tibble(predictor = predictors(main_glmn))

all_dummies <-
  main_rec %>% 
  prep(ames_train) %>% 
  tidy(number = 4) %>% 
  mutate(predictor = main_rec$steps[[4]]$naming(terms, columns))

qual_vars <- 
  inner_join(all_dummies, main_vars, by = "predictor") %>% 
  distinct(terms) %>% 
  pull(terms)

quant_vars <- 
  summary(main_rec) %>% 
  dplyr::select(predictor = variable) %>% 
  inner_join(main_vars, by = "predictor") %>% 
  pull(predictor)

used_main <- c(qual_vars, quant_vars)

# make their interactions

interaction_subset <- t(combn(as.character(used_main), 2))
colnames(interaction_subset) <- c("var1", "var2")

interaction_subset <- 
  interaction_subset %>% 
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

interaction_subset <- paste("~", interaction_subset)
interaction_subset <- as.formula(interaction_subset)

two_stage_rec <-
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
  step_interact(interaction_subset) %>% 
  step_zv(all_predictors()) %>%
  step_bs(Longitude, Latitude, options = list(df = 5)) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

two_stage_glmn <- 
  train(two_stage_rec,
        data = ames_train, 
        method = "glmnet",
        tuneGrid = glmn_grid,
        trControl = ctrl
  )

two_stage_info <- 
  list(
    all = 
      two_stage_rec %>% 
      prep(ames_train) %>% 
      juice(all_predictors()) %>% 
      ncol(),
    main = sum(!grepl("_x_", predictors(two_stage_glmn))),
    int = sum(grepl("_x_", predictors(two_stage_glmn))),
    perf = getTrainPerf(two_stage_glmn)
  )

save(used_main, two_stage_info, two_stage_glmn, file = "ames_glmnet_two_stage.RData")

# notes from
# https://bookdown.org/max/FES/approaches-when-complete-enumeration-is-practically-impossible.html

two_stage_glmn$bestTune

two_stage_info$main

two_stage_info$int

# ------------------------------------------------------------------------------

sessionInfo()

if (!interactive())
  q("no")
