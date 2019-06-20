# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 7.5.2 at
# https://bookdown.org/max/FES/detecting-interaction-effects.html#approaches-when-complete-enumeration-is-practically-impossible
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(pre)
library(ranger)
library(tidymodels)
library(AmesHousing)
library(doParallel)
library(stringr)

source("../clean_value.R")

# ------------------------------------------------------------------------------

workers <- parallel::detectCores() - 1

# Memory requirements: 
# In addition to the main R session (using ~1GB), each parallel worker's peak  
# memory usage is estimated to need about 10GB of memory when pre::pre() and 
# pre::interact() are called. These numbers increase as the parameter `ntrees`
# increases. 

cl <- makeForkCluster(nnodes = workers)
registerDoParallel(cl)

# ------------------------------------------------------------------------------

h_stat <- function(data, ...) {
  require(foreach)
  pre_obj <- pre(data = data, ...)
  
  pred_names <- pre_obj$x_names
  pred_df <- 
    tibble(
      Predictor = pred_names,
      H = 0
    )
  
  int_obj <-
    try(
      interact(
        pre_obj,
        parallel = TRUE,
        plot = FALSE
      ),
      silent = TRUE)
  
  if (!inherits(int_obj, "try-error")) {
    res <-
      tibble(
        Predictor = names(int_obj),
        H = as.numeric(int_obj)
      )
    missing_pred <- setdiff(pred_names, res$Predictor)
    if (length(missing_pred) > 0) {
      res <-
        pred_df %>% 
        dplyr::filter(Predictor %in% missing_pred) %>%
        bind_rows(res)
    }
  } else {
    print(int_obj)
    res <- pred_df
  }
  res
}

boot_h <- function(split, ...) {
  dat <-
    recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built +
             Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
             Central_Air + Longitude + Latitude + MS_SubClass +
             Alley + Lot_Frontage + Pool_Area + Garage_Finish + 
             Foundation + Land_Contour + Roof_Style,
           data = analysis(split)) %>%
    step_log(Sale_Price, base = 10) %>%
    step_BoxCox(Lot_Area, Gr_Liv_Area, Lot_Frontage) %>%
    step_other(Neighborhood, MS_SubClass, Roof_Style, Foundation,
               threshold = 0.05)  %>%
    step_zv(all_predictors()) %>% 
    prep(training = analysis(split), retain = TRUE) %>%
    juice()
  h_stat(data = dat, ...)
}

# ------------------------------------------------------------------------------

ames <- make_ames()

set.seed(955)
ames_split <- initial_split(ames)
ames_train <- training(ames_split)

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
  step_other(Neighborhood, MS_SubClass, Roof_Style, Foundation, 
             threshold = 0.05)  %>%
  step_zv(all_predictors())

ames_pre <- ames_rec %>% prep(ames_train) %>% juice()

# ------------------------------------------------------------------------------

set.seed(3108)
obs_h <- 
  h_stat(
    data = ames_pre,
    Sale_Price ~ .,
    normalize = TRUE,
    ntrees = 50,
    winsfrac = 0,
    verbose = TRUE,
    par.final = TRUE
  )

# ------------------------------------------------------------------------------

set.seed(9081)
h_vals <- 
  bootstraps(ames_train, times = 20) %>%
  mutate(
    stats = 
      map(
        splits,
        boot_h,
        Sale_Price ~ .,
        normalize = FALSE,
        ntrees = 50,
        winsfrac = 0,
        par.final = TRUE
      )
  )

h_stats <- 
  h_vals %>%
  pull(stats) %>%
  bind_rows() %>%
  group_by(Predictor) %>%
  summarise(Bootstrap = median(H)) %>%
  full_join(
    obs_h %>% dplyr::rename(`Original Data` = H)
  ) %>%
  arrange(Bootstrap)

ordered_pred <- 
  h_stats %>%
  pull(Predictor)

h_stats <-
  h_stats %>%
  gather(Estimator, H, -Predictor) %>%
  mutate(Predictor = factor(Predictor, levels = ordered_pred)) %>% 
  dplyr::filter(H > 0)

# https://bookdown.org/max/FES/approaches-when-complete-enumeration-is-practically-impossible.html#fig:interactions-h-statistic
p <- 
  h_stats %>% 
  mutate(
    Predictor = clean_value(Predictor),
    Predictor = str_replace(Predictor, ":", "")
  ) %>% 
  ggplot(aes(x = reorder(Predictor, H), y = H, shape = Estimator)) + 
  geom_hline(yintercept = 0.001, col = "red", alpha = 0.5, lty = 2) + 
  geom_point() + 
  coord_flip() + 
  xlab("") + 
  scale_shape_manual(values = c(16, 1)) + 
  theme(legend.position = "top")


# ------------------------------------------------------------------------------

stopCluster(cl)

# ------------------------------------------------------------------------------

rf_mod <- ranger(Sale_Price ~ ., data = ames_pre, num.trees = 2000,
                 importance = "impurity", num.threads = workers,
                 seed = 8516)

rf_imp <- 
  tibble(
    Predictor = names(rf_mod$variable.importance),
    Importance = unname(rf_mod$variable.importance)
  )

# https://bookdown.org/max/FES/approaches-when-complete-enumeration-is-practically-impossible.html#fig:interactions-rf-importance
p <- 
  ggplot(rf_imp, aes(x = reorder(Predictor, Importance), y = Importance)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  xlab("")

# ------------------------------------------------------------------------------

save(h_stats, obs_h, file = "ames_h_stats.RData")
save(rf_mod, rf_imp, file = "ames_rf.RData")

# ------------------------------------------------------------------------------

sessionInfo()

if (!interactive())
  q("no")

