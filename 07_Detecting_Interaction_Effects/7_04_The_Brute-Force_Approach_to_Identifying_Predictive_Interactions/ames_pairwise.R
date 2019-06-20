# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 7.4 at
# https://bookdown.org/max/FES/detecting-interaction-effects.html#complete-enumeration
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(utils)
library(caret)
library(tidymodels)
library(AmesHousing)
library(doParallel)

# ------------------------------------------------------------------------------

workers <- parallel::detectCores() - 1

# Memory requirements: 
# In addition to the main R session, each parallel worker is estimated to need 
# about 500MB of memory.

cl <- makeForkCluster(nnodes = workers)
registerDoParallel(cl)

# ------------------------------------------------------------------------------

compare_models_1way <- function(a, b, metric = a$metric[1], ...) {
  mods <- list(a, b)
  rs <- resamples(mods)
  diffs <- diff(rs, metric = metric[1], ...)
  diffs$statistics[[1]][[1]]
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
  step_other(Neighborhood, threshold = 0.05)  %>%
  step_bs(Longitude, Latitude, options = list(df = 5)) %>% 
  step_zv(all_predictors())

# ------------------------------------------------------------------------------

ames_preds <- ames_rec$var_info$variable[ames_rec$var_info$role == "predictor"]

## Get all two factor interactions
interactions <- t(combn(ames_preds, 2))
colnames(interactions) <- c("var1", "var2")

set.seed(2532)
interactions <- 
  interactions %>% 
  as_tibble() %>% 
  # sample_n(50) %>%
  mutate(
    Reduction = NA_real_,
    Pvalue = NA_real_,
    RMSE = NA_real_,
    anova_p = NA_real_
  )

# ------------------------------------------------------------------------------

int_ctrl <- trainControl(method = "repeatedcv", repeats = 5)

set.seed(2691)
main_eff <- train(ames_rec, 
                  data = ames_train, 
                  method = "lm", 
                  metric = "RMSE",
                  trControl = int_ctrl)

for (i in 1:nrow(interactions)) {
  tmp_vars <- c("Class", interactions$var1[i], interactions$var2[i])
  
  int <-
    as.formula(paste0(
      "~ starts_with('",
      interactions$var1[i],
      "'):starts_with('",
      interactions$var2[i],
      "')"
    ))
  
  int_rec <- 
    ames_rec %>% 
    step_interact(int) %>% 
    step_zv(all_predictors())
  
  set.seed(2691)
  main_int <- train(int_rec, 
                    data = ames_train, 
                    method = "lm", 
                    metric = "RMSE",
                    trControl = int_ctrl)  
  
  tmp_diff <- compare_models_1way(main_int, main_eff, alternative = "less")
  interactions$RMSE[i] <- getTrainPerf(main_eff)[1, "TrainRMSE"]
  interactions$Reduction[i] <- -tmp_diff$estimate
  interactions$Pvalue[i] <- tmp_diff$p.value
  
  a1 <- 
    anova(main_eff$finalModel, main_int$finalModel) %>% 
    tidy() %>% 
    slice(2) %>% 
    pull(p.value)
  
  interactions$anova_p[i] <- a1
}


# ------------------------------------------------------------------------------

raw <- 
  interactions %>%
  dplyr::filter(Reduction > 0) %>%
  dplyr::select(var1, var2, Pvalue, anova_p) %>%
  dplyr::rename(Resampling = Pvalue) %>%
  dplyr::rename(Traditional = anova_p) %>%
  mutate(Method = "No Adjustment")

fdrs <- 
  raw %>%
  mutate(
    Resampling = p.adjust(Resampling, method = "BH"),
    Traditional = p.adjust(Traditional, method = "BH")
  ) %>%
  mutate(Method = "FDR")

bon <- 
  raw %>%
  mutate(
    Resampling = p.adjust(Resampling, method = "bonferroni"),
    Traditional = p.adjust(Traditional, method = "bonferroni")
  ) %>%
  mutate(Method = "Bonferroni")

all_p <- 
  bind_rows(raw, fdrs, bon) %>%
  gather(Estimate, Value, -var1, -var2, -Method) %>%
  mutate(
    Method = factor(Method, levels = c("No Adjustment", "FDR", "Bonferroni")),
    Estimate = factor(Estimate, levels = c("Traditional", "Resampling"))
  )

# ------------------------------------------------------------------------------

plot <- 
  ggplot(all_p, aes(x = Value)) + 
  geom_histogram(breaks = (0:20)/20) + 
  geom_rug(col = "red") + 
  facet_grid(Estimate ~ Method)

save(all_p, interactions, file = "ames_pairwise.RData")

# ------------------------------------------------------------------------------

sessionInfo()

if (!interactive())
  q("no")
