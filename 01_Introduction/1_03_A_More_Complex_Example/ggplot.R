# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 1.3 at
# https://bookdown.org/max/FES/intro-intro.html#a-more-complex-example
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(caret)
library(tidymodels)
library(stringr)

# ------------------------------------------------------------------------------

rd_files <- list.files(pattern = "RData$")

lvl <- c("{1}", "{1,2}", "{1,2,3}", "{1,2,4}", "{1,2,5}")

get_resample <- function(fl) {
  load(fl)
  obj <- ls()[ls() != "fl"]
  model_name <- map_chr(str_split(obj, "_"), `[`, 1)
  feature_set <- str_replace(obj, paste0(model_name, "_"), "")
  
  times <- 
    get(obj) %>% 
    purrr::pluck("times") %>% 
    purrr::pluck("everything") %>% 
    purrr::pluck("elapsed")
  
  get(obj) %>% 
    purrr::pluck("resample") %>%
    dplyr::select(RMSE, Resample) %>% 
    summarize(RMSE = mean(RMSE)) %>% 
    mutate(
      file = obj,
      Time = times/60,
      Model = model_name,
      Feature_Set = feature_set,
      Feature_Set = 
        case_when(
          feature_set == "date_only" ~ "{1}",
          feature_set == "date_lag14" ~ "{1,2}",
          feature_set == "date_lagall" ~ "{1,2,3}",
          feature_set == "date_lag14_weth" ~ "{1,2,4}",
          feature_set == "date_lag14_hol" ~ "{1,2,5}",
          TRUE ~ ""
        ),
      Model_Type = 
        case_when(
          Model %in% c("boost", "cb", "rf", "rp") ~ "Trees",
          Model %in% c("glmn", "lm", "pls") ~ "Linear",
          Model %in% c("svmr", "mars") ~ "Nonlinear",
          TRUE ~ "Unknown"
        ),
      Model_Name = 
        case_when(
          Model == "boost" ~ "Boosted Tree",
          Model == "cb" ~ "Cubist",
          Model == "rf" ~ "Random Forest",
          Model == "rp" ~ "CART",
          Model == "glmn" ~ "glmnet",
          Model == "lm" ~ "Linear Regression",
          Model == "pls" ~ "PLS",
          Model == "svmr" ~ "SVM (rbf)",
          Model == "mars" ~ "MARS",
          TRUE ~ "Unknown"
        ),
      Time_Label = 
        case_when(
          Time <= 1 ~ "< 1min",
          Time > 1 & Time <= 60 ~ paste(round(Time, 0), "min"),
          Time > 60 & Time <= 60 * 24 ~ paste(round(Time / 60, 1), "hr"),
          Time > 60 * 24 ~ paste(round(Time / 60 / 24, 1), "days"),
          TRUE ~ "Forever"
        ),
      Label = paste(Model_Name, Time_Label)
      ) 
}

chi_model_results <- 
  map_dfr(rd_files, get_resample) %>% 
  dplyr::filter(Feature_Set != "") %>% 
  mutate(Feature_Set = factor(Feature_Set, levels = lvl))

save(chi_model_results, file = "chi_model_results.RData")

# ggplot(rmse_data, aes(x = Feature_Set, y = RMSE, group = Model, col = Model)) +
#   geom_line(alpha = 0.5) + 
#   geom_point(aes(size = Time), alpha = 0.5, show.legend = FALSE) + 
#   facet_wrap(~ Model_Type) + 
#   theme_bw() + 
#   theme(legend.position = "none") + 
#   ylab("RMSE (in Thousands of Rides)") + 
#   xlab("")

# ------------------------------------------------------------------------------

if (!interactive())
  q("no")
