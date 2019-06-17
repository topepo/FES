# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 3.2.2 at
# https://bookdown.org/max/FES/measuring-performance.html#class-metrics
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(tidymodels)
library(gridExtra)

theme_set(theme_bw() + theme(legend.position = "top"))

# ------------------------------------------------------------------------------

# Load logistic model results for OkC data
load(
  file.path(
    "..",
    "05_Encoding_Categorical_Predictors",
    "5_06_Creating_Features_from_Text_Data/",
    "okc_glm_keyword.RData"
  )
)

# ------------------------------------------------------------------------------

glm_keyword$pred %>% conf_mat(obs, pred)

class_metrics <- metric_set(accuracy, kap, sens, spec, precision)

glm_keyword$pred %>% class_metrics(obs, estimate = pred)

uncond_metrics <- metric_set(ppv, npv)
glm_keyword$pred %>% uncond_metrics(obs, estimate = pred, prevalence = 0.05)
glm_keyword$pred %>% uncond_metrics(obs, estimate = pred, prevalence = 0.05)

glm_keyword$pred %>% ppv(obs, pred, prevalence = 0.05)
glm_keyword$pred %>% npv(obs, pred, prevalence = 0.05)

# ------------------------------------------------------------------------------

# https://bookdown.org/max/FES/measuring-performance.html#tab:review-class-table
fig_3_4 <- 
  glm_keyword$pred %>% 
  ggplot(aes(x = stem)) + 
  geom_histogram(binwidth = 0.03, col = "#377EB8", fill = "#377EB8", alpha = .5) + 
  facet_wrap(~obs, ncol = 1) + 
  xlab("Pr[Profile is STEM]")

glm_keyword$pred %>% roc_auc(obs, stem)

# https://bookdown.org/max/FES/measuring-performance.html#fig:review-roc-pr-plot
# panel (a)
# glm_keyword$pred %>% roc_curve(obs, stem) %>% autoplot()

glm_keyword$pred %>% pr_auc(obs, stem)

# https://bookdown.org/max/FES/measuring-performance.html#fig:review-roc-pr-plot
# panel (b)
# glm_keyword$pred %>% pr_curve(obs, stem) %>% autoplot()

# ------------------------------------------------------------------------------

sessionInfo()
