# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 3.2.1 at
# https://bookdown.org/max/FES/measuring-performance.html#reg-metrics
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(tidymodels)
library(gridExtra)

theme_set(theme_bw() + theme(legend.position = "top"))

# ------------------------------------------------------------------------------

# Load linear model results for Chicago data
load(
  file.path(
    "..",
    "01_Introduction",
    "1_03_A_More_Complex_Example",
    "lm_date_only.RData"
  )
)

# ------------------------------------------------------------------------------

lm_date_only$pred %>% rmse(obs, pred)
lm_date_only$pred %>% rsq(obs, pred)

# ------------------------------------------------------------------------------

chi_xy <- ggplot(lm_date_only$pred, aes(x = obs, y = pred)) + 
  geom_abline(intercept = 0, slope = 1, col = "grey", lty = 1) + 
  geom_point(alpha = .5, size = .7) + 
  geom_smooth(method = lm, se = FALSE, col = rgb(0, 0, 1, .2), alpha = .5) + 
  xlab("Actual Train Ridership (thousands)") + 
  ylab("Predicted Ridership (thousands)") + 
  ggtitle("(a)")

chi_hst <- ggplot(lm_date_only$pred, aes(x = obs - pred)) + 
  geom_histogram(binwidth = 1, col = "#377EB8", fill = "#377EB8", alpha = .5) + 
  xlab("Ridership Residuals (thousands)") + 
  ggtitle("(b)")

# https://bookdown.org/max/FES/measuring-performance.html#fig:review-chi-r2
# grid.arrange(chi_xy, chi_hst, nrow = 1, ncol = 2)

# ------------------------------------------------------------------------------

sessionInfo()

