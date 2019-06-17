# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 3.6 and 3.7 at
# https://bookdown.org/max/FES/model-optimization-and-tuning.html
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(caret)
library(tidymodels)

theme_set(theme_bw() + theme(legend.position = "top"))

# ------------------------------------------------------------------------------

# Load pre-computed model results for OkC data
load(
  file.path(
    "..",
    "05_Encoding_Categorical_Predictors",
    "5_06_Creating_Features_from_Text_Data/",
    "okc_knn_keyword.RData"
  )
)

load(
  file.path(
    "..",
    "05_Encoding_Categorical_Predictors",
    "5_06_Creating_Features_from_Text_Data/",
    "okc_mlp_keyword.RData"
  )
)

load(
  file.path(
    "..",
    "05_Encoding_Categorical_Predictors",
    "5_06_Creating_Features_from_Text_Data/",
    "okc_dff.RData"
  )
)

# ------------------------------------------------------------------------------

# https://bookdown.org/max/FES/model-optimization-and-tuning.html#fig:review-knn-plot

fig_3_11 <- 
  ggplot(knn_keyword) +
  geom_line(data = knn_keyword$resample, aes(group = Resample, col = Resample), alpha = .4) + 
  theme(legend.position = "none")

# ------------------------------------------------------------------------------

mlp_keyword$results %>% 
  arrange(desc(ROC)) %>% 
  dplyr::select(size, dropout, batch_size, lr, rho, decay, activation, ROC)

# ------------------------------------------------------------------------------

glm_roc <- glm_keyword$resample[, c("ROC", "Resample")]
names(glm_roc)[1] <- "Logistic Regression"
net_roc <- merge(mlp_keyword$resample, mlp_keyword$bestTune)
net_roc <- net_roc[, c("ROC", "Resample")]
names(net_roc)[1] <- "Neural Network"
roc_diffs <- merge(glm_roc, net_roc)
roc_diffs$Difference <- roc_diffs[, "Neural Network"] - roc_diffs[, "Logistic Regression"]
rownames(roc_diffs) <- gsub("Fold0", "Fold  ", roc_diffs$Resample)
rownames(roc_diffs) <- gsub("Fold10", "Fold 10", rownames(roc_diffs))

roc_diffs$Resample <- NULL

cor(roc_diffs[, "Neural Network"], roc_diffs[, "Logistic Regression"])

okc_dff

# ------------------------------------------------------------------------------

sessionInfo()



