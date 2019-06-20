# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 2.4 at
# https://bookdown.org/max/FES/stroke-tour.html#stroke-exploration
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

# ------------------------------------------------------------------------------
# Code to compare 2-way interaction models to their main effects model ---------

# `a` and `b` are two models from `train()`
compare_models_1way <- function(a, b, metric = a$metric[1], ...) {
  mods <- list(a, b)
  rs <- resamples(mods)
  diffs <- diff(rs, metric = metric[1], ...)
  diffs$statistics[[1]][[1]]
}

# `risk_preds` is contained in the original data file and has the predictor 
# names for the risk related variables

# ------------------------------------------------------------------------------
# Create a "null model" with no predictors to get baseline performance ---------

null_mat <- data.frame(intercept = rep(1, nrow(stroke_train)))

ctrl <- 
  trainControl(method = "repeatedcv", repeats = 5,
               classProbs = TRUE, 
               summaryFunction = twoClassSummary)

set.seed(63331)
null_mod <- train(x = null_mat, 
                  y = stroke_train$Stroke, 
                  preProc = "YeoJohnson",
                  method = "glm", 
                  metric = "ROC", 
                  trControl = ctrl)

# ------------------------------------------------------------------------------
# Compare the models with single predictors to the risk model. These data make
# https://bookdown.org/max/FES/stroke-tour.html#tab:stroke-strokeRiskAssociations

# `VC_preds` and `risk_preds` contain the predictor names for different sets.

one_predictor_res <- 
  data.frame(Predictor = c(VC_preds, risk_preds), 
             Improvement = NA,
             Pvalue = NA,
             ROC = NA,
             stringsAsFactors = FALSE)


for (i in 1:nrow(one_predictor_res)) {
  set.seed(63331)
  var_mod <- train(Stroke ~ ., 
                   data = stroke_train[, c("Stroke", one_predictor_res$Predictor[i])], 
                   method = "glm", 
                   metric = "ROC",
                   trControl = ctrl)  
  tmp_diff <- compare_models_1way(var_mod, null_mod, alternative = "greater")
  one_predictor_res$ROC[i] <- getTrainPerf(var_mod)[1, "TrainROC"]
  one_predictor_res$Improvement[i] <- tmp_diff$estimate
  one_predictor_res$Pvalue[i] <- tmp_diff$p.value
}

# ------------------------------------------------------------------------------
# Data in table 2.3
# https://bookdown.org/max/FES/stroke-tour.html#tab:stroke-strokeRiskAssociations

one_predictor_res %>% 
  dplyr::filter(Predictor %in% risk_preds) %>% 
  arrange(Pvalue)

# ------------------------------------------------------------------------------
# Figure 2.4
# https://bookdown.org/max/FES/stroke-tour.html#fig:stroke-vascuCAPAssocations

vc_pred <- 
  recipe(Stroke ~ ., data = stroke_train %>% dplyr::select(Stroke, !!!VC_preds)) %>% 
  step_YeoJohnson(all_predictors()) %>% 
  prep(stroke_train %>% dplyr::select(Stroke, !!!VC_preds)) %>% 
  juice() %>% 
  gather(Predictor, value, -Stroke)

# get_max value per predictor

pred_max <- 
  vc_pred %>% 
  group_by(Predictor) %>% 
  summarize(max_val = max(value)) %>% 
  inner_join(one_predictor_res %>% dplyr::select(Pvalue, Predictor)) %>% 
  mutate(
    x = 1.5, 
    value = 1.25 * max_val,
    label = paste0("p-value: ", format.pval(Pvalue, digits = 2, sci = FALSE, eps = .0001))
  )

new_order <- pred_max$Predictor[order(pred_max$Pvalue)]

vc_pred <- 
  vc_pred %>% 
  mutate(Predictor = factor(Predictor, levels = new_order))

pred_max <- 
  pred_max %>% 
  mutate(Predictor = factor(Predictor, levels = new_order))

fig_2_4 <-
  ggplot(vc_pred, aes(x = Stroke, y = value)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.3, cex = .5) + 
  geom_text(data = pred_max, aes(x = x, label = label), size = 3) + 
  facet_wrap(~Predictor, scales = "free_y") + 
  ylab("")

# ------------------------------------------------------------------------------
# Figure 2.5
# https://bookdown.org/max/FES/stroke-tour.html#fig:stroke-maxRemodelingRatioROC

fig_2_5 <- 
  roc_curve(stroke_train, Stroke, MaxRemodelingRatio) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) + 
  geom_abline(alpha = .5, lty = 2) +
  geom_path()

# ------------------------------------------------------------------------------
# Interaction exploration

pairs <- 
  combn(VC_preds, 2) %>% 
  t() %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  mutate(
    Improvement = NA,
    Pvalue = NA,
    ROC = NA
  )

for (i in 1:nrow(pairs)) {
  tmp_vars <- c("Stroke", pairs$V1[i], pairs$V2[i])
  set.seed(63331)
  main_eff <- train(Stroke ~ ., 
                    data = stroke_train[, tmp_vars], 
                    preProc = c("center", "scale", "YeoJohnson"),
                    method = "glm", 
                    metric = "ROC",
                    trControl = ctrl)
  set.seed(63331)
  main_int <- train(Stroke ~ (.)^2, 
                    data = stroke_train[, tmp_vars], 
                    preProc = c("center", "scale", "YeoJohnson"), 
                    method = "glm", 
                    metric = "ROC", 
                    trControl = ctrl)  
  tmp_diff <- compare_models_1way(main_int, main_eff, alternative = "greater")
  pairs$ROC[i] <- getTrainPerf(main_eff)[1, "TrainROC"]
  pairs$Improvement[i] <- tmp_diff$estimate
  pairs$Pvalue[i] <- tmp_diff$p.value
}

retained_pairs <- 
  pairs %>% 
  dplyr::filter(ROC > 0.5  & Pvalue <= 0.2)

# ------------------------------------------------------------------------------
# Figure 2.6
# https://bookdown.org/max/FES/stroke-tour.html#fig:stroke-interactionScreening

vol_plot <- 
  pairs %>% 
  dplyr::filter(ROC > 0.5) %>%
  mutate(Term = paste(V1, "by", V2, "\nROC:", round(ROC, 2))) %>%
  ggplot(aes(x = Improvement, y = -log10(Pvalue))) + 
  xlab("Improvement") +
  geom_point(alpha = .2, aes(size = ROC, text = Term))

vol_plot <- ggplotly(vol_plot, tooltip = "Term")
# vol_plot

# ------------------------------------------------------------------------------

## Create interaction formula
int_form <- 
  pairs %>% 
  dplyr::filter(ROC > 0.5  & Pvalue <= 0.2 & Improvement > 0) %>% 
  mutate(form  = paste0(V1, ":", V2)) %>% 
  pull(form) %>% 
  paste(collapse = "+")
int_form <- paste("~", int_form)
int_form <- as.formula(int_form)

save(int_form, file = "interactions.RData")

# ------------------------------------------------------------------------------

sessionInfo()

