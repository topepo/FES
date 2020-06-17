# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 1.1 at
# https://bookdown.org/max/FES/intro-intro.html#a-simple-example
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(caret)
library(tidymodels)
# library(pROC)
library(ggthemes)
library(e1071)

# Create example data ----------------------------------------------------------

data(segmentationData)

segmentationData <- 
  segmentationData %>% 
  dplyr::select(EqSphereAreaCh1, PerimCh1, Class, Case) %>% 
  setNames(c("PredictorA", "PredictorB", "Class", "Case")) %>% 
  mutate(Class = factor(ifelse(Class == "PS", "One", "Two")))

example_train <- 
  segmentationData %>% 
  dplyr::filter(Case == "Train") %>% 
  dplyr::select(-Case)

example_test  <- 
  segmentationData %>% 
  dplyr::filter(Case == "Test") %>% 
  dplyr::select(-Case)

# Do analysis in natural units -------------------------------------------------

example_ctrl <- 
  trainControl(method = "none",
               classProbs = TRUE,
               summaryFunction = twoClassSummary)

natural_plot <- 
  ggplot(example_train) + 
  aes(x = PredictorA, y = PredictorB, color = Class) +
  geom_point(alpha = .3, cex = 1.5) + 
  theme(legend.position = c(.1, .8)) +
  scale_colour_tableau() + 
  ggtitle("(a)")

natural_terms <- train(Class ~ PredictorA + PredictorB,
                       data = example_train,
                       method = "glm",
                       metric = "ROC",
                       trControl = example_ctrl)

natural_dat <-
  example_test %>% 
  mutate(prob = predict(natural_terms, example_test, type = "prob")[,1]) %>% 
  roc_curve(Class, prob) %>% 
  mutate(Format = "Natural Units")

# autoplot(natural_dat)

# Analysis of transformed data -------------------------------------------------

trans_terms <- train(Class ~ PredictorA + PredictorB,
                     data = example_train,
                     method = "glm",
                     preProc = "BoxCox",
                     metric = "ROC",
                     trControl = example_ctrl)

trans_dat <-
  example_test %>% 
  mutate(prob = predict(trans_terms, example_test, type = "prob")[,1]) %>% 
  roc_curve(Class, prob) %>% 
  mutate(Format = "Inverse Units") 

# https://bookdown.org/max/FES/a-simple-example.html#fig:intro-natural
# panel (a)
trans_plot <- 
  ggplot(example_train) + 
  aes(x = 1/PredictorA, y = 1/PredictorB, color = Class) +
  geom_point(alpha = .3, cex = 1.5) + 
  theme(legend.position = c(.1, .8)) +
  scale_colour_tableau() + 
  xlab("1/A") + ylab("1/B") + 
  ggtitle("(a)")

# Combine for plot -------------------------------------------------------------

both_dat <- 
  bind_rows(natural_dat, trans_dat) %>%
  mutate(Format = factor(Format, levels = c("Natural Units", "Inverse Units")))

# https://bookdown.org/max/FES/a-simple-example.html#fig:intro-natural
# panel (b)
trans_roc_plot <- 
  ggplot(both_dat) +
  geom_step(aes(x = 1 - specificity, y = sensitivity, color = Format)) + 
  coord_equal() + 
  xlab("False Positive Rate") + 
  ylab("True Positive Rate") + 
  theme_bw()+ 
  theme(legend.position = c(.8, .2)) + 
  scale_colour_manual(values = c("Natural Units" = "black", "Inverse Units" = "blue")) + 
  ggtitle("(b)") + 
  geom_abline(intercept = 0, slope = 1, col = "grey", lty = 2)

# ------------------------------------------------------------------------------

sessionInfo()

