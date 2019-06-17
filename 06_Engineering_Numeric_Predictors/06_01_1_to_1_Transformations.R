# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 6.1 at
# https://bookdown.org/max/FES/numeric-one-to-one.html
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(caret)
library(tidymodels)

theme_set(theme_bw() + theme(legend.position = "top"))

# ------------------------------------------------------------------------------

data("segmentationData")

# ------------------------------------------------------------------------------

segmentationData$Cell <- NULL
segmentationData <- segmentationData[, c("EqSphereAreaCh1", "PerimCh1", "Class", "Case")]
names(segmentationData)[1:2] <- paste0("Predictor", LETTERS[1:2])

example_train <- subset(segmentationData, Case == "Train")
example_test  <- subset(segmentationData, Case == "Test")

example_train$Case <- NULL
example_test$Case  <- NULL

simple_trans_rec <- recipe(Class ~ ., data = example_train) %>%
  step_BoxCox(PredictorA, PredictorB) %>%
  prep(training = example_train)

simple_trans_test <- bake(simple_trans_rec, example_test)
pred_b_lambda <-
  tidy(simple_trans_rec, number = 1) %>% 
  filter(terms == "PredictorB") %>% 
  select(value)

bc_before <- ggplot(example_test, aes(x = PredictorB)) + 
  geom_histogram(bins = 35, col = "blue", fill = "blue", alpha = .6) + 
  xlab("Predictor B") + 
  ggtitle("(a)")
bc_after <- ggplot(simple_trans_test, aes(x = PredictorB)) + 
  geom_histogram(bins = 35, col = "red", fill = "red", alpha = .6) + 
  xlab("Predictor B (inverse)") + 
  ggtitle("(b)")


# ------------------------------------------------------------------------------

sessionInfo()

