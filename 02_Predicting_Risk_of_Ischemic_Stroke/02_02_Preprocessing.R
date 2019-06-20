# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 2.2 at
# https://bookdown.org/max/FES/stroke-preprocessing.html
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(tidymodels)
library(corrplot)

# ------------------------------------------------------------------------------

load(
  file.path(
    "..",
    "Data_Sets",
    "Ischemic_Stroke",
    "stroke_data.RData"
  )
)

VC_preds <- 
  c("CALCVol", "CALCVolProp", "MATXVol", "MATXVolProp", "LRNCVol", 
    "LRNCVolProp", "MaxCALCArea", "MaxCALCAreaProp", "MaxDilationByArea", 
    "MaxMATXArea", "MaxMATXAreaProp", "MaxLRNCArea", "MaxLRNCAreaProp", 
    "MaxMaxWallThickness", "MaxRemodelingRatio", "MaxStenosisByArea", 
    "MaxWallArea", "WallVol", "MaxStenosisByDiameter")

risk_preds <-
  c("age", "sex", "SmokingHistory", "AtrialFibrillation", "CoronaryArteryDisease", 
    "DiabetesHistory", "HypercholesterolemiaHistory", "HypertensionHistory")

# ------------------------------------------------------------------------------

stroke_train %>% 
  count(Stroke) %>% 
  mutate(Data = "Training") %>% 
  bind_rows(
    stroke_test %>% 
      count(Stroke) %>% 
      mutate(Data = "Testing")
  ) %>% 
  spread(Stroke, n)

# ------------------------------------------------------------------------------

# https://bookdown.org/max/FES/numeric-one-to-one.html#numeric-one-to-one

fig_2_2_a <- 
  bind_rows(stroke_train, stroke_test) %>% 
  ggplot(aes(x = MaxLRNCArea)) +
  geom_histogram(bins = 15, col = "#D53E4F", fill = "#D53E4F", alpha = .5) +
  xlab("MaxLRNCArea") +
  ylab("Frequency") +
  ggtitle("(a)") +
  theme_bw()

fig_2_2_b <- 
  recipe(Stroke ~ ., data = bind_rows(stroke_train, stroke_test)) %>% 
  step_YeoJohnson(all_predictors()) %>% 
  prep() %>% 
  juice() %>% 
  ggplot(aes(x = MaxLRNCArea)) +
  geom_histogram(bins = 15, col = "#D53E4F", fill = "#D53E4F", alpha = .5) +
  xlab("Transformed MaxLRNCArea") +
  ylab("Frequency") +
  ggtitle("(b)") +
  theme_bw()

# ------------------------------------------------------------------------------

risk_train <-
  recipe(Stroke ~ ., data = stroke_train) %>%
  step_center(VC_preds) %>%
  step_scale(VC_preds) %>%
  step_YeoJohnson(VC_preds) %>%
  prep() %>% 
  juice() %>%
  select(-one_of(c("Stroke", "NASCET", risk_preds)))

risk_corr <- cor(risk_train)

# https://bookdown.org/max/FES/stroke-preprocessing.html#fig:stroke-corrMatrix
# corrplot(risk_corr, addgrid.col = rgb(0, 0, 0, .05), order = "hclust")

# ------------------------------------------------------------------------------

sessionInfo()



