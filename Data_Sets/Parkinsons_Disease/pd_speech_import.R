library(tidyverse)
library(rsample)

# data at https://archive.ics.uci.edu/ml/datasets/Parkinson%27s+Disease+Classification#

pd_speech <-
  read_csv("pd_speech_features.csv", skip = 1) %>%
  group_by(id) %>%
  summarise_all(mean) %>%
  mutate(
    class = factor(
      ifelse(class == 1, "PD", "control"),
      levels = c("PD", "control")
    )
  ) %>%
  dplyr::select(-id, -numPulses, -numPeriodsPulses, male = gender, Class = class)

save(pd_speech, file = "pd_data.RData")

# For our analyses, only look at scored numeric predictors ---------------------

pd_speech <-
  pd_speech %>%
  dplyr::select(-male)

# Split the data ---------------------------------------------------------------

set.seed(825)
pd_split <- initial_split(pd_speech, prop = 0.75)

pd_tr <- training(pd_split)
pd_te <-  testing(pd_split)

save(pd_tr, pd_te, file = "pd_split.RData")

# ------------------------------------------------------------------------------

sessionInfo()

# ------------------------------------------------------------------------------

if (!interactive())
  q("no")
