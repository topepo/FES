# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 5.4 at
# https://bookdown.org/max/FES/categorical-supervised-encoding.html
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(tidymodels)
library(embed)
library(plotly)
library(gridExtra)

options(width = 120)

# Create example data ----------------------------------------------------------

load("../Data_Sets/OkCupid/okc.RData")
load("../Data_Sets/OkCupid/okc_binary.RData")

# Partial pooling example ------------------------------------------------------

partial_rec <- 
  recipe(Class ~ ., data = okc_train) %>%
  step_lencode_bayes(
    where_town,
    outcome = vars(Class),
    verbose = FALSE,
    options = list(
      chains = 5, 
      iter = 1000, 
      cores = min(parallel::detectCores(), 5),
      seed = 18324
    )
  ) %>%
  prep()

# Get raw rates and log-odds
okc_props <- 
  okc_train %>%
  group_by(where_town) %>%
  summarise(
    rate = mean(Class == "stem"),
    raw  = log(rate/(1-rate)),
    n = length(Class)
  ) %>%
  mutate(where_town = as.character(where_town))

okc_props

# Embedding methods ------------------------------------------------------------

# Get the keyword columns
keywords <- names(okc_train_binary)[-1]

# Merge the basic OkC data with the keyword indicators
okc_embed <-
  okc_train %>% 
  dplyr::select(Class, where_town, profile) %>%
  full_join(okc_train_binary, by = "profile")

# Tensorflow wants doubles instead of binary integers
okc_embed[, keywords] <- apply(okc_embed[, keywords], 2, as.numeric)

# Use the entity embedding for supervised learning
set.seed(355)
nnet_rec <- 
  recipe(Class ~ ., data = okc_embed) %>% 
  step_embed(
    where_town,
    outcome = vars(Class),
    num_terms = 3,
    hidden_units = 10,
    predictors = vars(!!!keywords),
    options = embed_control(
      loss = "binary_crossentropy",
      epochs = 30,
      validation_split = 0.2,
      verbose = 0
    )
  ) %>%
  prep()

# Organize results -------------------------------------------------------------

partial_pooled <- 
  tidy(partial_rec, number = 1) %>%
  dplyr::select(-terms, -id) %>%
  setNames(c("where_town", "partial"))

word_embed <- 
  tidy(nnet_rec, number = 1) %>%
  dplyr::select(-terms, -id) %>%
  setNames(c(paste0("Feature", 1:3), "where_town"))

all_est <- 
  partial_pooled %>%
  full_join(okc_props, by = "where_town") %>%
  inner_join(word_embed, by = "where_town") %>%
  dplyr::select(where_town, rate, n, raw, partial, Feature1, Feature2, Feature3)

odds_rng <- extendrange(c(all_est$raw, all_est$partial), f = 0.01)

# Figure 5.2 -------------------------------------------------------------------
# https://bookdown.org/max/FES/categorical-supervised-encoding.html#fig:categorical-log-odds


odds_1 <- 
  ggplot(all_est) +
  aes(x = raw, y = partial, size = log10(n)) + 
  scale_size(range = c(.1, 6)) +
  geom_abline(alpha = .4, lty = 2)  +
  xlim(odds_rng) +
  ylim(odds_rng) +
  xlab("Raw Log-Odds") +
  ylab("Shrunken Log-Odds") + 
  geom_point(aes(text = gsub("_", " ", where_town)), alpha = .4)


odds_2 <- 
  ggplot(all_est) +
  aes(x = .5*(raw + partial), y = raw - partial, size = log10(n)) + 
  scale_size(range= c(.1, 6)) + 
  geom_hline(alpha = .4, lty = 2, yintercept = 0) + 
  xlab("Average Estimate") +
  ylab("Raw - Shrunken") + 
  geom_point(aes(text = gsub("_", " ", where_town)), alpha = .4)

odds_1 <- ggplotly(odds_1, tooltip = "text")
odds_2 <- ggplotly(odds_2, tooltip = "text") 

# plotly::subplot(odds_1, odds_2, nrows = 1, margin = .05, titleX = TRUE, titleY = TRUE)

# Figure 5.3 -------------------------------------------------------------------
# https://bookdown.org/max/FES/categorical-supervised-encoding.html#fig:categorical-word-embed

embed_plot <- 
  word_embed %>%
  full_join(okc_props, by = "where_town") %>%
  gather(feature, value, -rate, -raw, -n, -where_town) %>%
  mutate(location = gsub("_", " ", where_town)) %>%
  ggplot(aes(x = value, y = raw)) + 
  facet_wrap(~feature, scale = "free_x") + 
  ylab("Raw Odds-Ratio") + 
  theme(legend.position = "top") + 
  xlab("Feature Value") + 
  scale_size(range = c(.1, 6)) + 
  geom_point(aes(size = log10(n), text = location), alpha = .4)

# ggplotly(embed_plot, tooltip = "text")

# ------------------------------------------------------------------------------

sessionInfo()

