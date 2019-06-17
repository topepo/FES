# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 5.2 at
# https://bookdown.org/max/FES/encoding-predictors-with-many-categories.html
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(tidymodels)
library(FeatureHashing)
library(stringr)

options(width = 150)

# Create example data ----------------------------------------------------------

load("../Data_Sets/OkCupid/okc.RData")

# ------------------------------------------------------------------------------

# Make small example data set

sample_towns <- c(
  'alameda', 'belmont', 'benicia', 'berkeley', 'castro_valley', 'daly_city', 
  'emeryville', 'fairfax', 'martinez', 'menlo_park', 'mountain_view', 'oakland', 
  'other', 'palo_alto', 'san_francisco', 'san_leandro', 'san_mateo', 
  'san_rafael', 'south_san_francisco', 'walnut_creek'
)

location <- 
  okc_train %>% 
  dplyr::select(where_town) %>% 
  distinct(where_town) %>% 
  arrange(where_town)

# ------------------------------------------------------------------------------
# Create hash features using binary representations

binary_hashes <-
  hashed.model.matrix(
    ~ where_town,
    data = location,
    hash.size = 2 ^ 4,
    signed.hash = FALSE,
    create.mapping = TRUE
  )

binary_mapping <- hash.mapping(binary_hashes)
names(binary_mapping) <- str_remove(names(binary_mapping), "where_town")
binary_calcs <- 
  binary_mapping %>% 
  enframe() %>% 
  set_names(c("town", "column_num_16")) %>% 
  mutate(integer_16 = hashed.value(names(binary_mapping))) %>% 
  dplyr::filter(town %in% sample_towns) %>% 
  arrange(town)

# Table 5.2
# https://bookdown.org/max/FES/encoding-predictors-with-many-categories.html#tab:categorical-hash-values

binary_calcs

binary_df <- 
  binary_hashes %>% 
  as.matrix() %>% 
  as_tibble() %>% 
  bind_cols(location) %>% 
  dplyr::rename(town = where_town) %>% 
  dplyr::filter(town %in% sample_towns) %>% 
  arrange(town)

# TODO update links
# Table 5.3
# https://bookdown.org/max/FES/encoding-predictors-with-many-categories.html#tab:categorical-hash-values
binary_df

# ------------------------------------------------------------------------------
# Create hash features using signed integer representations

signed_hashes <-
  hashed.model.matrix(
    ~ where_town,
    data = location,
    hash.size = 2 ^ 4,
    signed.hash = TRUE
  )

signed_df <- 
  signed_hashes %>% 
  as.matrix() %>% 
  as_tibble() %>% 
  bind_cols(location) %>% 
  dplyr::rename(town = where_town) %>% 
  dplyr::filter(town %in% sample_towns) %>% 
  arrange(town)

# TODO update links
# Table 5.4
# https://bookdown.org/max/FES/encoding-predictors-with-many-categories.html#tab:categorical-hash-values
signed_df

# ------------------------------------------------------------------------------

sessionInfo()

