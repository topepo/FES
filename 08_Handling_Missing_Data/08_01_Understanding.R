# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 8.1 at
# https://bookdown.org/max/FES/understanding-the-nature-and-severity-of-missing-information.html
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(caret)
library(tidyverse)
library(naniar)
library(visdat)
library(ComplexHeatmap)

theme_set(theme_bw())

# Data used --------------------------------------------------------------------

data(scat)

load("../Data_Sets/Chicago_trains/chicago.RData")
load("../Data_Sets/Chicago_trains/chicago_raw_entries.RData")
load("../Data_Sets/Chicago_trains/stations.RData")

# Figure 8.1 -------------------------------------------------------------------
# https://bookdown.org/max/FESunderstanding-the-nature-and-severity-of-missing-information.html#fig:missing-vis

convert_missing <- function(x) ifelse(is.na(x), 0, 1)
scat_missing <- apply(scat, 2, convert_missing)

# https://bookdown.org/max/FES/understanding-the-nature-and-severity-of-missing-information.html#fig:missing-vis
Heatmap(
  scat_missing, 
  name = "Missing", #title of legend
  column_title = "Predictors", row_title = "Samples",
  col = c("black","lightgrey"),
  show_heatmap_legend = FALSE,
  row_names_gp = gpar(fontsize = 0) # Text size for row names
)

gg_miss_upset(scat, nsets = 7) 

# Figure 8.2 -------------------------------------------------------------------

scat_flat <- 
  scat %>%
  mutate(flat = ifelse(flat == 1, "yes", "no"))

# https://bookdown.org/max/FES/understanding-the-nature-and-severity-of-missing-information.html#fig:missing-pairs
ggplot(scat_flat, aes(col = flat)) + 
  geom_point(aes(x = Diameter, y = Mass), alpha = .5) + 
  geom_rug(data = scat_flat[is.na(scat_flat$Mass),], 
           aes(x = Diameter), 
           sides = "b", 
           lwd = 1)+ 
  geom_rug(data = scat_flat[is.na(scat_flat$Diameter),], 
           aes(y = Mass), 
           sides = "l", 
           lwd = 1) + 
  theme(legend.position = "top")

# Figure 8.3 -------------------------------------------------------------------

only_rides <-
  raw_entries %>%
  dplyr::select(-date)

convert_missing <- function(x) ifelse(is.na(x), 0, 1)
date_missing <- apply(only_rides, 2, convert_missing)

date_missing_ppn <-
  apply(only_rides, MARGIN = 1, function(x)
    sum(is.na(x))) / ncol(only_rides)

# PCA plot of sample space
pca_dates <- prcomp(date_missing)

pca_d <- 
  data.frame(pca_dates$x) %>%
  dplyr::select(PC1, PC2) %>%
  mutate(Percent = date_missing_ppn * 100) %>% 
  dplyr::distinct(PC1, PC2, Percent)

pca_d_rng <- extendrange(c(pca_d$PC1, pca_d$PC2))

# https://bookdown.org/max/FES/understanding-the-nature-and-severity-of-missing-information.html#fig:missing-PCA-dates
ggplot(pca_d, aes(x = PC1, y = PC2, size = Percent)) +
  geom_point(alpha = .5, col = "#1B9E77") +
  xlim(pca_d_rng) + 
  ylim(pca_d_rng) + 
  scale_size(limits = c(0, 10), range = c(.5, 10))

# Figure 8.4 ------------------------------------------------------------------------------

cols_missing <- 
  only_rides %>%
  summarise_all(list(~ sum(is.na(.)))) %>%
  mutate_all(funs(ppn = ./nrow(only_rides))) %>%
  dplyr::select(ends_with("ppn")) %>%
  gather(key = "station_label", value = ppn) %>%
  mutate(station_label = gsub("_ppn", "", station_label))

# PCA plot of predictor space
pca_stations <- prcomp(t(date_missing))

pca_s <- 
  data.frame(pca_stations$x) %>%
  dplyr::select(PC1, PC2) %>%
  bind_cols(cols_missing) %>%
  mutate(Percent = ppn * 100) %>%
  dplyr::distinct(PC1, PC2, Percent, ppn)

pca_s_rng <- extendrange(c(pca_s$PC1, pca_s$PC2))

# https://bookdown.org/max/FES/understanding-the-nature-and-severity-of-missing-information.html#fig:missing-PCA-stations
ggplot(pca_s, aes(x = PC1, y = PC2, size = Percent)) +
  geom_point(alpha = .5, col = "#D95F02") +
  xlim(pca_s_rng) + 
  ylim(pca_s_rng) + 
  scale_size_continuous(limits = c(0, 100), range = c(3, 10))

# Figure 8.5 -------------------------------------------------------------------

miss_entries <- 
  raw_entries %>%
  dplyr::select(-date) %>%
  is.na() 
miss_num <- apply(miss_entries, 2, sum)

has_missing <- vapply(raw_entries[, -1], function(x) sum(is.na(x)) > 1, logical(1))
miss_station <- names(has_missing)[has_missing]

# do clustering on just the station data (not time) and get a reordering 
# of the stations for plotting
miss_data <- 
  raw_entries[, miss_station] %>%
  is.na()

clst <- hclust(dist(t(miss_data)))
clst_stations <- 
  tibble(
    station_id = colnames(miss_data),
    order = clst$order
  )

station_names <- 
  stations %>% 
  dplyr::select(name, station_id) %>%
  right_join(clst_stations, by = "station_id") 

station_lvl <- station_names[["name"]][station_names$order]

miss_vert <-
  raw_entries %>%
  gather(station_id, raw_entries, -date) %>%
  filter(station_id %in% miss_station) %>%
  mutate(status = ifelse(is.na(raw_entries), "missing", "complete")) %>%
  full_join(station_names, by = "station_id") %>%
  mutate(
    name = factor(name, levels = station_lvl),
    status = factor(status, levels = c("missing", "complete"))
  )

# https://bookdown.org/max/FES/understanding-the-nature-and-severity-of-missing-information.html#fig:missing-chicago
ggplot(miss_vert, aes(x = date, y = name, fill = status)) + 
  geom_tile() + 
  ylab("") + xlab("") + 
  scale_fill_grey()

# ------------------------------------------------------------------------------

sessionInfo()


