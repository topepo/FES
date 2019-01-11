library(tidyverse)
library(psych)
library(zoo)

# ------------------------------------------------------------------------------

load("small_scale.RData")
load("large_scale.RData")

small_scale <- 
  small_scale %>% 
  dplyr::rename(Reactor = BatchID, Day = Sample)

large_scale <- 
  large_scale %>% 
  dplyr::rename(Reactor = BatchID, Day = Sample)

# Make versions of the data where the spectra are stacked ----------------------
small_info <-
  small_scale %>%
  dplyr::select(Reactor, Day, BatchSample, Glucose)

small_x <- 
  small_scale %>%
  dplyr::select(-Glucose)

small_x_long <- 
  gather(small_x, key = spec_num, value = y, `400`:`3050`) %>%
  mutate(spec_num = as.numeric(spec_num),
         Type = "Small")

large_info <-
  large_scale %>%
  dplyr::select(Reactor, Day, BatchSample, Glucose)

large_x <- large_scale %>%
  dplyr::select(-Glucose)

large_x_long <- 
  gather(large_x, key = spec_num, value = y, `400`:`3050`) %>%
  mutate(spec_num = as.numeric(spec_num),
         Type = "Large")

small_large_long <-
  bind_rows(small_x_long, large_x_long) %>%
  mutate(Reactor = factor(Reactor),
         BatchSample = factor(BatchSample),
         Type = factor(Type, levels = c("Small", "Large")))

# Polynomial baseline estimation -----------------------------------------------

poly_baseline <- function(dat, min_points = 50, num_iter = 10, degree = 5){
  orig_data <- dat
  sub_data <- dat
  min_points <- min_points
  current_formula <- as.formula(paste0("y ~ poly(spec_num,", degree, ")"))
  for (i in 1:num_iter){
    current_poly <- lm(current_formula, data = sub_data)
    
    sub_data <-
      orig_data %>%
      mutate(pred = predict(current_poly, orig_data)) %>%
      dplyr::filter(pred > y)
    
    if(nrow(sub_data) < min_points) break
  }
  
  baseline_data <- 
    dat %>%
    mutate(baseline = predict(current_poly, dat))
  
  baseline_data
}

small_by_batch <- split(small_x_long, small_x_long$BatchSample)
small_baseline <- 
  map_dfr(small_by_batch, poly_baseline) %>%
  mutate(y = y - baseline) %>% 
  dplyr::select(-baseline)

large_by_batch <- split(large_x_long, large_x_long$BatchSample)
large_baseline <- 
  map_dfr(large_by_batch, poly_baseline) %>%
  mutate(y = y - baseline) %>% 
  dplyr::select(-baseline)

# Reduce the noise in the spectra ----------------------------------------------

standardize <- function(x) {
  (x - mean(x, trim = 0.05)) / psych::winsor.sd(x, trim = 0.05)
}

small_standardized <-
  small_baseline %>%
  group_by(BatchSample) %>%
  mutate(y = standardize(y))

large_standardized <-
  large_baseline %>%
  group_by(BatchSample) %>%
  mutate(y = standardize(y)) 

small_smoothed <-
  small_standardized %>%
  group_by(BatchSample) %>%
  mutate(y = zoo::rollmean(y, k = 15, na.pad = TRUE))

large_smoothed <-
  large_standardized %>%
  group_by(BatchSample) %>%
  mutate(y = zoo::rollmean(y, k = 15, na.pad = TRUE)) 

# Reduce Correlation -----------------------------------------------------------

small_derivative <-
  small_smoothed %>%
  group_by(BatchSample) %>%
  mutate(smoothed_lag1 = lag(y, n = 1),
         y = y - smoothed_lag1) %>% 
  dplyr::select(-smoothed_lag1)

large_derivative <-
  large_smoothed %>%
  group_by(BatchSample) %>%
  mutate(smoothed_lag1 = lag(y, n = 1),
         y = y - smoothed_lag1)%>% 
  dplyr::select(-smoothed_lag1)

# Convert back to long format where spectra are variables and also remove ------
# wavelengths with missing values in the small or large reactor data -----------

small_none <-
  small_x_long %>%
  mutate(
    label = format(spec_num),
    label = gsub(" ", "0", label),
    label = paste0("w_", label)
  ) %>% 
  dplyr::select(-spec_num) %>% 
  spread(key = label, value = y) %>% 
  full_join(small_info, by = c("Reactor", "Day", "BatchSample")) %>% 
  dplyr::select(-Type, -BatchSample)

large_none <-
  large_x_long %>%
  mutate(
    label = format(spec_num),
    label = gsub(" ", "0", label),
    label = paste0("w_", label)
  ) %>% 
  dplyr::select(-spec_num) %>% 
  spread(key = label, value = y) %>% 
  full_join(large_info, by = c("Reactor", "Day", "BatchSample")) %>% 
  dplyr::select(-Type, -BatchSample)

small_none_na <- map_dbl(small_none, function(x) mean(is.na(x)))
large_none_na <- map_dbl(large_none, function(x) mean(is.na(x)))

if (any(small_none_na > 0) | any(large_none_na > 0)) {
  removals <- 
    c(names(small_none_na)[small_none_na > 0], 
      names(large_none_na)[large_none_na > 0])
  removals <- unique(removals)
  small_none <-
    small_none %>% 
    dplyr::select(-!!!removals)
}

save(small_none, large_none, file = "no_processing.RData")


small_baseline <-
  small_baseline %>%
  mutate(
    label = format(spec_num),
    label = gsub(" ", "0", label),
    label = paste0("w_", label)
  ) %>% 
  dplyr::select(-spec_num) %>% 
  spread(key = label, value = y) %>% 
  full_join(small_info, by = c("Reactor", "Day", "BatchSample")) %>% 
  dplyr::select(-Type, -BatchSample)

large_baseline <-
  large_baseline %>%
  mutate(
    label = format(spec_num),
    label = gsub(" ", "0", label),
    label = paste0("w_", label)
  ) %>% 
  dplyr::select(-spec_num) %>% 
  spread(key = label, value = y) %>% 
  full_join(large_info, by = c("Reactor", "Day", "BatchSample")) %>% 
  dplyr::select(-Type, -BatchSample)


small_baseline_na <- map_dbl(small_baseline, function(x) mean(is.na(x)))
large_baseline_na <- map_dbl(large_baseline, function(x) mean(is.na(x)))

if (any(small_baseline_na > 0) | any(large_baseline_na > 0)) {
  removals <- 
    c(names(small_baseline_na)[small_baseline_na > 0], 
      names(large_baseline_na)[large_baseline_na > 0])
  removals <- unique(removals)
  small_baseline <-
    small_baseline %>% 
    dplyr::select(-!!!removals)
}

save(small_baseline, large_baseline, file = "baseline_corr.RData") 


small_standardized <-
  small_standardized %>%
  mutate(
    label = format(spec_num),
    label = gsub(" ", "0", label),
    label = paste0("w_", label)
  ) %>% 
  ungroup() %>% 
  dplyr::select(-spec_num) %>% 
  spread(key = label, value = y) %>% 
  full_join(small_info, by = c("Reactor", "Day", "BatchSample")) %>% 
  dplyr::select(-Type, -BatchSample)

large_standardized <-
  large_standardized %>%
  mutate(
    label = format(spec_num),
    label = gsub(" ", "0", label),
    label = paste0("w_", label)
  ) %>% 
  ungroup() %>% 
  dplyr::select(-spec_num) %>% 
  spread(key = label, value = y) %>% 
  full_join(large_info, by = c("Reactor", "Day", "BatchSample")) %>% 
  dplyr::select(-Type, -BatchSample)

small_standardized_na <- map_dbl(small_standardized, function(x) mean(is.na(x)))
large_standardized_na <- map_dbl(large_standardized, function(x) mean(is.na(x)))

if (any(small_standardized_na > 0) | any(large_standardized_na > 0)) {
  removals <- 
    c(names(small_standardized_na)[small_standardized_na > 0], 
      names(large_standardized_na)[large_standardized_na > 0])
  removals <- unique(removals)
  small_standardized <-
    small_standardized %>% 
    dplyr::select(-!!!removals)
}

save(small_standardized, large_standardized, file = "standardized.RData")

small_smoothed <-
  small_smoothed %>%
  mutate(
    label = format(spec_num),
    label = gsub(" ", "0", label),
    label = paste0("w_", label)
  ) %>% 
  ungroup() %>% 
  dplyr::select(-spec_num) %>% 
  spread(key = label, value = y) %>% 
  full_join(small_info, by = c("Reactor", "Day", "BatchSample")) %>% 
  dplyr::select(-Type, -BatchSample)

large_smoothed <-
  large_smoothed %>%
  mutate(
    label = format(spec_num),
    label = gsub(" ", "0", label),
    label = paste0("w_", label)
  )  %>% 
  ungroup()%>% 
  dplyr::select(-spec_num) %>% 
  spread(key = label, value = y) %>% 
  full_join(large_info, by = c("Reactor", "Day", "BatchSample")) %>% 
  dplyr::select(-Type, -BatchSample)

small_smoothed_na <- map_dbl(small_smoothed, function(x) mean(is.na(x)))
large_smoothed_na <- map_dbl(large_smoothed, function(x) mean(is.na(x)))

if (any(small_smoothed_na > 0) | any(large_smoothed_na > 0)) {
  removals <- 
    c(names(small_smoothed_na)[small_smoothed_na > 0], 
      names(large_smoothed_na)[large_smoothed_na > 0])
  removals <- unique(removals)
  small_smoothed <-
    small_smoothed %>% 
    dplyr::select(- !!removals)
}

save(small_smoothed, large_smoothed, file = "smoothed.RData")

small_derivative <-
  small_derivative %>%
  mutate(
    label = format(spec_num),
    label = gsub(" ", "0", label),
    label = paste0("w_", label)
  ) %>% 
  ungroup() %>% 
  dplyr::select(-spec_num) %>% 
  spread(key = label, value = y) %>% 
  full_join(small_info, by = c("Reactor", "Day", "BatchSample")) %>% 
  dplyr::select(-Type, -BatchSample)

large_derivative <-
  large_derivative %>%
  mutate(
    label = format(spec_num),
    label = gsub(" ", "0", label),
    label = paste0("w_", label)
  ) %>% 
  ungroup() %>% 
  dplyr::select(-spec_num) %>% 
  spread(key = label, value = y) %>% 
  full_join(large_info, by = c("Reactor", "Day", "BatchSample")) %>% 
  dplyr::select(-Type, -BatchSample)

small_derivative_na <- map_dbl(small_derivative, function(x) mean(is.na(x)))
large_derivative_na <- map_dbl(large_derivative, function(x) mean(is.na(x)))

if (any(small_derivative_na > 0) | any(large_derivative_na > 0)) {
  removals <- 
    c(names(small_derivative_na)[small_derivative_na > 0], 
      names(large_derivative_na)[large_derivative_na > 0])
  removals <- unique(removals)
  small_derivative <-
    small_derivative %>% 
    dplyr::select(- !!removals)
}

save(small_derivative, large_derivative, file = "derivatives.RData")


# Create resampling folds where 12 reactors are modeled and 3 are predicted ----

# BatchID is reactor
# Sample is day 
combos <- utils::combn(x = unique(small_baseline$Reactor), 12)
sel_combos <- floor(seq.int(1, ncol(combos), length.out = 25))
combos <- combos[, sel_combos]
modeling_ind <- 
  apply(combos, 2, function(x, dat) which(dat %in% x), dat = small_baseline$Reactor)
resample_ind <- 
  modeling_ind %>% 
  as.data.frame() %>% 
  setNames(caret:::prettySeq(1:25)) %>% 
  as.list()

save(resample_ind, file = "resample_ind.RData")

# ------------------------------------------------------------------------------

sessionInfo()

# ------------------------------------------------------------------------------

if (!interactive())
  q("no")





