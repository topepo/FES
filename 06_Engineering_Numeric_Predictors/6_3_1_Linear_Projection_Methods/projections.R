# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 6.3.1 at
# https://bookdown.org/max/FES/engineering-numeric-predictors.html#linear-projection-methods
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(caret)
library(NMF)
library(fastICA)
library(tidymodels)
library(kernlab)
library(pls)
library(RColorBrewer)
library(leaflet)
library(htmltools)
library(dimRed)
library(heatmaply)
library(lobstr)

# The memory requires for this script are about 8GB although see the note below
# regarding NNMF (which can be much higher)

mem_in_gb <- function() {
  res <- as.numeric(mem_used()) * 1e-9
  cat(round(res, 1), "GB\n")
  invisible(NULL)
}

# ------------------------------------------------------------------------------

# For plots
source("chicago_maps.R")

# ------------------------------------------------------------------------------

load("../../Data_Sets/Chicago_trains/chicago.RData")

# ------------------------------------------------------------------------------

weekends <- 
  training %>% 
  dplyr::filter(dow %in% c("Sat", "Sun")) %>%
  dplyr::select(matches("l14_[0-9]"), s_40380)

stations <- 
  stations %>% 
  mutate(terms = gsub("s_", "l14_", station_id))

mem_in_gb()

# ------------------------------------------------------------------------------
# Emulate the rolling origin forecast resampling but for weekends

weekend_days <- train_days[training$dow %in% c("Sat", "Sun")]

wend_slices <- createTimeSlices(weekend_days, initialWindow = 1600, horizon = 4, fixedWindow = FALSE)
wend_ctrl <- ctrl
wend_ctrl$index <- wend_slices$train
wend_ctrl$indexOut <- wend_slices$test
wend_ctrl$verboseIter <- FALSE
wend_ctrl$allowParallel <- FALSE

# ------------------------------------------------------------------------------

simple_rec <- recipe(s_40380 ~ ., data = weekends)

set.seed(7599)
simple_mod <- 
  train(simple_rec, data = weekends, method = "lm", trControl = wend_ctrl) %>% 
  pluck("resample") %>% 
  mutate(model = "Original Predictors")

mem_in_gb()

# ------------------------------------------------------------------------------

pca_rec <- 
  simple_rec %>%
  step_center(matches("l14_[0-9]")) %>%
  step_scale(matches("l14_[0-9]")) %>%
  step_pca(matches("l14_[0-9]"), num_comp = 20)

pca_rec_tr <- 
  pca_rec %>%
  prep(training = weekends, verbose = TRUE)

pca_features <- juice(pca_rec_tr, matches("^PC"))

pca_cor <- apply(pca_features, 2, cor, y = weekends$s_40380, method = "spearman")
pca_cor <- pca_cor[order(-abs(pca_cor))]
pca_cor

mem_in_gb()

# ------------------------------------------------------------------------------

# Compute the % variation for each component
sds <- pca_rec_tr$steps[[3]]$res$sdev
pct_var <- (sds^2)/sum(sds^2)*100
cum_var <- cumsum(pct_var)

# Get component values
pca_coefs <- 
  tidy(pca_rec_tr, number = 3) %>%
  dplyr::select(-id) %>% 
  spread(component, value) %>% 
  dplyr::select(terms, PC1, PC2, PC3, PC4, PC5) %>%  
  full_join(stations, by = "terms")

# Get a different version used for the heatmap
five_pca_coefs <- 
  pca_coefs %>% 
  as.data.frame() %>% 
  na.omit()

rownames(five_pca_coefs) <- five_pca_coefs$description

five_pca_coefs <- 
  five_pca_coefs %>%  
  dplyr::select(starts_with("PC"))

# ------------------------------------------------------------------------------

five_comps_range <-
  tidy(pca_rec, number = 3) %>% 
  mutate(value = abs(value)) %>% 
  pull(value) %>% 
  max(na.rm = TRUE)

# https://bookdown.org/max/FES/engineering-numeric-predictors.html#fig:numeric-heatmap-html
# heatmaply(
#   five_pca_coefs,
#   cexRow = .5,
#   Colv = FALSE,
#   branches_lwd = .5,
#   colors = RdBu(50),
#   label_format_fun = function(...) round(..., digits = 3),
#   limits = c(-five_comps_range, five_comps_range),
#   margins = c(50,215,10,150)
# )

# https://bookdown.org/max/FES/engineering-numeric-predictors.html#fig:numeric-pc2-map-html
# chicago_map(pca_coefs, plot_var = "PC2")

# ------------------------------------------------------------------------------

set.seed(7599)
pca_mod <- 
  train(pca_rec, data = weekends, method = "lm", trControl = wend_ctrl) %>% 
  pluck("resample") %>% 
  mutate(model = "PCA")

rm(pca_rec, pca_rec_tr)

mem_in_gb()

# ------------------------------------------------------------------------------

# Determine a reasonable value for the radial basis function parameter sigma
sig_range <- 
  simple_rec %>%
  step_center(matches("l14_[0-9]")) %>%
  step_scale(matches("l14_[0-9]")) %>%
  prep(training = weekends, verbose = TRUE) %>%
  juice(matches("l14_[0-9]")) %>%
  as.matrix() %>%
  sigest(frac = 1) 

kpca_rec <- 
  simple_rec %>%
  step_center(matches("l14_[0-9]")) %>%
  step_scale(matches("l14_[0-9]")) %>%
  step_kpca(
    matches("l14_[0-9]"), 
    num_comp = 20, 
    options = list(kernel = "rbfdot", kpar = list(sigma = sig_range[2]))
  ) 

kpca_rec_tr <- 
  kpca_rec %>%
  prep(training = weekends, retain = TRUE, verbose = TRUE)

kpca_features <- juice(kpca_rec_tr, matches("PC"))

kpca_cor <- apply(kpca_features, 2, cor, y = weekends$s_40380, method = "spearman")
kpca_cor <- kpca_cor[order(-abs(kpca_cor))]
kpca_cor

mem_in_gb()

# ------------------------------------------------------------------------------

set.seed(7599)
kpca_mod <- 
  train(kpca_rec, data = weekends, method = "lm", trControl = wend_ctrl) %>% 
  pluck("resample") %>% 
  mutate(model = "kPCA")

rm(kpca_rec, kpca_rec_tr)

mem_in_gb()

# ------------------------------------------------------------------------------

# Create some reproducible random numbers for starting values
set.seed(1257)
ica_start <- matrix(rnorm(20^2), nrow = 20)

ica_rec <- 
  simple_rec %>%
  step_ica(
    matches("l14_[0-9]"),
    num_comp = 20,
    options = list(
      maxit = 1000,
      tol = 1e-10,
      alg.type = "deflation",
      w.init = ica_start
    )
  ) 

ica_rec_tr <- 
  ica_rec %>%
  prep(training = weekends, verbose = TRUE)

ica_features <- juice(ica_rec_tr, matches("IC"))

ica_cor <- apply(ica_features, 2, cor, y = weekends$s_40380, method = "spearman")
ica_cor <- ica_cor[order(-abs(ica_cor))]
ica_cor

ica_coefs <- 
  tidy(ica_rec_tr, number = 1) %>%
  dplyr::select(-id) %>% 
  spread(component, value) %>% 
  dplyr::select(terms, IC01, IC02, IC03) %>%  
  full_join(stations, by = "terms")

mem_in_gb()

# ------------------------------------------------------------------------------

set.seed(7599)
ica_mod <- 
  train(ica_rec, data = weekends, method = "lm", trControl = wend_ctrl) %>% 
  pluck("resample") %>% 
  mutate(model = "ICA")

rm(ica_rec, ica_rec_tr)

mem_in_gb()

# ------------------------------------------------------------------------------

# By default, the next section computes the NNMF results in parallel and *each 
# worker* uses about 1GB of memory (it will automatically try to use as many
# cores as `num_run`). See the details in ?NMF::nmf. This can be turned off by
# the `options` argument to `step_nnmf()`. 

nnmf_rec <- 
  simple_rec %>%
  step_scale(matches("l14_[0-9]")) %>%
  step_nnmf(matches("l14_[0-9]"), num_comp = 20, seed = 17202, num_run = 20) 

nnmf_rec_tr <- 
  nnmf_rec %>%
  prep(training = weekends, verbose = TRUE)

nnmf_features <- juice(nnmf_rec_tr)

nnmf_cor <- apply(nnmf_features[, -1], 2, cor, y = nnmf_features$s_40380, method = "spearman")
nnmf_cor <- nnmf_cor[order(-abs(nnmf_cor))]
nnmf_cor

# As of this writing, the `tidy` method isn't working for `step_nnmf()`
nnmf_coefs <- 
  nnmf_rec_tr$steps[[2]]$res@other.data$w %>%
  as.data.frame() %>%
  rownames_to_column(var = "terms") %>%  
  full_join(stations, by = "terms")

# https://bookdown.org/max/FES/engineering-numeric-predictors.html#fig:numeric-nnmf3-map-html
# chicago_map(nnmf_coefs, plot_var = names(nnmf_cor)[1])

mem_in_gb()

# ------------------------------------------------------------------------------

set.seed(7599)
nnmf_mod <- 
  train(nnmf_rec, data = weekends, method = "lm", trControl = wend_ctrl) %>% 
  pluck("resample") %>% 
  mutate(model = "NNMF")

rm(nnmf_rec, nnmf_rec_tr)

mem_in_gb()

# ------------------------------------------------------------------------------

pls_rec <- 
  simple_rec %>%
  step_center(matches("l14_[0-9]")) %>%
  step_scale(matches("l14_[0-9]")) %>%
  step_pls(matches("l14_[0-9]"), outcome = "s_40380", num_comp = 20)

pls_rec_tr <- 
  pls_rec %>%
  prep(training = weekends, verbose = TRUE)

pls_features <- juice(pls_rec_tr, matches("PLS"), s_40380)

pls_cor <- apply(pls_features, 2, cor, y = weekends$s_40380, method = "spearman")
pls_cor <- pls_cor[order(-abs(pls_cor))]
pls_cor

# ------------------------------------------------------------------------------

set.seed(7599)
pls_mod <- 
  train(pls_rec, data = weekends, method = "lm", trControl = wend_ctrl) %>% 
  pluck("resample") %>% 
  mutate(model = "PLS")

rm(pls_rec, pls_rec_tr)

mem_in_gb()

# ------------------------------------------------------------------------------

# Merge the first five features from each method for plotting
comps <- 
  pca_features %>%
  dplyr::select(num_range("PC0", 1:5)) %>% 
  bind_cols(ica_features %>% dplyr::select(num_range("IC0", 1:5))) %>%
  bind_cols(kpca_features %>% dplyr::select(num_range("kPC0", 1:5))) %>%
  bind_cols(nnmf_features %>% dplyr::select(num_range("NNMF0", 1:5))) %>%
  bind_cols(pls_features %>% dplyr::select(num_range("PLS0", 1:5), s_40380)) %>% 
  gather(name, value, -s_40380) %>% 
  mutate(
    Type = gsub("[[:digit:]]", "", name),
    Type = factor(Type, levels = c("PC", "kPC", "IC", "NNMF", "PLS")),
    Comp = paste("Component", gsub("([[:alpha:]])|(0)", "", name))
  )

# https://bookdown.org/max/FES/engineering-numeric-predictors.html#fig:numeric-weekend-scores
# ggplot(comps, aes(x = value, y = s_40380)) + 
#   geom_point(cex = .3, alpha = .5) + 
#   facet_grid(Comp ~ Type, scale = "free_x")

# ------------------------------------------------------------------------------

# Collate the resampling results together for interval estimates
model_res <- 
  simple_mod  %>% 
  bind_rows(pca_mod) %>% 
  bind_rows(kpca_mod) %>% 
  bind_rows(ica_mod) %>% 
  bind_rows(nnmf_mod) %>% 
  bind_rows(pls_mod)

# https://bookdown.org/max/FES/engineering-numeric-predictors.html#fig:numeric-weekend-ci
# model_res %>% 
#   group_by(model) %>% 
#   do(broom::tidy(t.test(.$RMSE))) %>% 
#   ggplot() + 
#   aes(x = reorder(model, estimate), y = estimate) + 
#   geom_point() + 
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .3) + 
#   coord_flip() + 
#   xlab("") + 
#   ylab("RMSE (resampled)")


# ------------------------------------------------------------------------------

save(weekends, file = "weekends.RData")
save(model_res, comps, sig_range, ica_coefs, ica_cor, nnmf_cor, file = "lin_proj_res.RData")
save(five_pca_coefs, pca_coefs, cum_var, file = "pca_res.RData")
save(nnmf_coefs, file = "nnmf_res.RData")

# ------------------------------------------------------------------------------

sessionInfo()

q("no")

