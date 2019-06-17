library(caret)
library(tidymodels)

rd_files <- list.files(pattern = "RData$")
rd_files <- rd_files[rd_files != "appropriate_results.RData"]

rs_res <- large_res <- vector(mode = "list", length = length(rd_files))

pls_res <- NULL

for (i in seq_along(rd_files)) {
  load(rd_files[i])
  
  rs_res[[i]] <- 
    t.test(mod_fit$resample$RMSE) %>% 
    broom::tidy() %>% 
    dplyr::select(RMSE = estimate, conf.low, conf.high) %>% 
    mutate(
      Model = !!model,
      Transform = !!predictors
    ) %>% 
    as_tibble()
  
  if(model == "PLS") {
    pls_res <- 
      pls_res %>% 
      bind_rows(
        mod_fit %>% 
          pluck("results") %>% 
          mutate(
            Model = !!model,
            Transform = !!predictors
          ) %>% 
          as_tibble()
      )
  }
  
  large_res[[i]] <- 
  test_pred %>% 
    mutate(
      Model = !!model,
      Transform = !!predictors
    )

  rm(model, mod_fit, predictors, test_pred)
}

rs_res <- 
  rs_res %>% 
  bind_rows() %>% 
  mutate(
    Transform = factor(Transform, levels = c("no processing", "baseline", "standardized", "smoothed", "derivatives"))
    )

large_res <- 
  large_res %>% 
  bind_rows() %>% 
  mutate(
    Transform = factor(Transform, levels = c("no processing", "baseline", "standardized", "smoothed", "derivatives")),
    Day = factor(Day, levels = paste(1:14))
  )
large_rng <- extendrange(c(large_res$Predicted, large_res$Glucose))

save(rs_res, large_res, large_rng, pls_res, file = "appropriate_results.RData")

sessionInfo()

if (!interactive())
  q("no")
