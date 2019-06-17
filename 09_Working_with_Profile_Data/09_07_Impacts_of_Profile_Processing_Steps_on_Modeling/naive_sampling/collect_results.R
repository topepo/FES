library(caret)
library(tidymodels)

rd_files <- list.files(pattern = "RData$")
rd_files <- rd_files[rd_files != "naive_results.RData"]

rs_res_naive <- large_res_naive <- vector(mode = "list", length = length(rd_files))

pls_res_naive <- NULL

for (i in seq_along(rd_files)) {
  load(rd_files[i])
  
  rs_res_naive[[i]] <- 
    t.test(mod_fit$resample$RMSE) %>% 
    broom::tidy() %>% 
    dplyr::select(RMSE = estimate, conf.low, conf.high) %>% 
    mutate(
      Model = !!model,
      Transform = !!predictors
    ) %>% 
    as_tibble()
  
  if(model == "PLS") {
    pls_res_naive <- 
      pls_res_naive %>% 
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
  
  large_res_naive[[i]] <- 
  test_pred %>% 
    mutate(
      Model = !!model,
      Transform = !!predictors
    )

  rm(model, mod_fit, predictors, test_pred)
}

rs_res_naive <- 
  rs_res_naive %>% 
  bind_rows() %>% 
  mutate(
    Transform = factor(Transform, levels = c("no processing", "baseline", "standardized", "smoothed", "derivatives"))
    )

large_res_naive <- 
  large_res_naive %>% 
  bind_rows() %>% 
  mutate(
    Transform = factor(Transform, levels = c("no processing", "baseline", "standardized", "smoothed", "derivatives")),
    Day = factor(Day, levels = paste(1:14))
  )

save(rs_res_naive, large_res_naive, pls_res_naive, file = "naive_results.RData")

sessionInfo()

if (!interactive())
  q("no")
