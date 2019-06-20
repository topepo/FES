# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Functions required for Section 7.5.3 at
# https://bookdown.org/max/FES/detecting-interaction-effects.html#approaches-when-complete-enumeration-is-practically-impossible
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(tidymodels)
library(furrr)
library(stringr)
library(cli)
library(crayon)

# ------------------------------------------------------------------------------

fsa_two_way <- function(rs, recipe, model, perf, 
                        fsa_control = list(
                          max_iter = 50, 
                          max_samples = 30, 
                          improve = c(p = 0.1, pct = 0),
                          maximize = FALSE,
                          verbose = TRUE
                        ),
                        ...) {

  if (interactive()) {
    good <- crayon::green(cli::symbol$tick)
  } else {
    good <- "+"
  }

  data <- rs %>% pull(splits) %>% pluck(1) %>% pluck("data")
  
  train_seed <- sample.int(10000, 5)
  
  orig_recipe <- recipe
  
  # prep the recipe across resamples that will be updated later
  if (fsa_control$verbose)
    cat("Prepping on entire data set\n")
  
  orig_recipe <- prep(orig_recipe, training = data)
  
  recipe_vars <- summary(orig_recipe)
  x_names <- 
    recipe_vars %>% 
    dplyr::filter(role == "predictor") %>% 
    pull(variable)
  x_print <- format(x_names)
  
  if (fsa_control$max_samples >= length(x_names) - 2)
    fsa_control$max_samples <- length(x_names) - 2
  
  # not sure if we need this
  nominal_vars <- 
    recipe_vars %>%
    dplyr::filter(type == "nominal" & role == "predictor") %>%
    pull(variable)
  
  if (length(nominal_vars) > 0) {
    rep_steps <- tidy(orig_recipe)
    if (!any(rep_steps$type == "dummy")) {
      main_recipe <- 
        orig_recipe %>%
        step_dummy(all_nominal(), -all_outcomes())
    }
  } else main_recipe <- orig_recipe
  
  # prep the recipe across resamples that will be updated later
  if (fsa_control$verbose)
    cat("Prepping and modeling across resamples\n")
  
  rs <- 
    rs %>% 
    mutate(
      orig_rec = map(splits, prepper, orig_recipe, fresh = TRUE),
      orig_perf = map2(splits, orig_rec, get_perf, model = model, perf = perf))
  
  orig_cv_perf <- map_dbl(rs$orig_perf, pull, 1)
  orig_mean_perf <- mean(orig_cv_perf, na.rm = TRUE)
  
  rand_pair <- 
    tibble(iter = 0:fsa_control$max_iter) %>%
    mutate(var_1 = NA_character_,
           var_2 = NA_character_,
           seeds = sample.int(10000, fsa_control$max_iter + 1),
           perf = NA_real_,
           pval = NA_real_,
           change = NA_real_,
           swaps  = NA_integer_)
  rand_pair$perf[1] <- orig_mean_perf
  
  for (iter in 2:nrow(rand_pair)) {
    ruler(iter)

    set.seed(rand_pair$seeds[[iter]])
    
    base_pair <- select_pairs(orig_recipe)
    base_form <- 
      paste0("~", paste0(base_pair, collapse = ":")) %>% 
      as.formula()
    
    rs <- 
      rs %>% 
      mutate(
        base_rec = map2(splits, orig_rec, add_int, base_form),
        base_perf = map2(splits, base_rec, get_perf, model = model, perf = perf))
    
    base_res <- 
      rs %>% 
      compare_res("orig_perf", "base_perf", fsa_control)
    
    rs <- 
      rs %>% 
      mutate(current_perf = base_perf)
    
    best_pairs <- base_pair
    best_res <- base_res
    
    no_improve <- 0
    fixed_var <- base_pair$var_1
    prev <- base_pair$var_2
    swaps <- 0
    
    if (fsa_control$verbose)
      cat("Finding match for", fixed_var, "\n")
    
    for (iter_1 in 1:fsa_control$max_samples) {
      
      cand_pair <- select_pairs(orig_recipe, fixed = fixed_var, exclude = prev)
      cand_form <- 
        paste0("~", paste0(cand_pair, collapse = ":")) %>% 
        as.formula()
      prev <- c(prev, cand_pair$var_2)
      
      rs <- 
        rs %>% 
        mutate(
          cand_rec = map2(splits, orig_rec, add_int, cand_form),
          cand_perf = map2(splits, cand_rec, get_perf, model = model, perf = perf))
      
      cand_res <- 
        rs %>% 
        compare_res("current_perf", "cand_perf", ctrl = fsa_control)
      
      if (cand_res$improved) {
        no_improve <- 0
        best_pairs <- cand_pair
        best_res <- cand_res
        rs <- 
          rs %>% 
          mutate(current_perf = cand_perf)
        if (fsa_control$verbose) {
          cat(" ", good, " ", 
              x_print[x_names == best_pairs$var_2],
              " ", 
              signif(best_res$new, 3), 
              "\n", sep = "")
        }
      } else {
        no_improve <- no_improve + 1
        swaps <- swaps + 1
      }
    }
    
    no_improve <- 0
    
    # we always vary the first elem
    tmp <- best_pairs
    tmp$var_1 <- best_pairs$var_2
    tmp$var_2 <- best_pairs$var_1
    
    best_pairs <- tmp
    fixed_var <- best_pairs$var_1
    prev <- best_pairs$var_2
    
    if (fsa_control$verbose)
      cat("Finding match for", fixed_var, "\n")
    
    for (iter_2 in 1:fsa_control$max_samples) {
      
      cand_pair <- select_pairs(orig_recipe, fixed = fixed_var, exclude = prev)
      cand_form <- 
        paste0("~", paste0(cand_pair, collapse = ":")) %>% 
        as.formula()
      prev <- c(prev, cand_pair$var_2)
      
      rs <- 
        rs %>% 
        mutate(
          cand_rec = map2(splits, orig_rec, add_int, cand_form),
          cand_perf = future_map2(splits, cand_rec, get_perf, model = model, perf = perf))
      
      cand_res <- 
        rs %>% 
        compare_res("current_perf", "cand_perf", ctrl = fsa_control)
      
      if (cand_res$improved) {
        no_improve <- 0
        best_pairs <- cand_pair
        best_res <- cand_res
        rs <- 
          rs %>% 
          mutate(current_perf = cand_perf)
        if (fsa_control$verbose) {
          cat(" ", good, " ",
              x_print[x_names == best_pairs$var_2],
              " ", 
              signif(best_res$new, 3), 
              "\n", sep = "")
        }
      } else {
        no_improve <- no_improve + 1
        swaps <- swaps + 1
      }
    }    
    
    final_res <- 
      rs %>% 
      compare_res("base_perf", "current_perf", fsa_control)
    
    sorted <- sort(c(best_pairs$var_1, best_pairs$var_2))
    rand_pair$perf[[iter]] <- final_res$new
    rand_pair$var_1[[iter]] <- sorted[1]
    rand_pair$var_2[[iter]] <- sorted[2]
    rand_pair$swaps[[iter]] <- swaps
    rand_pair$pval[[iter]] <- final_res$p.value
    rand_pair$change[[iter]] <- final_res$pct_diff
    
    if (fsa_control$verbose) {
      cat("\n")
      show_rows <- iter
      rand_pair[c(1,show_rows),] %>% 
        dplyr::select(-seeds, -change) %>% 
        dplyr::rename(p_value = pval) %>% 
        print()
      cat("\n\n")
    }
  }
  rand_pair
}

# ----------------------------------------------------------------

get_dummy_columns <- function(x) {
  tibble(values = attr(x, "values"))
}

tidy_dummies <- function(x, ...) {
  if (is_trained(x)) {
    res <- 
      purrr::map_dfr(x$levels, get_dummy_columns, .id = "terms")
  } else {
    res <- tibble(terms = sel2char(x$terms), values = rlang:::na_chr)
  }
  res %>% 
    mutate(
      columns = x$naming(terms, values),
      id = x$id
    )
}

# Placeholder for when we have more indicator variable steps
dummies <- "dummy"

select_pairs <- function(recipe, fixed = NULL, exclude = NULL) {
  vars <- 
    summary(recipe) %>% 
    dplyr::filter(role == "predictor") %>% 
    pull(variable)
  if (!is.null(fixed))
    vars <- vars[vars != fixed]
  if (!is.null(exclude))
    vars <- vars[!(vars %in% unique(exclude))]
  
  rec_steps <- tidy(recipe)
  dummy_steps <- which(rec_steps$type %in% dummies)
  if (length(dummy_steps) > 0) {
    mapping <- tidy_dummies(recipe$steps[[dummy_steps]])
  } else {
    mapping <- NULL
  }
  
  if (length(dummy_steps) > 0) {
    # Sample the first column then check to see if it was
    # created as a dummy variable. If it was, remove the other
    # indicator variables in that set from selection. Then select 
    # the second. 
    
    if (!is.null(fixed)) {
      selected <- fixed
    } else {
      selected <- sample(vars, 1)
    }
    
    if (!is.null(mapping) && any(mapping$columns == selected)) {
      orig_var <- mapping$terms[mapping$columns == selected]
      reject <- mapping$columns[mapping$terms == orig_var]
      vars <- vars[!(vars %in% reject)]
    }
    selected <- c(selected, sample(vars, 1))
  } else {
    if (!is.null(fixed)) {
      selected <- c(fixed, sample(vars, 1))
    } else {
      selected <- sample(vars, 2)
    }
  }
  tibble(var_1 = selected[1], var_2 = selected[2])
}

get_perf <- function(split, recipe, model, perf) {
  analysis_pp <- juice(recipe)
  assess_pp <- bake(recipe, assessment(split))
  
  y_var <- 
    summary(recipe) %>% 
    dplyr::filter(role == "outcome") %>%
    pull(variable)
  mod <- 
    fit_xy(model, 
           x = analysis_pp %>% dplyr::select(-one_of(y_var)),
           y = analysis_pp %>% dplyr::select(one_of(y_var)) %>% pull(1),
           control = fit_control(catch = TRUE))
  
  preds <- 
    assess_pp %>% 
    dplyr::select(one_of(y_var)) %>% 
    bind_cols(predict(mod, new_data = assess_pp %>% dplyr::select(-one_of(y_var)))) %>% 
    dplyr::rename(obs = !!y_var)
  
  if (model$mode == "classification") {
    probs <-
      predict(mod,
              new_data = assess_pp %>% dplyr::select(-one_of(y_var)),
              type = "prob")
    preds <- bind_cols(preds, probs)
    
    pred_name <- ".pred_class"
    prob_names <- names(probs)
    res <- perf(preds, obs, estimate = !!pred_name, !!!prob_names)
  } else {
    pred_name <- ".pred"
    res <- perf(preds, obs, estimate = !!pred_name) 
  }
  
  res %>% 
    dplyr::select(-.estimator) %>% 
    spread(.metric, .estimate)
}


# ------------------------------------------------------------------------------


add_int <- function(split, recipe, int, ...) {
  recipe %>% 
    step_interact(int) %>% 
    prep(training = analysis(split), ...)
}

compare_res <- function(object, main, int, ctrl) {
  res_data <- 
    tibble(
      old = map_dbl(object %>% pull(main), pull, 1),
      new = map_dbl(object %>% pull(int), pull, 1)
    )
  old_mean <- mean(res_data$old, na.rm = TRUE)
  new_mean <- mean(res_data$new, na.rm = TRUE)
  
  # alternative = "greater" is the alternative that x has a 
  # larger mean than y.
  alt <- ifelse(ctrl$maximize, "greater", "less")
  res <- 
    t.test(
      x = res_data$new, 
      y = res_data$old, 
      paired = TRUE, 
      alternative = alt
    ) %>% 
    broom::tidy() %>% 
    mutate(
      pct_diff = (new_mean - old_mean)/old_mean*100,
      old = old_mean,
      new = new_mean)
  
  better_p <- res$p.value <= ctrl$improve["p"]
  better_pct <- abs(res$pct_diff) >= ctrl$improve["pct"]
  if (ctrl$maximize) {
    res$improved <- (res$new > res$old) & better_p & better_pct
    
  } else {
    res$improved <- (res$new < res$old) & better_p & better_pct
  }

  res
}

ruler <- function(i) {
  if (interactive()) { 
    res <- cli::rule(left = paste("Iteration", i - 1))
    print(res)
  } else {
    wd <- options()$width - 13 - nchar(paste(i - 1))
    res <- str_c("-- Iteration", i - 1, str_flatten(rep("-", wd)), "\n", sep = " ")
    cat(res)
  }
  invisible(NULL)
}


