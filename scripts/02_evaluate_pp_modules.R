library(tidyverse)
library(jnds)
library(rjson)
library(parallel)


# Functions ---------------------------------------------------------------

cv_idx_generate <- function(n, n_folds, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  full_idx <- 1:n
  cv_idx <- list()
  for (i in seq_len(n_folds - 1)) {
    temp_idx <- sample(full_idx, n / n_folds, replace = FALSE)
    cv_idx[[i]] <- temp_idx
    full_idx <- setdiff(full_idx, temp_idx)
  }
  cv_idx[[n_folds]] <- sample(full_idx, length(full_idx), replace = FALSE)
  cv_idx
}

eval_pp <- function(pp, data, n_folds = 10, n_iters = 480, n_cores = 8, seed = 42) {
  data <- predict(pp, data = data)
  cv_idx <- cv_idx_generate(nrow(data), n_folds, seed = seed)
  
  rf_time <- 0
  rf_pred <- rep(NA, nrow(data))
  cat("Cross validation\n")
  for (k in seq_len(n_folds)) {
    cat("fold = ", k, "\n", sep = "")

    # split data into training & validation set
    val <- data[cv_idx[[k]], ]
    train <- setdiff(data, val)
    x_train <- select(train, -price)
    y_train <- train$price
    x_val <- select(val, -price)
    
    # random forest
    rf_params <- list(n_iters = floor(n_iters / n_cores))
    rf_time_k <- system.time({
      rf_fitted <- mclapply(1:n_cores, function(x) randomforest(x_train, y_train, rf_params), mc.cores = n_cores)
      rf_fitted <- reduce(rf_fitted, randomForest::combine)
    })
    rf_time <- rf_time + unname(rf_time_k["elapsed"])
    rf_pred[cv_idx[[k]]] <- model_predict(rf_fitted, x_val, rf_params)
  }
  
  list(
    time = rf_time,
    loss = rmse(rf_pred, data$price)
  )
}

open_logfile <- function() {
  dir.create("outputs/", showWarnings = FALSE)
  file <- try(fromJSON(file = "outputs/eval_pp.json"), silent = TRUE)
  if (inherits(file, "try-error")) {
    file <- list()
  }
  file
}

write_logfile <- function(file) {
  write_file(toJSON(file, indent = 2), "outputs/eval_pp.json")
}

oneclick_eval <- function(pp_file, multi_module) {
  cat("Evaluating", pp_file, "\n")
  pp_path <- str_c("models/", pp_file, ".RData")
  eval_log <- open_logfile()
  pp <- get(load(pp_path))
  if (is_pp(pp)) {
    res <- list(eval_pp(pp, data = train_raw))
  } else {
    res <- lapply(pp, function(p) eval_pp(p, data = train_raw))
  }
  eval_log[[pp_file]] <- res
  write_logfile(eval_log)
}


# Run ---------------------------------------------------------------------

train_raw <- read_csv("data/train.csv")

oneclick_eval("pp_baseline")
oneclick_eval("pp_date_yr_grid")
oneclick_eval("pp_date_yr_switch")
oneclick_eval("pp_relevel_switch")
oneclick_eval("pp_conti_grid")
oneclick_eval("pp_zipcode_switch")
oneclick_eval("pp_compose_1")
oneclick_eval("pp_spatial_grid")
