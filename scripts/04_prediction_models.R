library(tidyverse)
library(jnds)
library(pbmcapply)
library(xgboost)


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


train_raw <- read_csv("data/train.csv")
pp <- get(load("models/pp_compose_1.RData"))
train_pp <- predict(pp, train_raw)

n_folds <- 5
cv_idx <- cv_idx_generate(nrow(train_raw), n_folds, seed = 42)

data_list <- list()
for (k in seq_len(n_folds)) {
  idx <- cv_idx[[k]]
  x_train <- select(train_raw[-idx, ], -price)
  y_train <- train_raw$price[-idx]
  x_val <- select(train_raw[idx, ], -price)
  y_val <- train_raw$price[idx]
  data_list[[k]] <- list(
    x_train = x_train,
    y_train = y_train,
    x_val = x_val,
    y_val = y_val
  )
}


for (depth in 3:6) {
  cat("depth:", depth, "\n")
  hptune <- model_tune(
    params = list(
      list(
        name = "learning_rate",
        range = c(-2, 0)
      ),
      list(
        name = "col_fraction",
        range = c(-0.2, 0)
      ),
      list(
        name = "row_fraction",
        range = c(-0.2, 0)
      )
    ),
    n_search = 32,
    data_list = data_list,
    model = jnds::xgboost,
    loss_fn = rmse,
    more_params = list(
      objective = "reg:linear",
      depth = depth,
      min_child = 1,
      n_iters = 5000,
      early_stopping_iters = 100
    ),
    max_iters = 2,
    n_cores = 8,
    seed = 42
  )
  outfile <- str_c("models/hptune_depth_", depth, ".RData")
  save(hptune, outfile)
}
