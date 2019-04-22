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


hptune <- model_tune(
  params = list(
    list(
      name = "learning_rate",
      range = c(-1, 0)
    )
  ),
  n_search = 20,
  data_list = data_list,
  model = jnds::xgboost,
  loss_fn = rmse,
  more_params = list(
    objective = "reg:linear",
    depth = 3,
    min_child = 1,
    n_iters = 1000,
    early_stopping_iters = 50
  ),
  max_iters = 3,
  n_cores = 8,
  seed = 42
)
save(hptune, "models/hptune.RData")

# 
# ###
# xgb_params = list(
#   objective = "reg:linear",
#   depth = 3,
#   min_child = 1,
#   learning_rate = 0.3,
#   n_iters = 500,
#   early_stopping_iters = 50
# )
# 
# models <- list()
# for (k in 1:n_folds) {
#   data <- data_list[[k]]
#   x_train <- data$x_train
#   y_train <- data$y_train
#   x_val <- data$x_val
#   y_val <- data$y_val
#   
#   models[[k]] <- xgboost(x_train, y_train, more_params, x_val, y_val)
# }
# 
# pred <- NULL
# for (k in 1:n_folds) {
#   data <- data_list[[k]]
#   x_val <- data$x_val
#   preds <- lapply(models, function(mod) model_predict(mod, x_val))
#   pred <- c(pred, unlist(map(transpose(preds), function(x) reduce(x, sum))) / n_folds)
# }
# y_val_full <- unlist(map(data_list, "y_val"))
# rmse(pred, y_val_full)
# 
# ###
# pred <- NULL
# for (k in 1:n_folds) {
#   data <- data_list[[k]]
#   x_val <- data$x_val
#   pred <- c(pred, model_predict(models[[k]], x_val))
# }
# y_val_full <- unlist(map(data_list, "y_val"))
# rmse(pred, y_val_full)
