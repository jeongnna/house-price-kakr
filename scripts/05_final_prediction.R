library(tidyverse)
library(jnds)
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


# load data
train_raw <- read_csv("data/train.csv")
test_raw <- read_csv("data/test.csv")
pp <- get(load("models/pp_compose_1.RData"))
train_pp <- predict(pp, train_raw)
test_pp <- predict(pp, test_raw)


# split data in 5 folds
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


# model tuning
xgb_params = list(
  objective = "reg:linear",
  depth = 3,
  min_child = 1,
  learning_rate = 0.3,
  n_iters = 1000,
  early_stopping_iters = 50
)

models <- list()
for (k in 1:n_folds) {
  data <- data_list[[k]]
  x_train <- data$x_train
  y_train <- data$y_train
  x_val <- data$x_val
  y_val <- data$y_val
  
  models[[k]] <- jnds::xgboost(x_train, y_train, xgb_params, x_val, y_val)
}
n_iters <- mean(unlist(map(models, "best_iteration")))
xgb_params$n_iters <- n_iters

x_train <- select(train_pp, -price)
y_train <- train_pp$price
xgb_fitted <- jnds::xgboost(x_train, y_train, xgb_params)


# prediction
pred <- model_predict(xgb_fitted, test_pp)


# update submission file
submission <- read_csv("data/sample_submission.csv")
submission$price <- pred
write_csv(submission, "submission.csv")
