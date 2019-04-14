library(tidyverse)
source("src/source_dir.R")
source_dir("jds")


# preparation -------------------------------------------------------------

train_raw <- read_csv("data/train.csv")
pp <- get(load("model/pp_baseline.RData"))
train_pp <- predict(pp, data = train_raw)


# cross validation --------------------------------------------------------

# create cv indices
set.seed(42)
n_folds <- 5
n <- nrow(train_pp)
full_idx <- 1:n
cv_idx <- list()
for (i in seq_len(n_folds - 1)) {
  temp_idx <- sample(full_idx, n / n_folds, replace = FALSE)
  cv_idx[[i]] <- temp_idx
  full_idx <- setdiff(full_idx, temp_idx)
}
cv_idx[[n_folds]] <- sample(full_idx, length(full_idx), replace = FALSE)

# run cv
tr_time <- 0
rf_pred <- rep(NA, nrow(train_pp))
cat("Cross validation\n")
for (k in seq_len(n_folds)) {
  cat("fold = ", k, "\n", sep = "")
  
  # split data into training & validation set
  val <- train_pp[cv_idx[[k]], ]
  train <- setdiff(train_pp, val)
  x_train <- select(train, -price)
  y_train <- train$price
  x_val <- select(val, -price)
  
  # model fitting & prediction
  rf_params <- list(n_trees = 200)
  tr_time_k <- system.time({
    rf_fitted <- randomforest(x_train, y_train, rf_params)
  })
  tr_time <- tr_time + tr_time_k["elapsed"]
  rf_pred[cv_idx[[k]]] <- model_predict(rf_fitted, x_val, rf_params)
}

(rf_rmse <- rmse(rf_pred, train_pp$price))
# target not log: 130872.8
