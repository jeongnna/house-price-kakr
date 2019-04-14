library(tidyverse)
library(jnds)


# functions ---------------------------------------------------------------

zipcode_cluster <- function(data, w = 1, k = NULL, h = NULL) {
  zipcode_summ <- data %>% 
    group_by(zipcode) %>% 
    summarize(
      lat = mean(lat),
      long = mean(long),
      log_price = mean(log(price))
    )
  if (is.null(k) & is.null(h)) k <- 53
  s <- 0.5 * (sd(zipcode_summ$lat) + sd(zipcode_summ$long))
  zipcode_summ$log_price <- scale(zipcode_summ$log_price) * s * w
  hc <- hclust(dist(zipcode_summ[2:4]))
  zipcode_summ$cluster <- factor(cutree(hc, k, h))
  zipcode_summ %>% select(zipcode, cluster)
}


# data loading ------------------------------------------------------------

train_raw <- read_csv("data/train.csv")


# pp modules --------------------------------------------------------------

# baseline
cat_cols <- c("waterfront", "zipcode")
cat_to_factor <- pp_factor(cols = cat_cols)
remove_id <- pp_remove("id")
remove_sqft_living <- pp_remove("sqft_living")
trans_zipcode_baseline <- pp_instant(
  expr({
    repl <- object$sub_object$cluster
    names(repl) <- object$sub_object$zipcode
    data$zipcode <- plyr::revalue(factor(data$zipcode), repl)
  }),
  sub_object = zipcode_cluster(train_raw, w = 1)
)

# ordinal variables
trans_view <- pp_instant(expr({
  data$view[data$view == 1] <- 2
}))
trans_condition <- pp_instant(expr({
  data$condition <- factor(ifelse(data$condition >= 3, 1, 0))
}))
trans_grade <- pp_instant(expr({
  data$grade[data$grade <= 4] <- 4
}))

# discrete variables
trans_bathrooms <- pp_instant(expr({
  data$bathrooms[data$bathrooms <= 0.75] <- 0.75
  data$bathrooms[data$bathrooms >= 4.5] <- 4.5
}))
trans_floors <- pp_instant(expr({
  data$floors[data$floors >= 3] <- 3
}))
as_factor_floors <- pp_factor(cols = "floors")

# continuous variables
trans_sqft_basement <- pp_instant(expr({
  zero_mask <- data$sqft_basement == 0
  data$sqft_basement[zero_mask] <- mean(data$sqft_basement, na.rm = TRUE)
}))
new_no_basement <- pp_instant(expr({
  data$no_basement <- factor(ifelse(data$sqft_basement == 0, 1, 0))
}))
remove_sqft_15s <- pp_remove(c("sqft_living15", "sqft_lot15"))
remove_sqft_lot <- pp_remove("sqft_lot")

# date & year
trans_yr_renovated <- pp_instant(expr({
  no_rv <- data$yr_renovated == 0
  data$yr_renovated[no_rv] <- data$yr_built[no_rv]
}))
trans_yr_renoveted2 <- pp_instant(expr({
  data <- 
    data %>%
    mutate(delay_renovated = yr_renovated - yr_built) %>%
    select(-yr_renovated)
}))
remove_yr <- pp_remove(c("yr_built", "yr_renovated"))
remove_date <- pp_remove("date")

# spatial
trans_zipcode_w1_h.4 <- pp_instant(
  expr({
    repl <- object$sub_object$cluster
    names(repl) <- object$sub_object$zipcode
    data$zipcode <- plyr::revalue(factor(data$zipcode), repl)
  }),
  sub_object = zipcode_cluster(train_raw, w = 1, h = .4)
)


# pp sequential modules ---------------------------------------------------

pp_baseline <- pp_sequential(
  cat_to_factor,
  trans_zipcode_baseline,
  remove_id,
  remove_sqft_living,
  data = train_raw
)


# save pp modules ---------------------------------------------------------

save(pp_baseline, file = "model/pp_baseline.RData")
