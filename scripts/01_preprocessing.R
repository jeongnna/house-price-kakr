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
  if (is.null(k) & is.null(h)) k <- 53  # minimal levels RF model can handle
  s <- 0.5 * (sd(zipcode_summ$lat) + sd(zipcode_summ$long))
  zipcode_summ$log_price <- scale(zipcode_summ$log_price) * s * w
  hc <- hclust(dist(zipcode_summ[2:4]))
  zipcode_summ$cluster <- factor(cutree(hc, k, h))
  zipcode_summ %>% select(zipcode, cluster)
}

create_pp_zipcode <- function(w = 1, k = NULL, h = NULL, desc = NULL) {
  pp_instant(
    expr({
      repl <- object$sub_object$cluster
      names(repl) <- object$sub_object$zipcode
      data$zipcode <- plyr::revalue(factor(data$zipcode), repl)
    }),
    sub_object = zipcode_cluster(train_raw, w, k, h),
    desc = desc
  )
}


# data loading ------------------------------------------------------------

train_raw <- read_csv("data/train.csv")


# pp modules --------------------------------------------------------------

# baseline
cat_to_factor <- pp_factor(
  cols = c("waterfront", "zipcode"),
  desc = "category to factor"
)
remove_id <- pp_remove(
  "id",
  desc = "Remove id"
)
remove_sqft_living <- pp_remove(
  "sqft_living",
  desc = "remove sqft_living"
)
# trans_zipcode_baseline <- pp_instant(
#   expr({
#     repl <- object$sub_object$cluster
#     names(repl) <- object$sub_object$zipcode
#     data$zipcode <- plyr::revalue(factor(data$zipcode), repl)
#   }),
#   sub_object = zipcode_cluster(train_raw, w = 1),
#   desc = "zipcode 53 clusters"
# )

trans_zipcode_baseline <- create_pp_zipcode(desc = "zipcode 53 clusters")

trans_yr_renovated <- pp_instant(
  expr({
    no_rv <- data$yr_renovated == 0
    data$yr_renovated[no_rv] <- data$yr_built[no_rv]
  }),
  desc = "adjust yr_renovated zero values"
)


# date & year
new_renovated <- pp_instant(
  expr({
    data <- mutate(data, renovated = ifelse(yr_renovated == yr_built, 0, 1))
  }),
  desc = "create renovated"
)
trans_yr_renovated2 <- pp_instant(
  expr({
    data <- 
      data %>%
      mutate(delay_renovated = yr_renovated - yr_built) %>%
      select(-yr_renovated)
  }),
  desc = "yr_renovated to delay_renovated"
)
remove_yr <- pp_remove(c("yr_built", "yr_renovated"), desc = "remove yr_built, yr_renovated")
remove_date <- pp_remove("date", desc = "remove date")


# releveling
trans_view <- pp_instant(
  expr({
    data$view[data$view == 1] <- 2
  }),
  desc = "relevel view"
)
trans_condition <- pp_instant(
  expr({
    data$condition <- factor(ifelse(data$condition >= 3, 1, 0))
  }),
  desc = "relevel condition"
)
trans_grade <- pp_instant(
  expr({
    data$grade[data$grade <= 4] <- 4
  }),
  desc = "relevel grade"
)
trans_bathrooms <- pp_instant(
  expr({
    data$bathrooms[data$bathrooms <= 0.75] <- 0.75
    data$bathrooms[data$bathrooms >= 4.5] <- 4.5
  }),
  desc = "relevel bathrooms"
)
trans_floors <- pp_instant(
  expr({
    data$floors[data$floors >= 3] <- 3
  }),
  desc = "relevel floors"
)
as_factor_floors <- pp_factor(cols = "floors", desc = "floors to factor")


# continuous variables
new_no_basement <- pp_instant(
  expr({
    data$no_basement <- factor(ifelse(data$sqft_basement == 0, 1, 0))
  }),
  desc = "create no_basement"
)
trans_sqft_basement <- pp_instant(
  expr({
    zero_mask <- data$sqft_basement == 0
    data$sqft_basement[zero_mask] <- mean(data$sqft_basement, na.rm = TRUE)
  }),
  desc = "handle sqft_basement zero values"
)
remove_sqft_living15 <- pp_remove(
  "sqft_living15",
  desc = "remove sqft_living15"
)
remove_sqft_lot15 <- pp_remove(
  "sqft_lot15",
  desc = "remove sqft_lot15"
)
remove_sqft_lot <- pp_remove(
  "sqft_lot",
  desc = "remove sqft_lot"
)


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
  remove_id,
  remove_sqft_living,
  trans_zipcode_baseline,
  trans_yr_renovated,
  data = train_raw
)

pp_date_yr_grid <- pp_grid(
  new_renovated,
  trans_yr_renovated2,
  remove_date,
  default = pp_baseline,
  data = train_raw
)

pp_date_yr_switch <- pp_switch(
  remove_yr,
  pp_sequential(
    new_renovated,
    remove_yr,
    data = train_raw
  ),
  pp_sequential(
    remove_date,
    remove_yr,
    data = train_raw
  ),
  pp_sequential(
    new_renovated,
    remove_date,
    remove_yr,
    data = train_raw
  ),
  default = pp_baseline,
  data = train_raw
)

pp_relevel_switch <- pp_switch(
  trans_view,
  trans_condition,
  trans_grade,
  trans_bathrooms,
  trans_floors,
  default = pp_baseline,
  data = train_raw
)

pp_conti_grid <- pp_grid(
  new_no_basement,
  trans_sqft_basement,
  remove_sqft_living15,
  remove_sqft_lot15,
  remove_sqft_lot,
  default = pp_baseline,
  data = train_raw
)

pp_zipcode_switch <- pp_switch(
  create_pp_zipcode(w = exp(-0.4), h = exp(-2.0)),
  create_pp_zipcode(w = exp(-0.4), h = exp(-1.8)),
  create_pp_zipcode(w = exp(-0.4), h = exp(-1.6)),
  create_pp_zipcode(w = exp(-0.4), h = exp(-1.4)),
  create_pp_zipcode(w = exp(-0.4), h = exp(-1.2)),
  create_pp_zipcode(w = exp(-0.4), h = exp(-1.0)),
  create_pp_zipcode(w = exp(-0.4), h = exp(-0.8)),
  create_pp_zipcode(w = exp(-0.4), h = exp(-0.6)),
  create_pp_zipcode(w = exp(-0.2), h = exp(-2.0)),
  create_pp_zipcode(w = exp(-0.2), h = exp(-1.8)),
  create_pp_zipcode(w = exp(-0.2), h = exp(-1.6)),
  create_pp_zipcode(w = exp(-0.2), h = exp(-1.4)),
  create_pp_zipcode(w = exp(-0.2), h = exp(-1.2)),
  create_pp_zipcode(w = exp(-0.2), h = exp(-1.0)),
  create_pp_zipcode(w = exp(-0.2), h = exp(-0.8)),
  create_pp_zipcode(w = exp(-0.2), h = exp(-0.6)),
  create_pp_zipcode(w = exp(0), h = exp(-2.0)),
  create_pp_zipcode(w = exp(0), h = exp(-1.8)),
  create_pp_zipcode(w = exp(0), h = exp(-1.6)),
  create_pp_zipcode(w = exp(0), h = exp(-1.4)),
  create_pp_zipcode(w = exp(0), h = exp(-1.2)),
  create_pp_zipcode(w = exp(0), h = exp(-1.0)),
  create_pp_zipcode(w = exp(0), h = exp(-0.8)),
  create_pp_zipcode(w = exp(0), h = exp(-0.6)),
  create_pp_zipcode(w = exp(0.2), h = exp(-2.0)),
  create_pp_zipcode(w = exp(0.2), h = exp(-1.8)),
  create_pp_zipcode(w = exp(0.2), h = exp(-1.6)),
  create_pp_zipcode(w = exp(0.2), h = exp(-1.4)),
  create_pp_zipcode(w = exp(0.2), h = exp(-1.2)),
  create_pp_zipcode(w = exp(0.2), h = exp(-1.0)),
  create_pp_zipcode(w = exp(0.2), h = exp(-0.8)),
  create_pp_zipcode(w = exp(0.2), h = exp(-0.6)),
  create_pp_zipcode(w = exp(0.4), h = exp(-2.0)),
  create_pp_zipcode(w = exp(0.4), h = exp(-1.8)),
  create_pp_zipcode(w = exp(0.4), h = exp(-1.6)),
  create_pp_zipcode(w = exp(0.4), h = exp(-1.4)),
  create_pp_zipcode(w = exp(0.4), h = exp(-1.2)),
  create_pp_zipcode(w = exp(0.4), h = exp(-1.0)),
  create_pp_zipcode(w = exp(0.4), h = exp(-0.8)),
  create_pp_zipcode(w = exp(0.4), h = exp(-0.6)),
  default = pp_sequential(
    cat_to_factor,
    remove_id,
    remove_sqft_living,
    trans_yr_renovated,
    data = train_raw
  ),
  data = train_raw
)

pp_compose_1 <- pp_sequential(
  cat_to_factor,
  remove_id,
  remove_sqft_living,
  trans_yr_renovated,
  create_pp_zipcode(w = exp(0.2), h = exp(-1.6)),
  new_renovated,
  trans_yr_renovated2,
  remove_date,
  trans_grade,
  new_no_basement,
  trans_sqft_basement,
  remove_sqft_lot15,
  data = train_raw
)


# save pp modules ---------------------------------------------------------

dir.create("models/", showWarnings = FALSE)
save(pp_baseline, file = "models/pp_baseline.RData")
save(pp_date_yr_grid, file = "models/pp_date_yr_grid.RData")
save(pp_date_yr_switch, file = "models/pp_date_yr_switch.RData")
save(pp_relevel_switch, file = "models/pp_relevel_switch.RData")
save(pp_conti_grid, file = "models/pp_conti_grid.RData")
save(pp_zipcode_switch, file = "models/pp_zipcode_switch.RData")
save(pp_compose_1, file = "models/pp_compose_1.RData")
