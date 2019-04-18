library(tidyverse)
library(rjson)


eval_log <- fromJSON(file = "outputs/eval_pp.json")
names(eval_log)
names(eval_log[[1]])
names(eval_log[[2]])
eval_log[[1]]
eval_log[[2]]
flatten(eval_log[[2]])

map(eval_log[[1]], "loss")
map(eval_log[[2]], "loss")

loss <- c(
  eval_log[[1]]$loss,
  unlist(map(eval_log[[2]], "loss")),
  unlist(map(eval_log[[3]], "loss")),
  unlist(map(eval_log[[4]], "loss")),
  unlist(map(eval_log[[5]], "loss"))
)
loss

train_time <- c(
  unlist(eval_log[[1]]$train_time),
  unlist(map(eval_log[[2]], "train_time")),
  unlist(map(eval_log[[3]], "train_time")),
  unlist(map(eval_log[[4]], "train_time")),
  unlist(map(eval_log[[5]], "train_time"))
)
train_time

tibble(train_time = train_time, loss = loss) %>% 
  mutate(id = 1:n()) %>% 
  ggplot(aes(train_time, loss)) +
  geom_point() +
  geom_text(aes(label = id), vjust = -.5)
