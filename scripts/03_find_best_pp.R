library(tidyverse)
library(rjson)


eval_log <- fromJSON(file = "outputs/eval_pp.json")

i <- 6
names(eval_log)[i]
df <- reduce(eval_log[c(1, i)], bind_rows)
df %>% 
  mutate(id = 1:n()) %>% 
  ggplot(aes(time, loss)) +
  geom_point() +
  geom_text(aes(label = id), vjust = -.5)

df %>% 
  mutate(
    id = 1:n(),
    w = factor(c(1, rep(2:9, each = 5))),
    h = factor(c(1, rep(2:9, 5)))
  ) %>% 
  ggplot(aes(time, loss, col = h)) +
  geom_point() +
  geom_text(aes(label = id), vjust = -.5) +
  geom_line(aes(group = h))
