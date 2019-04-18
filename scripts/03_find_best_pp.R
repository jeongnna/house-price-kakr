library(tidyverse)
library(rjson)


eval_log <- fromJSON(file = "outputs/eval_pp.json")

df <- NULL
for (log in eval_log) {
  tmp <- log %>% map("rf")
  df <- bind_rows(df, tmp)
}

df %>% 
  mutate(id = 1:n()) %>% 
  ggplot(aes(time, loss)) +
  geom_point() +
  geom_text(aes(label = id), vjust = -.5)
