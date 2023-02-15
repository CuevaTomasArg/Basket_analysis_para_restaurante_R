library(tidyverse)
library(dplyr)

data <- read.csv("scripts/Pizzas/PizzaData.csv",sep = ",")

glimpse(data)
best_pizzas <- data %>% 
  select(
    pizza_name,quantity
  ) %>% 
  group_by(
    pizza_name
  ) %>% 
  summarise(sum(quantity)) %>% 
  arrange(desc(`sum(quantity)`)) %>% 
  mutate(
    pizza_name = factor(pizza_name,levels = pizza_name)
  )






ggplot(best_pizzas, aes(x = pizza_name, y = `sum(quantity)`))+
  geom_bar(stat = "identity")+
  coord_flip()
