library(tidyverse)
library(dplyr)

data <- read.csv("scripts/Pizzas/PizzaData.csv",sep = ",")

glimpse(data)

View(data)
best_pizzas <- data %>% 
  transmute(
    pizza_id = as.factor(pizza_name),
    quantity = quantity
  ) %>% 
  group_by(
    pizza_id
  ) %>% 
  summarise(sum(quantity)) %>% 
  arrange(desc(`sum(quantity)`))

glimpse(best_pizzas)

View(best_pizzas)

ggplot(best_pizzas, aes(x = pizza_id, y = `sum(quantity)`))+
  geom_bar(stat = "identity")+
  coord_flip()
