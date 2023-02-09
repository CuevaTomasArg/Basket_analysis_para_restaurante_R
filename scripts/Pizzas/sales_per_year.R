library(tidyverse)
library(dplyr)
library(stringr)

data <- read.csv("scripts/Pizzas/PizzaData.csv",sep = ",")

glimpse(data)
View(data)

data <- data %>% 
  select(order_date,total_price) %>% 
  mutate(
    order_date = str_split(order_date,"-",simplify = T),
    month = as.factor(order_date[,2])
  ) %>% 
  select(month,total_price) %>% 
  group_by(month) %>% 
  summarise(sum(total_price)) %>% 
  mutate(
    month = c("January","February","March","April","May","June","July","August","September","October","November","December")
  )

glimpse(data)

View(data)

barplot(height = data$`sum(total_price)`,names= data$month)
