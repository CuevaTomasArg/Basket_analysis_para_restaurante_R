library(tidyverse)
library(dplyr)
library(stringr)

data <- read.csv("scripts/Pizzas/PizzaData.csv",sep = ",")

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


barplot(height = data$`sum(total_price)`,names= data$month)


data %>% 
  tail(10) %>% 
  ggplot(aes(x = month, y = `sum(total_price)`, group = 1))+
  geom_line()+
  geom_point()
