library(arules)
library(arulesViz)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(stringr)

data_inicial <- read.csv("scripts/Pizzas/PizzaData.csv", sep = ',')

glimpse(data_inicial)
View(data_inicial)

data_orders <- data_inicial %>% 
  select(order_details_id,order_id,order_date,pizza_name) %>% 
  group_by(order_id,order_date) %>% 
  summarise(glue::glue_collapse(glue::glue("{pizza_name}"), sep = ','))


data_csv <- data_orders

data_csv$order_id <- NULL
data_csv$order_date <- NULL
data_csv$`glue::glue_collapse(glue::glue("{pizza_name}"), sep = ",")` <- as.character(data_csv$`glue::glue_collapse(glue::glue("{pizza_name}"), sep = ",")`)

View(data_csv)

write.csv(data_csv, "scripts/Pizzas/PizzasList.csv", quote = FALSE, row.names = TRUE)

