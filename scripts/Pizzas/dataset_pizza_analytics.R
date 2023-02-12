install.packages("readxl")
library(readxl)
library(tidyverse)
library(dplyr)
library(stringr)

data_description <- function(data){
  na <- sum(is.na(data))
  if(na == 0){
    description <- "El dataframe no tiene valores na."
  }else{
    decription <- "El dataframe SI TIENE valores na."
    na.omit(data)
    print("se eliminarion "+ na + " valores NA")
  }
  
  description <-cat(description ,"\n", "Contiene " , nrow(data), " filas y ",ncol(data), " columnas","\n","Con las siguientes variables: ",names(data)) 
  
  return(description)
}


data <- read_xlsx("C:/Users/tomas/OneDrive/Escritorio/R/ProyectoFinal_R_CuevaTomas/data/PizzaSales.xlsx")

glimpse(data)

View(data)

print(data_description(data))

manipulated <- data %>% 
  mutate(
    order_details_id = as.integer(order_details_id),
    order_id = as.integer(order_id),
    quantity = as.integer(quantity),
    pizza_id = NULL,
    order_time = NULL,
    pizza_size = NULL,
    pizza_category = as.factor(pizza_category),
    pizza_name = as.factor(pizza_name),
    pizza_id = NULL,
    pizza_ingredients = NULL
  )


glimpse(manipulated)

write.csv(manipulated, "PizzaData.csv")

View(manipulated)

pizza_types <- manipulated %>% 
  select(pizza_name) %>% 
  group_by(pizza_name) %>% 
  unique()
View(pizza_types)
