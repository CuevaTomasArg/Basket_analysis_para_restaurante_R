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
    pizza_id = as.factor(pizza_id),
    order_time = str_split(as.character(order_time), " ", simplify = T),
    order_time =  order_time[,2],
    pizza_size = as.factor(pizza_size),
    pizza_category = as.factor(pizza_category),
  )
glimpse(manipulated)

write.csv(manipulated, "PizzaData.csv")

View(manipulated)
