library(tidyverse)
library(dplyr)
library(ggplot2)

data <- read.csv("./data/best_swimmers.csv",sep = ",")
view(data)
print()

names(data)

str(data)

dim(data)

data %>% 
  select(Team.Name, Team.Code) %>% 
  head()
  

#Funcio para la descripcion de la data
data_description <- function(data){
  na <- sum(is.na(data))
  description <- "---"
  if(na == 0){
    description <- "El dataframe no tiene valores na"
  }else{
    decription <- "El dataframe SI TIENE valores na"
  }
  
  description <-cat(description , " , contiene " , nrow(data), " filas y ",ncol(data), " columnas",", con las siguientes variables: ",names(data)) 
  
  
  return(description)
}

print(data_description(data))

glimpse(data)
