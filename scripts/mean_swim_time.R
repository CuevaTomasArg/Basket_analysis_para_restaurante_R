library(tidyverse)
library(dplyr)
library(ggplot2)

data <- read.csv("./data/best_swimmers.csv",sep = ",")
glimpse(data)


mean_evolution_time <- function(data,category){
  media <- 0
  mean_growth <- data %>% 
    filter(Event.description == category) %>% 
    transmute(
      Swim.time = as.double.difftime(Swim.time)
      ) %>%
    unique()
  
  mean_growth <- as.vector(mean_growth$Swim.time)
  
  print(mean_growth)
  
#  for (i in 1:len) {
#    media <- media + (mean_growth[i+1] - mean_growth[i])
#  }
  
#  return(media/(len+1))
    
}

print(mean_evolution_time(data,"Men 200 Freestyle LCM Male"))

      