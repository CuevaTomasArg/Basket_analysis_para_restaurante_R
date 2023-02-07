library(tidyverse)
library(dplyr)
library(ggplot2)


mean_age <- function(data,G){
  new_df <- data %>% 
    select(Athlete.Full.Name, Age.to.swim, Gender) %>%
    unique() %>% 
    filter(Gender == G) %>% 
    group_by(Age.to.swim) %>% 
    summarise(
      Amount.Swimmers = sum(Age.to.swim/Age.to.swim)
    ) %>% 
    arrange(desc(Amount.Swimmers))
    
  return(new_df)
}

data <- read.csv("./data/best_swimmers.csv",sep = ",")


data <- data %>% 
  mutate(
    Swim.date = as.Date(Swim.date,format = "%m-%d-%y"),
    Athlete.birth.date = as.Date(Athlete.birth.date,format = "%m-%d-%y"),
    Team.Code = as.factor(Team.Code),
    Team.Name = as.factor(Team.Name),
    Age.to.swim = trunc(as.numeric((`Swim.date`- `Athlete.birth.date`)/365))
  )


female_data_age <- mean_age(data,"F")
  
  
male_data_age <- mean_age(data,"M")

ggplot(
  data =female_data_age,
  mapping = aes(
    x =Age.to.swim,
    y = Amount.Swimmers)
) +
  geom_bar( stat = "identity")
