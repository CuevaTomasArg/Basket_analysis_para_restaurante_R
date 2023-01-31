library(tidyverse)
library(dplyr)
library(ggplot2)

data <- read.csv("../data/best_swimmers.csv",sep = ",")


data <- data %>% 
  mutate(
    Swim.date = as.Date(Swim.date,format = "%m-%d-%y"),
    Athlete.birth.date = as.Date(Athlete.birth.date,format = "%m-%d-%y"),
    Team.Code = as.factor(Team.Code),
    Team.Name = as.factor(Team.Name),
    Age.to.swim = trunc(as.numeric((`Swim.date`- `Athlete.birth.date`)/365))
  )


data <- data %>% 
  select(Athlete.Full.Name, Age.to.swim) %>%
  unique() %>% 
  group_by(Age.to.swim) %>% 
  summarise(
    Amount.Swimmers = sum(Age.to.swim/Age.to.swim)
  ) %>% 
  arrange(desc(Amount.Swimmers))


glimpse(data)

view(data)
