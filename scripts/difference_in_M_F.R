library(tidyverse)
library(dplyr)
library(ggplot2)

data <- read.csv("./data/best_swimmers.csv",sep = ",")

gender_select <- function(data,gender){
  dataframe <- data %>% 
    filter(Gender == gender) %>% 
    select(Athlete.Full.Name,Team.Code,Team.Name,Gender) %>%
    unique() %>% 
    group_by(Team.Code,Team.Name) %>% 
    transmute(
      Team.Code = as.factor(Team.Code),
      id.team.code = as.factor(Team.Code),
      id.team.code = as.integer(id.team.code)
    ) %>% 
    summarise(
      Amount.Swimmers = sum(id.team.code/id.team.code)
    ) %>% 
    arrange(desc(Amount.Swimmers))
  
  return(dataframe)
}


female_df <- gender_select(data,"F")

male_df <- gender_select(data,"M")


ggplot(data =male_df,mapping = aes(x =as.factor(Team.Code),y = Amount.Swimmers)) +
  geom_bar( stat = "identity")+
  
  
  #full swimmers in the ranking END
  