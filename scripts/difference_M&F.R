library(tidyverse)
library(dplyr)
library(ggplot2)

data <- read.csv("./data/best_swimmers.csv",sep = ",")

gender_select <- function(data,gender){
  dataframe <- data %>% 
    filter(Gender == gender) %>% 
    select(Athlete.Full.Name,Team.Code,Gender) %>%
    unique() %>% 
    group_by(Team.Code,Gender) %>% 
    mutate(
      Team.Code = as.factor(Team.Code),
      id.team.code = as.factor(Team.Code),
      id.team.code = as.integer(id.team.code)
    ) %>% 
    summarise(
      Amount.Swimmers = sum(id.team.code/id.team.code),
    ) %>% 
    arrange(desc(Amount.Swimmers))
  
  return(dataframe)
}


female_df <- gender_select(data,"F")

male_df <- gender_select(data,"M")

genders <- full_join(female_df,male_df)

genders <-genders %>% 
  arrange(desc(Amount.Swimmers))

ggplot(genders,aes(x = `Team.Code`,
                   y = `Amount.Swimmers`,
                   fill = Gender)) +
  geom_col(data = subset(genders,Gender == "M") %>% 
             mutate(`Amount.Swimmers` = -`Amount.Swimmers`),
           width = 0.5,fill = "blue")+
  geom_col(data = subset(genders,Gender == "F"),
           width = 0.5,fill = "pink")+
  coord_flip()
  #Agregar la parte de scala  continuidad de "https://juvenalcampos.com/2020/01/26/visualizando-pir%C3%A1mides-de-poblaci%C3%B3n-en-r/"
