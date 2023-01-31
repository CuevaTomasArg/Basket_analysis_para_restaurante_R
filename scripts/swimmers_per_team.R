library(tidyverse)
library(dplyr)
library(ggplot2)

data <- read.csv("./data/best_swimmers.csv",sep = ",")
#full swimmers in the ranking BEGIN
swimmers <- data %>% 
  select(Athlete.Full.Name,Team.Code,Team.Name) %>%
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

ggplot(data =swimmers,mapping = aes(x =as.factor(Team.Code),y = Amount.Swimmers)) +
  geom_bar( stat = "identity")


#full swimmers in the ranking END
