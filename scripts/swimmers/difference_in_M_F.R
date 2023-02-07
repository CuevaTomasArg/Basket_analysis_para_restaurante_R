library(tidyverse)
library(dplyr)
library(ggplot2)

gender_select <- function(data,gender){
  dataframe <- data %>% 
    filter(Gender == gender) %>% 
    select(Athlete.Full.Name,Team.Name,Gender) %>%
    unique() %>% 
    group_by(Team.Name) %>% 
    transmute(
      Team.Name = as.factor(Team.Name),
      id.team.code = as.factor(Team.Name),
      id.team.code = as.integer(id.team.code),
    ) %>%
    summarise(
      Amount.Swimmers = sum(id.team.code/id.team.code)
    ) %>% 
    arrange(desc(Amount.Swimmers))
  
  dataframe <- dataframe %>% 
    mutate(
      Gender = gender
    )
  
  return(dataframe)
}

data <- read.csv("./data/best_swimmers.csv",sep = ",")
View(data)
glimpse(data)

options(scipen = 999)




data <- data %>% 
  mutate(
    Swim.time = NULL,
    Event.Name = as.factor(Event.Name),
    Event.description = as.factor(Event.description),
    City = as.factor(City),
    Country.Code = as.factor(Country.Code),
    Swim.date = as.Date(Swim.date, "%m-%d-%y"),
    Athlete.birth.date = as.Date(Athlete.birth.date,"%m-%d-%y"),
    Team.Name = as.factor(Team.Name),
    Team.Code = NULL,
    Duration..hh.mm.ss.ff. = str_split(Duration..hh.mm.ss.ff. , ":", simplify = TRUE),
    Minute =as.integer(Duration..hh.mm.ss.ff.[,2]),
    Second = as.integer(Duration..hh.mm.ss.ff.[,3]),
    hundredths = as.integer(Duration..hh.mm.ss.ff.[,4]),
    Duration..hh.mm.ss.ff. = NULL,
    index = NULL,
    Gender = as.factor(Gender),
    Age.to.swim = as.integer(trunc(as.numeric((`Swim.date`- `Athlete.birth.date`)/365)))
  ) %>% 
  filter(
    Swim.date < as.Date("2023-01-01") & Athlete.birth.date < as.Date("2023-01-01")
  )

glimpse(data)


female_df <- gender_select(data,"F")
male_df <- gender_select(data,"M")

df_swimmers <- full_join(male_df,female_df)
  
displacement <- 10
ggplot(df_swimmers, aes(x = `Team.Name`,
                        y = `Amount.Swimmers`))+
  geom_linerange(data = subset(df_swimmers, Gender == "M") %>% 
                   mutate(`Amount.Swimmers` = -`Amount.Swimmers`),
                 aes(ymin = -displacement,
                     ymax = -displacement + `Amount.Swimmers`),
                 size = 2,
                 color = "blue")+
  geom_linerange(data = subset(df_swimmers, Gender == "F"),
                 aes(ymin = displacement,
                     ymax = displacement + `Amount.Swimmers`),
                 size = 2,
                 color = "pink")+
  coord_flip()+
  scale_y_continuous(
    breaks = c(seq(-90 ,0,by=10)-displacement,
               seq(0,90,by = 10)+displacement),
    labels = c(rev(seq(0,90, by = 10)), seq(0,90, by = 10))
  )+
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank(),
        axis.text.y = element_blank()
        )+
  geom_label(aes(x = `Team.Name`,
                 y = 0,
                 label = `Team.Name`),
             size = 3.5,
             label.padding = unit(0.0,"lines"),
             label.size = 0,
             label.r = unit(0.0,"lines"),
             fill = "#FAFAFA",
             alpha = 0.9,
             color = "#5D646F")

