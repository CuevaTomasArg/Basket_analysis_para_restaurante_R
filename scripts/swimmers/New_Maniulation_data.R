library(tidyverse)
library(dplyr)
library(stringr)

data <- read.csv("./best_swimmers.csv",sep = ",")

glimpse(data)

data_cleaned <- data %>% 
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
    Hour = as.integer(Duration..hh.mm.ss.ff.[,1]),
    Minute =as.integer(Duration..hh.mm.ss.ff.[,2]),
    Second = as.integer(Duration..hh.mm.ss.ff.[,3]),
    hundredths = as.integer(Duration..hh.mm.ss.ff.[,4]),
    Duration..hh.mm.ss.ff. = NULL,
    index = NULL,
    Gender = as.factor(Gender),
  )

na <- sum(is.na(filter(data_cleaned, Hour != 0)))

if(na == 0){
  data <- data_cleaned %>% 
    mutate(
      Hour = NULL
    )
  print("Finalizada la manipulacion de datos")
}else{
  decription <- "El dataframe SI TIENE valores na"
}

glimpse(data)
View(data)