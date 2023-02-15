library(arules)
library(arulesViz)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(stringr)

data_inicial <- read.csv("scripts/Pizzas/PizzaData.csv", sep = ',')

View(data_inicial)
data_inicial <- data_inicial %>% 
  na.omit()

data_orders <- data_inicial %>%
  select(order_details_id,order_id,order_date,pizza_category) %>% 
  group_by(order_id,order_date) %>% 
  summarise(glue::glue_collapse(glue::glue("{pizza_category}"), sep = ','))

data_csv <- data_orders

data_csv$order_id <- NULL
data_csv$order_date <- NULL
data_csv$`glue::glue_collapse(glue::glue("{pizza_category}"), sep = ",")` <- as.character(data_csv$`glue::glue_collapse(glue::glue("{pizza_category}"), sep = ",")`)

View(data_csv)

write.csv(data_csv, "scripts/Pizzas/PizzasList.csv", quote = FALSE, row.names = TRUE)

transacciones_pizzas <- read.transactions(
  "scripts/Pizzas/PizzasList.csv", 
  rm.duplicates = TRUE, 
  format = "basket",
  sep = ",", 
  cols = 1
)

transacciones_pizzas@itemInfo$labels <- gsub("\"","",transacciones_pizzas@itemInfo$labels)

itemFrequencyPlot(
  transacciones_pizzas,
  topN = 20,
  type = "absolute",
  main = "Frecuencia Absoluta"
)

itemFrequencyPlot(
  transacciones_pizzas, 
  topN = 15, 
  type = "relative",
  ylab = "Frequency (relative)",
  main="Frecuencia Relativa"
)

data_inicial %>% 
  mutate(
    Month = as.factor(month(order_date))
  ) %>% 
  group_by(Month) %>% 
  summarise(
    Transactions = n()
  ) %>% 
  ggplot(aes(x = Month, y = Transactions)) +
  geom_bar(stat = 'identity', fill = "mistyrose1", show.legend = FALSE, color = "black") +
  ylab("Transacciones") +
  xlab("Mes") +
  ggtitle("Transacciones por Mes") +
  theme_bw()

data_inicial %>% 
  mutate(
    DiaSemana = as.factor(weekdays(as.Date(order_date)))
  ) %>% 
  group_by(DiaSemana) %>% 
  summarise(
    Transactions = n()
  ) %>% 
  ggplot(aes(x = DiaSemana, y = Transactions)) +
  geom_bar(stat = 'identity', fill = "mistyrose1", show.legend = FALSE, color = "black") +
  geom_label(aes(label = Transactions)) +
  scale_x_discrete(
    limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  ylab("Transacciones") +
  xlab("Mes") + 
  ggtitle("Transacciones por Dia de la Semana") +
  theme_bw()

supportLevels <- c(0.1, 0.05, 0.01, 0.005, 0.001)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.05)

# Empty integers 
rules_sup10 <- integer(length = 10)
rules_sup5 <- integer(length = 10)
rules_sup1 <- integer(length = 10)
rules_sup0.5 <- integer(length = 10)
rules_sup0.1 <- integer(length = 10)

Apriori_algorithm <- function(rules,confidence_levels,transactions,support_levels,n){
  for (i in 1:length(confidenceLevels)) {
    rules[i] <- length(
      apriori(
        transactions, 
        parameter = list(
          minlen = 1,
          sup = support_levels[n],
          conf = confidence_levels[i],
          target = "rules"
        )
      )
    )
  }
  
  return(rules)
}

rules_sup10 <- Apriori_algorithm(rules_sup10,confidenceLevels,transacciones_pizzas,supportLevels,1)
rules_sup5 <- Apriori_algorithm(rules_sup5,confidenceLevels,transacciones_pizzas,supportLevels,2)
rules_sup1 <- Apriori_algorithm(rules_sup1,confidenceLevels,transacciones_pizzas,supportLevels,3)
rules_sup0.5 <- Apriori_algorithm(rules_sup0.5,confidenceLevels,transacciones_pizzas,supportLevels,4)
rules_sup0.1 <- Apriori_algorithm(rules_sup0.1,confidenceLevels,transacciones_pizzas,supportLevels,5)


# Function of number of rules found with a support levels

numbers_rules <- function(confidence,rules,percentage,scale = NULL){
  if( ! is.null(scale)){
    plot <- qplot(
      confidence,
      rules,
      geom = c("point", "line"), 
      xlab = "Confidence level", ylab = "Number of rules found", 
      main = cat("Apriori with a support level of ",percentage)
    ) +
      scale_y_continuous(breaks = scale) +
      theme_bw()
  }else{
    plot <- qplot(
      confidence,
      rules,
      geom = c("point", "line"), 
      xlab = "Confidence level", ylab = "Number of rules found", 
      main = cat("Apriori with a support level of ",percentage)
    ) +
      theme_bw()
  }
  
  return(plot)
}

plot1 <- numbers_rules(confidenceLevels,rules_sup10,"10%")
plot2 <- numbers_rules(confidenceLevels,rules_sup5,"5%",seq(0, 10, 2))
plot3 <- numbers_rules(confidenceLevels,rules_sup1,"1%",seq(0, 50, 10))
plot4 <- numbers_rules(confidenceLevels,rules_sup0.5,"0.5%",seq(0, 130, 20))

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5, confidenceLevels)


ggplot(data = num_rules, aes(x = confidenceLevels)) +
  geom_line(aes(y = rules_sup10, colour = "Nivel de Soporte: 10%")) + 
  geom_point(aes(y = rules_sup10, colour = "Nivel de Soporte: 10%")) +
  geom_line(aes(y = rules_sup5, colour = "Nivel de Soporte: 5%")) +
  geom_point(aes(y = rules_sup5, colour = "Nivel de Soporte: 5%")) +
  geom_line(aes(y = rules_sup1, colour = "Nivel de Soporte: 1%")) + 
  geom_point(aes(y = rules_sup1, colour = "Nivel de Soporte: 1%")) +
  geom_line(aes(y = rules_sup0.5, colour = "Nivel de Soporte: 0.5%")) +
  geom_point(aes(y = rules_sup0.5, colour = "Nivel de Soporte: 0.5%")) +
  labs(
    x = "Niveles de Confianza", 
    y = "Cantidad de reglas encontradas", 
    title = "Algoritmo Apriori algorithm para distintos niveles de soporte"
  ) +
  theme_bw() +
  theme(legend.title = element_blank())

rules_sup1_conf10 <- apriori(
  transacciones_pizzas, 
  parameter = list(
    minlen = 4,
    sup = 0.001, 
    conf = 0.2, 
    target = "rules"
  )
)

inspect(rules_sup1_conf10)

plot(rules_sup1_conf10, measure = c("support", "lift"), shading = "confidence",jitter = 0)

# Grafico de asociaciones.
plot(rules_sup1_conf10, method = "graph",max = 10)

# Forma alternativa de graficar asociaciones, considerar que esta visualizacion puede tardar un tiempo considerable si se trata 
# con un gran numero de reglas
plot(rules_sup1_conf10, method = "paracoord")
