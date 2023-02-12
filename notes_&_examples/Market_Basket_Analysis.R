# Librerias
library(arules)
library(arulesViz)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(stringr)
# Load the data set
data_inicial <- read.csv("notes_&_examples/Groceries_dataset.csv", sep = ',') # para ggplot

View(data_inicial)


data_inicial <- data_inicial %>% 
  mutate(
    itemDescription = factor(itemDescription),
    Date = as_date(Date, format = "%d-%m-%Y")
  )

data_csv <- data_inicial %>% 
  arrange(Member_number) %>% 
  group_by(Date, Member_number) %>% 
  mutate(
    itemList = glue::glue_collapse(glue::glue("{itemDescription}"), sep = ',')
  )

data_csv$Member_number <- NULL
data_csv$Date <- NULL
data_csv$itemDescription <- NULL
data_csv$itemList <- as.character(data_csv$itemList)

View(data_csv)
# Escribo data transaccional
write.csv(data_csv, "ItemList.csv", quote = FALSE, row.names = TRUE)

# Leo el csv para obtener transacciones en formato Basket

transacciones <- read.transactions(
  "ItemList.csv", 
  rm.duplicates = TRUE, 
  format = "basket",
  sep = ",", 
  cols = 1
  )
View(transacciones)

# Quito Quotes
transacciones@itemInfo$labels <- gsub("\"","",transacciones@itemInfo$labels)
View(transacciones@itemInfo$labels)

# 2. Grafico de frecuencia de items
# Create an item absolute frequency plot for the top 20 items
itemFrequencyPlot(
  transacciones,
  topN = 20,
  type = "absolute",
  main = "Frecuencia Absoluta"
  )
  
# frecuencia relativa
# Relative Item Frequency Plot
itemFrequencyPlot(
  transacciones, 
  topN = 15, 
  type = "relative",
  ylab = "Frequency (relative)",
  main="Frecuencia Relativa"
  )

# Utilicemos ggplot2 para ver la distribucion de los productos y transacciones por fecha

# Transacciones por Mes
# Esto sirve sobre data original sin eliminar data
data_inicial %>% 
  mutate(
    Month = as.factor(month(Date))
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

# Transacciones por dia de la semana

data_inicial %>% 
  mutate(
    DiaSemana = as.factor(weekdays(as.Date(Date)))
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

# Algoritmo apriori

# El primer paso para crear un conjunto de reglas de asociacion es determinar el nivel optimo de soporte y confianza.
# Si estos valores son muy bajos, entonces el algoritmo va a tomar mas tiempo en ejecutar y vamos a obtener un gran
# numero de reglas (la mayoria no seran utiles). Entones que valores elegimos? La idea para resolver esto es probar 
# diferentes valores de soporte y confianza y analizar graficamente cuantas reglas se generan con cada combinacion.

# Support and confidence values
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
}

# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
  rules_sup10[i] <- length(
    apriori(
      transacciones, 
      parameter = list(
        minlen = 1,
        sup = supportLevels[1],
        conf = confidenceLevels[i],
        target = "rules"
        )
      )
    )
}

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)) {
  rules_sup5[i] <- length(
    apriori(
      transacciones,
      parameter = list(
        minlen = 1, 
        sup = supportLevels[2], 
        conf = confidenceLevels[i],
        target = "rules"
        )
      )
    )
}

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels)) {
  rules_sup1[i] <- length(
    apriori(
      transacciones,
      parameter = list(
        sup = supportLevels[3], 
        conf = confidenceLevels[i],
        target = "rules"
        )
      )
    )
}

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)) {
  rules_sup0.5[i] <- length(
    apriori(
      transacciones,
      parameter = list(
        sup = supportLevels[4], 
        conf = confidenceLevels[i],
        target = "rules"
        )
      )
    )
}

# Apriori algorithm with a support level of 0.1%
for (i in 1:length(confidenceLevels)) {
  rules_sup0.1[i] <- length(
    apriori(
      transacciones,
      parameter = list(
        sup = supportLevels[4], 
        conf = confidenceLevels[i],
        target = "rules"
      )
    )
  )
}

# Ejercicio colaborativo en clase
# Iteracion Anidada con purrr

iteracion_parametros <- purrr::map_dfr(
  .x = supportLevels,
  .f = function(x) {
    
    purrr::map(
      .x = confidenceLevels,
      .f = function(.x) {
        
        data.frame(
          support = x,
          confidence = .x,
          rules = length(
            apriori(transacciones, parameter = list(minlen = 1, sup = x, conf = .x, target = "rules")
            )
          )
        )
      }
    )
  }
)  

# Number of rules found with a support level of 10%
plot1 <- qplot(
  confidenceLevels,
  rules_sup10,
  geom = c("point", "line"), 
  xlab = "Confidence level", ylab = "Number of rules found", 
  main = "Apriori with a support level of 10%"
  ) +
  theme_bw()

# Number of rules found with a support level of 5%
plot2 <- qplot(
  confidenceLevels,
  rules_sup5,
  geom = c("point", "line"), 
  xlab = "Confidence level", ylab = "Number of rules found", 
  main = "Apriori with a support level of 5%"
  ) + 
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  theme_bw()

# Number of rules found with a support level of 1%
plot3 <- qplot(
  confidenceLevels,
  rules_sup1,
  geom = c("point", "line"), 
  xlab = "Confidence level", ylab = "Number of rules found", 
  main = "Apriori with a support level of 1%"
  ) + 
  scale_y_continuous(breaks = seq(0, 50, 10)) +
  theme_bw()

# Number of rules found with a support level of 0.5%
plot4 <- qplot(
  confidenceLevels,
  rules_sup0.5, 
  geom = c("point", "line"), 
  xlab = "Confidence level", ylab = "Number of rules found", 
  main = "Apriori with a support level of 0.5%"
  ) + 
  scale_y_continuous(breaks = seq(0, 130, 20)) +
  theme_bw()

# Subplot
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

# Data frame
num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5, confidenceLevels)
View(num_rules)

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

# Interpretacion

# Con un nivel de soporte de 0.5% hay demasiados resultados o reglas con muy poca confianza
# La idea aaqui es elegir un nuvel de confianza y soporte razonable segun los resultados. 
# Lo mejor que pudimos lograr fue encontrar una relacion para un nivel de soporte de 10% y un nivel de confianza de 20%

# Modificar vectores de hiperparametros
# Es importante aqui el hiperparametro minlen ya que define la minima cantidad de relaciones para cada producto
# Cuanto menor sea este numero, mayor la cantidad de relaciones.
# Se recomienda jugar con este parametro para poder observar la diferencia de los resultados
rules_sup1_conf10 <- apriori(
  transacciones, 
  parameter = list(
    minlen = 4,
    sup = 0.001, 
    conf = 0.2, 
    target = "rules"
  )
)

# Las asociaciones generadas son las siguientes
inspect(rules_sup1_conf10)

# Como interpretamos estos resultados?
# El 85% de los clientes que compraron "other vegetables, pastry y soda" tambien compran "whole milk".
# El razonamiento es el mismo para cada regla creada.

# Para visualizar esta informacion y analizar las distintas reglas y su nivel de confianza utilizamos el siguiente comando
# El scatterplot sirve para visualizar todos los puntos en un grafico, no es particularmente util para entender como se relaciona
# cada producto entre si, sino mas que nada sirve para ver como se distribuyen los puntos y que nivel de confianza y soporte tienen 
plot(rules_sup1_conf10, measure = c("support", "lift"), shading = "confidence")

# Grafico de asociaciones.
plot(rules_sup1_conf10, method = "graph")

# Forma alternativa de graficar asociaciones, considerar que esta visualizacion puede tardar un tiempo considerable si se trata 
# con un gran numero de reglas
plot(rules_sup1_conf10, method = "paracoord")


