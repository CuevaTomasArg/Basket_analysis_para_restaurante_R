library(psych)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggbiplot)

# Voy a obtener la misma cantidad de compoentens principales que la cantidad de variables del dataset
# El parametro scale se utiliza para normalizar las variables y podes comprarlas entre si
pca_mtcars <- prcomp(mtcars, center = TRUE , scale = TRUE)

names(pca_mtcars)

# Muestro resumen de PCA, STD, Proporcion de Varianza y Acumulado
summary(pca_mtcars)

# dentro del objeto creado, x contiene las componentes principales, las cuales se puede utilizar para crear
# scatter plots entre dos componentes

# En este caso graficamos los primeros dos componentes

plot(pca_mtcars$x[,1], pca_mtcars$x[,2], xlab = "PCA 1", ylab = "PCA 2")
 
# Autovalores y autovectores
autovectores <- pca_mtcars$rotation 
autovalores <- pca_mtcars$sdev * pca_mtcars$sdev

# Calculo porcentaje explicado
pca_var_pct <- round(autovalores / sum(autovalores)*100, digits = 2)
barplot(pca_var_pct, main = "Scree Plot", xlab = "Componente Principal", ylab = "Variacion Porcentual")

 # Caluclo correlacion entre data original y componentes principales
round(cor(mtcars, pca_mtcars$x), digits = 3)

# Scree plot
screeplot(pca_mtcars, type = "l", main = "Screeplot mtcars")

 
# ggplot2 

pca_data <- data.frame(
  Modelo = rownames(pca_mtcars$x),
  x = pca_mtcars$x[, 1],
  y = pca_mtcars$x[, 2]
  )

ggplot(data = pca_data, aes(x, y, label = Modelo)) +
  geom_text() +
  xlab(paste0("CP 1: ", pca_var_pct[1],"%")) +
  ylab(paste0("CP 2: ", pca_var_pct[2],"%")) +
  theme_bw() +
  ggtitle("Gráfico PCA")

# El eje X indica cuanta variacion explica el componente principal 1
# El eje y indidca cuanta variacion explica el componente principal 2

# Los scores estan guardados en la variable "rotation"
# Vamos a ver a los scores para cada variable del primer componente dado que es el que mas explica

pca_mtcars$rotation[,1]


# Utilizo la libreria psych para generar un analisis de PCA pero unicamente con dos factores o componentes principales
# en base al grafico anterior

pca_final <- principal(mtcars, nfactors = 2, rotate = "none")

# Observamos el resultado
pca_final 

# En este caso logramos explicar el 84% de la varianza del dataset 
# Podemos seguir agregando factores para lograr un mayor resultado


mtcars_pais <- c(
  rep("Japon", 3),
  rep("EEUU",4),
  rep("Europa", 7),
  rep("EEUU",3), 
  "Europa", 
  rep("Japon", 3), 
  rep("EEUU",4), 
  rep("Europa", 3), 
  "EEUU", 
  rep("Europa", 3)
  )

ggbiplot(pca_mtcars, ellipse = TRUE,  labels = rownames(mtcars), groups = mtcars_pais)





