library(factoextra)
library(cluster)
library(dplyr)

table(iris$Species)

species <- iris$Species

iris_std <- iris %>% 
  select(-Species) %>% 
  scale()

iris_dist <- iris_std %>% 
  dist()

# por defecto hclust implementa clusterizacion ascendente

hc <- hclust(iris_dist, method = "complete")

# Dendograma
plot(hc)
rect.hclust(hc, k = 3, border = 3:5)

# Clusters: Corto el arbol resultante de hclust, en la cantidad de clusters que queramos segun lo visto en el dendograma anterior
# Queremos 3 grupos en base al grafico y con la siguiente funcion obtenemos a que cluster pertenece cada observacion segun el algoritmo de hc
# Cantidad de miembros en cada cluster. Sabemos que los grupos estan perfectamente balanceados en el dataset original, con lo cual la clusterizacion no fue perfecta.

clusters <- cutree(hc, k = 3)
table(clusters, iris$Species)

# Visualizo

rownames(iris_std) <- paste0(iris$Species, "_", 1:nrow(iris))

factoextra::fviz_cluster(
  object = list(
    data = iris_std,
    cluster = clusters
  ),
  ggtheme = theme_bw()
)

# La peor clasificacion fue con las versicolor

# Determino cantidad optima de clusters
# hcut fitea un modelo de hclust y posteriormente corta el arbol
fviz_nbclust(iris_std, FUNcluster = hcut, method = "wss")

# En el caso de hierachical clustering hay varios temas a considerar:

# 1. Que medida de similitud utilizar?
# 2. Que tipo de linkeo usar?
# 3. Donde deberiamos cortar el dendograma para obtener los clusters?

# Cada una de estas decisiones tienen un fuerte impacto en los resultados obtenidos. En la practica, tipicamente se prueban distintas opciones, y se observa
# cual es la mas util y con la mejor interpretacion posible para el problema especifico. Con lo cual si bien, implementar HC en R es sencillo, hay multiples 
# aristas que considerar a la hora decidir como encarar un problema no supervisado y tambien como interpretar los resultados de la mejor manera posible.

# Extra
cluster_method <- c( "average", "single", "complete", "ward")
names(cluster_method) <- cluster_method

hc_list <- purrr::map(
  .x = cluster_method,
  .f = function(x) {
    agnes(iris_dist, method = x)
  } 
)

# El mejor metodo de clustering es el de Ward, dado que tiene el mejor coeficiente de aglomeracion.
hc_list

# Repito proceso anterior pero con metodo de Ward, vuelvo a graficar y comparo tabla de clusters

hc_ward <- hcut(iris_dist, k = 3, hc_method = "ward.D2")
plot(hc_ward)
rect.hclust(hc_ward, k = 3, border = 1:3)

# Nuevos resultados
table(hc_ward$cluster, iris$Species)

# Resultados anteriores
table(clusters, iris$Species)


