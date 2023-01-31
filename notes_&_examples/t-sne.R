library(vroom)
library(Rtsne)

# que es t-SNE

# es un algoritmo para reducir la dimensionalidad de datos de manera no lineal. Mapea data multidimensional a dos
# o mas dimensiones, Con la ayuda de t-SNE, es posible graficar una menor cantidad de variables para explicar
# data multidimensional

# Que es la reduccion de la dimensionalidad?

# En terminos sencillos, la reduccion de la dimensionalidad es una tecnica para representar data multidimensional 
# en dos o tres dimensiones

# Imaginen si tienen un dataset con deces de columnas. Imaginan analizarlo creando graficos normales de barras 
# o con scatterplots para cada combinacion de variable? 

# Con tecnicas de reduccion de dimensionalidad es que se puede visualizar semejantes conjuntos de datos para lograr
# una comprension global de los mismos.


# MOVER ESTO A SCRIPT PCA
# Limitaciones de PCA 

# PCA es un algoritmo lineal, por lo tanto, no sera capaz de interpretar relaciones complejas polinomiales entre las 
# variables. Por otro lado t-SNE utiliza distribuciones de probabolidades para encontrar estructura en los datos.



# Notas sobre t-SNE:

# El algoritmo es considerablemente mas costoso para correr en terminos computacionales, dado que el algoritmo debe computar las probabilidades condicionales para cada par de variables y minimizar la suma de las diferencias de las probabilidades en altas y bajas dimensiones.
# Este proceso acarrea una gran cantidade de computo, es por eso que requiere de amplios recursos de sistema.
# En especial cuando se utilizan datasets de mas de 10.000 observaciones.

# Que es lo que t-SNE hace?

# t-SNE es un algoritmo no lineal para reducir la dimensionalidad de la data. Esto lo consigue identificando clusters
# basados en la similitud de la data. Mapea la data multidimensional a un espacio dimensional mas reducido, donde las
# variables input ya no son identificables. Por lo tanto no se puede realizar inferencias en base al output del algoritmo.
# Su utilidad es primordialmente para explorar y visualizar datos.

# Sin embargo, el output de t-SNE puede ser utilizado en problemas de clasificacion y clusterizacion, tomando su output
# como un input de otros algorritmos de clasificacion (aprendizaje semi-supervisado).

# Casos de uso:

# t-SNE puede ser utilizado en casi todos los datasets de alta dimensionalidad.

# Para implementar t-SNE en R utilizaremos la librera Rtsne.
# Utilizaremos el dataset MNIST, que puede ser descargado de Kaggle.
# https://www.kaggle.com/competitions/digit-recognizer/data

data <- vroom("train.csv", show_col_types = FALSE)

data <- data %>% 
  mutate(
    label = factor(label)
  )

colors <- rainbow(length(unique(data$label)))
names(colors) <- unique(data$label)

# Ejecutamos el algoritmo para toda la data, excluyendo la primer fila factor
# Medimos tiempo de ejecucion
timestamp()

tsne <- Rtsne(
  data[,-1], 
  dims = 2, # la cantidad dimensiones a las cuales se debe reducir el dataset
  perplexity = 30,
  verbose = TRUE,
  max_iter = 500
  )

timestamp()

## Plotting
# El proceso de graficar semejante cantidad de puntos puede tardar un tiempo considerable
# En caso de no poder graficar, se recomienda filtrar la data original para utilizar una menor cantidad de
# observaciones
plot(tsne$Y, t = 'n', main = "tsne")
text(tsne$Y, labels = data$label, col = colors[data$label])





