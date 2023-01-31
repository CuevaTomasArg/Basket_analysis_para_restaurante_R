library(dplyr)
library(e1071)
library(caTools)
library(class)
library(ggplot2)

# 1. Clasificacion con KNN ------------------------------------------------

data(iris)
glimpse(iris)

# Split en training y testing sets
split <- sample.split(iris, SplitRatio = 0.7)
train_cl <- subset(iris, split == "TRUE")
test_cl <- subset(iris, split == "FALSE")

# Normalizacion de variables
train_scale <- scale(train_cl[, 1:4])
test_scale <- scale(test_cl[, 1:4])

# Corremos el modelo KNN para un solo K a modo de prueba
classifier_knn <- knn(
  train = train_scale,
  test = test_scale,
  cl = train_cl$Species,
  k = 1
)

classifier_knn

# Matriz de Confusion
cm <- table(test_cl$Species, classifier_knn)
cm

# Iteracion para distintos K
k <- c(3:19)

knn_results <- purrr::map_dfr(
  .x = k,
  .f = function(x) {
    
    classifier_knn <- knn(
      train = train_scale,
      test  = test_scale,
      cl    = train_cl$Species,
      k     = x
    )
    
    tibble(
      k = x,
      Accuracy = (1 - mean(classifier_knn != test_cl$Species))
    )
  }
)

knn_results %>% 
  ggplot(aes(x = reorder(k, Accuracy, desc), y = Accuracy)) +
  scale_y_continuous(breaks = seq(0, 1, 0.05), labels = scales::percent_format(accuracy = 1)) +
  geom_col(fill = "#6495ED", colour = "grey", alpha = 0.8) +
  xlab("K Vecinos") +
  theme_bw()
  

# 2.  Regresion con KNN ---------------------------------------------------

# Para predecir variables continuas con KNN vamos a utilizar el paquete caret.
# Esta libreria cuenta una gran cantidad de flexibilidad a la hora de especificar modelos de aprendizaje automaitco
# Fue la libreria predecesora del paradigma tidymodels, con lo cual si bien existen mejores alternativas, sirve conocer el paquete dado que 
# sigue siendo extremadamente util para ciertos ejercicios.

# Utilizarmos la data de viviendas de Boston, que viene con la libreria MASS. 
# El dataset contiene precios de viviendas segun distintos atributos y caracteristicas de las mismas.

# Queremos predecir la columna "medv" que signica "medium value" o valor medio de una propiedad

library(MASS)
library(caret)

data(Boston)
glimpse(Boston)


# Para fitear el modelo de regresion de KNN, utilizaremos la funcion train de caret.
# La funcion train, sirve para fitear cualquier cantidad de modelos, ademas auomaticamente realiza optimizacion de hiperparametros.
# Con lo cual el esfuerzo requerido para generar un modelo relativamente bueno con esta funcion es muy bajo.
# En primera instancia definiremos la ecuacion del modelo. "medv ~ ." que es el valor medio explicado por todos los predictores o todo el resto de las variables.
# En segundo lugar pasaremos la data que estamos utilizando.
# Finalmente especificamos el metodo que queremos utilizar, "knn".

set.seed(1)
model <- train(
  medv ~ .,
  data = Boston,
  method = "knn"
)
model
plot(model)

# Otra funcionalidad del paquete caret, es la posibilidad de poder preprocesar la data previo a ejecutar el algoritmo.
# agregamos otro parametro mas para normalizar las variables

set.seed(1)
model2 <- train(
  medv ~ .,
  data = Boston,
  method = 'knn',
  preProcess = c("center", "scale")
)

# Ahora particionamos la data en conjuntos de entrenamiento y testing, para verificar que no estamos sobreajustando el modelo (overfitting).

inTraining <- createDataPartition(Boston$medv, p = .80, list = FALSE)
training <- Boston[inTraining,]
testing  <- Boston[-inTraining,]

# Entrenamos el model con la data training que creamos
model3 <- train(
  medv ~ .,
  data = training,
  method = 'knn',
  preProcess = c("center", "scale")
)
model3

# Y ahora queremos comprar las predicciones sobre el conjunto de testing.

test_features <- subset(testing, select = -medv)
test_target <- subset(testing, select = medv)[,1]

# Utilizamos la funcion predict para predecir con el ultimo modelo generado

predictions <- predict(model3, newdata = test_features)

# RMSE: Root Mean Squared Error o Raiz de Error Cuadratico Medio 

sqrt(mean((test_target - predictions)^2))

# R2: Bondad de Ajuste. Que porcentaje de la variabilidad de la variable medv podemos explicar con nuestro modelo?

cor(test_target, predictions) ^ 2

# K-Fold Cross-Validation
# En la practica no utilizamos unicamente un solo conjunto de entrenamiento y de testing
# Sino que generamos multiples y generamos un modelo para cada particion generada, finalmente
# el modelo obtenido es un promedio de todos los modelos. Este metodo es mas robusto dado que prueba sobre todas las particiones de la data
# en lugar de utilizar unicamente una sola particion. 
# Por eso se denomina K Fold, se establecen K particiones para validar el modelo para cada particion generada.


# Generamos las particiones de cross validation, en este caso 10.
set.seed(1)
ctrl <- trainControl(
  method = "cv",
  number = 10,
)

# Pasamos las particiones al modelo para entrenar el modelo para cada una de ellas.
set.seed(1)
model4 <- train(
  medv ~ .,
  data = training,
  method = 'knn',
  preProcess = c("center", "scale"),
  trControl = ctrl
)

# Al parecer los resultados fueron mejores para el conjunto de entremaniento.
# Verifiquemos que el modelo mejoro comparando contra el conjunto de testing.

test_features <- subset(testing, select = -medv)
test_target <- subset(testing, select = medv)[,1]

predictions <- predict(model4, newdata = test_features)

# RMSE
sqrt(mean((test_target - predictions)^2))

# R2
cor(test_target, predictions) ^ 2

# Mejoramos el ajuste a 82.2%.

# * 1.  Optimizacion de Hiperparametros -----------------------------------

# Vamos a correr el modelo para distintos valores de K, entrentando con cross-validation como ya vimos


set.seed(1)

tuneGrid <- expand.grid(
  k = seq(5, 9, by = 1)
)

model5 <- train(
  medv ~ .,
  data = training,
  method = 'knn',
  preProcess = c("center", "scale"),
  trControl = ctrl,
  tuneGrid = tuneGrid
)
model5$results
plot(model5)


