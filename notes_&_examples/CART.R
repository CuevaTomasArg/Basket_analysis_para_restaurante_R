library(dplyr)
library(tidymodels)
library(rpart.plot)
tidymodels_prefer()

data <- read.csv("titanic.csv", sep = ",")

head(data)
tail(data)

# Podemos ver que la data no esta mezclada, dado que esta ordenada por la clase de los pasajeros. Esto implicaria que al momento de dividir en los conjuntos de entrenamiento y testing
# que al momento de entrenar, el modelo lo haria unicamente con data de las clases 1 y 2, sin tener ninguna observacion de la clase 3, esto puede llevar a que el modelo sea malo
# y muy posiblemente tenga muy mala performance al clasificar la tasa de supervivencia de la clase que no esta contenida en el conjunto de entrenamiento o training.

# Al encarar este tipo de problemas de clasificacion, es importante siempre analizar si la data esta ordenada, para no tener este tipo de problemas.

# Por esta razon, vamos a utilizar la funcion sample para generar aleatoriamente un vector de indices con la misma longitud del dataset para reordenar las observaciones.

indices_aleatorios <- sample(1:nrow(data))

data <- data[indices_aleatorios, ]

# A continuacion vamos a preprocesar la data para que no haya NAs y eliminar variables que no van a servir para el modelo

# El nombre del pasajero no nos interesa para saber si sobrevivio o no, el numero de la cabina tampoco, el destino de su hogar tampoco es relevante y la variable x, que es un indice que surge de utilizar sample tampoco sirve
# Convertimos las variables pclass y survived a factor y agregamos labels para distinguirlas mas facilmente, finalmente con na.omit() eliminamos observaciones con datos NA
# Tener en cuenta que aqui tuvimos que quitar unos signos de interrogacion en las variables edad (age) y precio de boleto (fare), reemplazamos dicho caracter por un NA para luego omitir estos casos on drop_na

data <- data %>% 
  mutate(
    pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
    survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes')),
    sex = factor(sex),
    age = as.integer(gsub("\\?", NA, age)),
    fare = as.numeric(gsub("\\?", NA, fare)),
    embarked = factor(embarked)
    ) %>%
  drop_na() %>% 
  select(-c(home.dest, cabin, name, x, ticket))
  
# Creamos el split entre training y testing
data_split <- initial_split(data)
train <- training(data_split)
test <- testing(data_split)

# Generamos K cross validation con la data de entrenamiento
k_folds <- vfold_cv(train)

# Generamos el modelo, utilizando la funcion tune() para cada uno de los tres hiperparametros del algoritmo
# De esta manera elegiremos el modelo con la mejor combinacion, sin que nosotros tengamos que hacer ninguna otra manipulacion.(Tune es uno de los tantos del ecosistema Tidymodels)
# Finalmente especificamos el engine o motor, que en este caso es la libreria "rpart", que es la corre el algoritmo de arboles de decision
# y Elegimos utilizar la modalidad "classification", porque queremos clasificar quienes sobrevivieron, que es un variable factor o categorica. 

# La funcion decision_tree es la que genera la especificacion del modelo.

tree_spec <- decision_tree(
  mode = "classification",
  engine = "rpart",
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
)

# Creamos una serie de valores para cada hiperparametro: complejidad, profundidad y tamanio minimo de nodo
# Elegimos que haya 4 valores para cada hiperparametro

tree_grid <- grid_regular(cost_complexity(), tree_depth(), min_n(), levels = 4)

# Generamos la especifiacion final del modelo por grid search con la funcion tune_grid
# Le pasamos como primer argumento a la especificacion del arbol de decision que queremos correr
# Definimos el modelo, en este caso queremos predecir la probabilidad de sobrevivir (survived) por todas las demas variables (~ .).
# Utiliamos resamples = k_folds, para entrenar el modelo utilizando cross-validation
# grid para correr el modelo para cada combinacion de valores entre los 3 hiperparametros
# finalemnte definimos con metric_set que variables nos interesan usar para evaluar el modelo

set.seed(1)

tree_rs <- tune_grid(
  tree_spec,
  survived ~ .,
  resamples = k_folds,
  grid = tree_grid,
  metrics = metric_set(accuracy, roc_auc, kap, precision, recall, sensitivity)
)

# Obtenemos resultados en un dataframe
model_metrics <- collect_metrics(tree_rs)

# Graficamos modelos
autoplot(tree_rs) 

# Seleccionamos el mejor modelo segun una metrica dada
show_best(tree_rs, "accuracy")
show_best(tree_rs, "kap")
show_best(tree_rs, "roc_auc")
show_best(tree_rs, "sensitivity")

# Seleccionamos el mejor modelo por la metrica kap, que es similar al accuracy
# Para obtener los parametros del modelo

select_best(tree_rs, "kap")

# Elegimos el mejor modelos segun nuestro criterio
final_tree <- finalize_model(tree_spec, select_best(tree_rs, "kap"))
final_tree

# Este modelo"final_tree" esta actualizado y finalizado, es decir ya no se va a optimizar mas, tiene todos los hiperparametros seteados pero por lo pronto el modelo no fue probado con data de testing.
# Para terminar el proceso utilizaremos la funcion last_fit() para refitear el modelo sobre toda la data de entrenamiento para posteriormente evaluarlo con la data de testing.

final_fit <- last_fit(final_tree, survived ~ ., data_split)
final_fit

# Generamos el grafico de arbol final para analizar que variables tuvieron mas incidencia para supervivencia de los pasajeros

final_fit %>% 
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE, extra = 106)

# Metricas del modelo final

collect_metrics(final_fit)

# Matriz de confusion

conf_mat_resampled(final_fit)
