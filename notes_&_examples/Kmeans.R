library(tidymodels)

# Visualizamos Species agrupadas por dos variables al azar
ggplot(iris, aes(x = Sepal.Length, y = Petal.Width, colour = Species)) +
  geom_point() +
  theme_bw()

# Quitamos el label de la data previo a calcular K means
iris_training <- iris %>% 
  select(-Species) %>% 
  scale()

levels(iris$Species)

# Calculamos k means, la funcion solo acepta variables numericas
# Elegimos 3 clusters porque sabemos que en la data hay tres grupos, setosa, virginica, y versicolor
kclust <- kmeans(iris_training, centers = 3)
kclust
summary(kclust)

# Observo estimacion de a que cluster perteneceria cada observacion
augment(kclust, iris_training)

# Sumarizo para cada cluster
tidy(kclust)

# General clustering summary
glance(kclust)

# Supongamos que no conocemos que iris esta distribuido en 3 grupos. Investigo iterando cuantos grupos podria tener.
# Itera de 2 a 9 k (grupos)
kclusts <- tibble(k = 2:9) %>%
  mutate(
    kclust    = map(k, ~ kmeans(iris_training, .x)),
    tidied    = map(kclust, tidy),
    glanced   = map(kclust, glance),
    augmented = map(kclust, augment, iris_training)
  )

# Ordeno la data

# Clusters

clusters <- kclusts %>%
  unnest(cols = c(tidied)) %>% 
  select(-c(glanced, augmented, kclust))

predicciones <- kclusts %>% 
  unnest(cols = c(augmented)) %>% 
  select(k, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, .cluster)
  
resumen <- kclusts %>%
  unnest(cols = c(glanced)) %>% 
  select(-c(kclust, tidied, augmented))

# Elijo graficar solo 2 variables continuas al azar porque el dataset tiene mas de 3 dimensiones para simplificar la visualizacion
ggplot(predicciones, aes(x = Sepal.Width, y = Petal.Length)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  geom_point(data = clusters, size = 5, shape = "+") +
  facet_wrap(~ k) +
  theme_bw()

# Ahora examino que k tiene mas sentido

# Busco rendimientos decrecientes a mayores k
ggplot(resumen, aes(x = k, y = tot.withinss)) +
  scale_x_discrete(name = "k", limits = c(2:9)) +
  geom_point() +
  geom_line() +
  theme_bw()

# Ejemplo en Vivo

diamonds_training <- diamonds %>% 
  select(price, carat) %>% 
  slice_sample(n = 5000)

kclusts_diamonds <- tibble(k = 2:10) %>%
  mutate(
    kclust    = map(k, ~ kmeans(diamonds_training, .x)),
    tidied    = map(kclust, tidy),
    glanced   = map(kclust, glance),
    augmented = map(kclust, augment, diamonds_training)
  )

kclusts_diamonds

clusters <- kclusts_diamonds %>%
  unnest(cols = c(tidied)) %>% 
  select(-c(glanced, augmented, kclust))

predicciones <- kclusts_diamonds %>% 
  unnest(cols = c(augmented)) %>% 
  select(k, price, carat, .cluster)

resumen <- kclusts_diamonds %>%
  unnest(cols = c(glanced)) %>% 
  select(-c(kclust, tidied, augmented))

# Elijo graficar solo 2 variables continuas al azar porque el dataset tiene mas de 3 dimensiones para simplificar la visualizacion
ggplot(predicciones, aes(x = carat, y = price)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  geom_point(data = clusters, size = 5, shape = "+") +
  facet_wrap(~ k) +
  theme_bw()

# Ahora examino que k tiene mas sentido
# Busco rendimientos decrecientes a mayores k
ggplot(resumen, aes(x = k, y = tot.withinss)) +
  scale_x_discrete(name = "k", limits = c(2:10)) +
  geom_point() +
  geom_line() +
  theme_bw()




