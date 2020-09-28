install.packages("e1071")
install.packages("caret")
install.packages("sqldf")
install.packages("ggthemes")
install.packages("scales")
install.packages("ggplot2")

library(scales)
library(ggthemes)
library(e1071)
library(caret) #this package has the createDataPartition function
library(dplyr)
library(tidyverse)            
library(knitr)
library(Matrix)
library(recommenderlab)
library(data.table)
library(sqldf)
library(ggplot2)

dataEventos <- read.csv("D:/Mis Documentos/UNI/IAA/Modelo de recomendacion/dataEventos.csv", sep=";")
event_calendar_atender <- read.csv("D:/Mis Documentos/UNI/IAA/Modelo de recomendacion/event_calendar_atender_2808.csv")

# DATA EVENTOS (Quienes crearon los eventos)

View(dataEventos)
View(event_calendar_atender)
     
dataEventos$event_id <- as.factor(dataEventos$event_id)
dataEventos$user_id <- as.factor(dataEventos$user_id)
dataEventos$category <- as.factor(dataEventos$category)
dataEventos$category_string <- as.factor(dataEventos$category_string)
dataEventos$day_string <- as.factor(dataEventos$day_string)
dataEventos$month_string<- as.factor(dataEventos$month_string)
dataEventos$key_words<- as.factor(dataEventos$key_words)
dataEventos$organizer<- as.factor(dataEventos$organizer)
dataEventos$time_stamp<- as.numeric(dataEventos$time_stamp)
dataEventos$event_tip <- as.factor(dataEventos$event_tip)
dataEventos$event_exp <- as.factor(dataEventos$event_exp) # Creada a partir de la descripci�n del evento

table(dataEventos$category) #328 eventos tienen categoria #se propone categorizar los otros eventos en base a su descripci�n a futuro
str(dataEventos$category_string) # 12 categorias en la data

#Lista de Categorias
item_list <- dataEventos  %>% 
  select(category_string) %>% 
  unique()

#Categorias m�s populares

dataEventos %>% 
  group_by(category_string) %>% 
  summarize(count = n())  %>%
  arrange(desc(count))

dataEventos %>% 
  group_by(event_tip) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

dataEventos %>% 
  group_by(day_string) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

dataEventos %>% 
  group_by(event_exp) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))


# DATA EVENT CALENDAR (asistentes a eventos)

event_calendar_atender_v1$event_id<- as.factor(event_calendar_atender_v1$event_id)
event_calendar_atender_v1$user_id<- as.factor(event_calendar_atender_v1$user_id)
event_calendar_atender_v1$time_stamp<- as.numeric(event_calendar_atender_v1$time_stamp)

event_calendar_atender %>% 
  group_by(title) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

event_calendar_atender %>% 
  group_by(user_id) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

#Unimos la data

dataEventos_v1 <- dataEventos %>% select(1,2,3,14,14,29)
dataEventos_v1$rating <- 3  #Si creaste un evento se asume un rating intermedio del evento
event_calendar_atender_v1$category_string <- "IT"
event_calendar_atender_v1 <- event_calendar_atender %>% select(2,3,4,7)
event_calendar_atender_v1$rating <- 5 #si asistes al evento se asume el mayor ranking posible

xdata <- rbind(dataEventos_v1,event_calendar_atender_v1)
for(i in 1:nrow(xdata)){
  if(xdata[i,5] == ""){
    xdata[i,5] <- "IT"
  }
}

#Dataset de prueba
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = xdata$rating, 
                                  times = 1, p = 0.1, list = FALSE)
edx <- xdata[-test_index,]
temp <- xdata[test_index,]

validation <- temp %>% 
  semi_join(edx, by = "event_id") %>%
  semi_join(edx, by = "user_id")

#Categorias
xdata %>% group_by(category_string) %>% 
  summarise(n=n())

#N�mero de ratings por evento
xdata %>% group_by(event_id) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white", bins = 10) +
  scale_x_log10() + 
  ggtitle("Distribuci�n de los eventos", 
          subtitle = "La distribuci�n es casi sim�trica") +
  xlab("n�mero de valoraciones recibidas") +
  ylab("N�mero de eventos") +   
  theme_economist()

xdata %>% group_by(user_id) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white", bins = 10) +
  scale_x_log10() + 
  ggtitle("Distribuci�n de usuarios") +
  xlab("n�mero de valoraciones dadas") +
  ylab("N�mero de usuarios") + 
  scale_y_continuous(labels = comma) + 
  theme_economist()


#Mapa de calor usuarios por evento

users <- sample(unique(xdata$user_id), 13)
xdata <- xdata[!duplicated(xdata[c(1,2)]),]

xdata %>% filter(user_id %in% users) %>%
  select(user_id, event_id, rating) %>%
  mutate(rating = 1) %>%
  spread(event_id, rating) %>% 
  select(sample(ncol(.), 13)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:13, 1:13,. , xlab="Eventos", ylab="Usuarios")%>%
  abline(h=0:13+0.5, v=0:13+0.5, col = "grey")%>%
  title("Matriz Usuario por Evento")
#podemos observar como un usuario tiene bastante actividad y todos los eventos tienen actividad parecida


#TRAIN TEST
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = xdata$rating, times = 1, p = 0.1, list = FALSE)
train_set <- xdata[-test_index,]
temp <- xdata[test_index,]

test_set <- temp %>% 
  semi_join(train_set, by = "event_id") %>%
  semi_join(train_set, by = "user_id")

removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)
rm(test_index, temp, removed)

#MODELO
set.seed(4321, sample.kind = "Rounding")

p <- function(x, y) mean(y == x)
rating <- seq(0.5,5,0.5)

# Estimamos la probabilidad para cada rating con la simulaci�n de Monte Carlo.
B <- 10^3

M <- replicate(B, {
  s <- sample(train_set$rating, 100, replace = TRUE)
  sapply(rating, p, y= s)
})

prob <- sapply(1:nrow(M), function(x) mean(M[x,]))

y_hat_random <- sample(rating, size = nrow(test_set), 
                       replace = TRUE, prob = prob)

result <- tibble()

result <- bind_rows(result, 
                    tibble(Method = "Predicci�n aleatoria", 
                           RMSE = RMSE(test_set$rating, y_hat_random),
                           MSE  = MSE(test_set$rating, y_hat_random),
                           MAE  = MAE(test_set$rating, y_hat_random)))

result


#Modelo Lineal

# promedio de los valores observados
mu <- mean(train_set$rating)
mu <- as.vector(mu)
mu <- c(mu,mu,mu)

# Actualizando la tabla de errores 
result <- bind_rows(result, 
                    tibble(Method = "Promedio", 
                           RMSE = RMSE(test_set$rating, mu),
                           MSE  = MSE(test_set$rating, mu),
                           MAE  = MAE(test_set$rating, mu)))


# Mostrando el nuevo valor RMSE
result

#Incluir el efecto del evento (bi)
#bi es el efecto del evento (bias) para el evento i.
#Efecto del evento (bi)
bi <- train_set %>% 
  group_by(event_id) %>% 
  summarize(b_i = mean(rating-mu))

y_hat_bi <- mu + test_set %>% 
  left_join(bi, by = "event_id") %>% 
  .$b_i

result <- bind_rows(result, 
                    tibble(Method = "Promedio + bi", 
                           RMSE = RMSE(test_set$rating, y_hat_bi),
                           MSE  = MSE(test_set$rating, y_hat_bi),
                           MAE  = MAE(test_set$rating, y_hat_bi)))
result

# Incluiyendo el efecto del usuario (bu)
bu <- train_set %>% 
  left_join(bi, by = 'event_id') %>%
  group_by(user_id) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Predicci�n
y_hat_bi_bu <- test_set %>% 
  left_join(bi, by='event_id') %>%
  left_join(bu, by='user_id') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# Actualizando la tabla de resultados
result <- bind_rows(result, 
                    tibble(Method = "promedio + bi + bu", 
                           RMSE = RMSE(test_set$rating, y_hat_bi_bu),
                           MSE  = MSE(test_set$rating, y_hat_bi_bu),
                           MAE  = MAE(test_set$rating, y_hat_bi_bu)))

# Mostrando el nuevo valor RMSE
result

#Evaluando los resultados del modelo

train_set %>% 
  left_join(bi, by='event_id') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%  
  slice(1:10)

titles <- train_set %>% 
  select(event_id, title) %>% 
  distinct()

#Mejores eventos (rankeadas por bi).
bi %>% 
  inner_join(titles, by = "event_id") %>% 
  arrange(-b_i) %>% 
  select(title) %>%
  head(5)

#Regularizaci�n

regularization <- function(lambda, trainset, testset){
  
  # Promedio
  mu <- mean(trainset$rating)
  
  # Efecto del evento (bi)
  b_i <- trainset %>% 
    group_by(event_id) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda))
  
  # Efecto del usuario (bu)  
  b_u <- trainset %>% 
    left_join(b_i, by="event_id") %>%
    filter(!is.na(b_i)) %>%
    group_by(user_id) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
  
  # Predicci�n: mu + bi + bu  
  predicted_ratings <- testset %>% 
    left_join(b_i, by = "event_id") %>%
    left_join(b_u, by = "user_id") %>%
    filter(!is.na(b_i), !is.na(b_u)) %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
}


lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, 
                regularization, 
                trainset = train_set, 
                testset = test_set)

tibble(Lambda = lambdas, RMSE = rmses) %>%
  ggplot(aes(x = Lambda, y = RMSE)) +
  geom_point() +
  ggtitle("Regularizaci�n", 
          subtitle = "Escoger la penalizaci�n que genera el menor RMSE") +
  theme_economist()

# Escogemos el lambda que genera el menor RMSE
lambda <- lambdas[which.min(rmses)]

# Ahora calculamos el rating predecido usando los mejores par�metros. 
# Logrado por la penalizaci�n
mu <- mean(train_set$rating)


# Efecto de la evento (bi)
b_i <- train_set %>% 
  group_by(event_id) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

# Efecto del usuario(bu)
b_u <- train_set %>% 
  left_join(b_i, by="event_id") %>%
  group_by(user_id) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

# Predicci�m
y_hat_reg <- test_set %>% 
  left_join(b_i, by = "event_id") %>%
  left_join(b_u, by = "user_id") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Actualizamos la tabla de resultados
result <- bind_rows(result, 
                    tibble(Method = "Regularizado en base al bi y al bu", 
                           RMSE = RMSE(test_set$rating, y_hat_reg),
                           MSE  = MSE(test_set$rating, y_hat_reg),
                           MAE  = MAE(test_set$rating, y_hat_reg)))

# resultados con regularizaci�n
result  

#Matriz de factorizaci�n

train_data <- train_set %>% 
  select(user_id, event_id, rating) %>% 
  spread(event_id, rating) %>% 
  as.matrix()

if(!require(recosystem)) 
  install.packages("recosystem", repos = "http://cran.us.r-project.org")
set.seed(123, sample.kind = "Rounding")

train_data <-  with(train_set, data_memory(user_index = user_id, 
                                           item_index = event_id, 
                                           rating     = rating))
test_data  <-  with(test_set,  data_memory(user_index = user_id, 
                                           item_index = event_id, 
                                           rating     = rating))

# Creando un modelo objeto
r <-  recosystem::Reco()

# Seleccionando los mejores parametros a trav�s del tuneo
opts <- r$tune(train_data, opts = list(dim = c(10, 20, 30), 
                                       lrate = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1), 
                                       costq_l2 = c(0.01, 0.1),
                                       nthread  = 4, niter = 10))

# Entrenando al algoritmo
r$train(train_data, opts = c(opts$min, nthread = 4, niter = 20))

y_hat_reco <-  r$predict(test_data, out_memory())
head(y_hat_reco, 10)

result <- bind_rows(result, 
                    tibble(Method = "Matrix Factorization - recosystem", 
                           RMSE = RMSE(test_set$rating, y_hat_reco),
                           MSE  = MSE(test_set$rating, y_hat_reco),
                           MAE  = MAE(test_set$rating, y_hat_reco)))
result



#Modelo lineal con regularizaci�n

mu_edx <- mean(xdata$rating)

# Efecto del evento (bi)
b_i_edx <- xdata %>% 
  group_by(event_id) %>%
  summarize(b_i = sum(rating - mu_edx)/(n()+lambda))

# Efecto del usuario (bu)
b_u_edx <- xdata %>% 
  left_join(b_i_edx, by="event_id") %>%
  group_by(user_id) %>%
  summarize(b_u = sum(rating - b_i - mu_edx)/(n()+lambda))

# Predicci�n
y_hat_edx <- validation %>% 
  left_join(b_i_edx, by = "event_id") %>%
  left_join(b_u_edx, by = "user_id") %>%
  mutate(pred = mu_edx + b_i + b_u) %>%
  pull(pred)

# Actualizando la tabla de resultados
result <- bind_rows(result, 
                    tibble(Method = "Final Regularization (edx vs validation)", 
                           RMSE = RMSE(validation$rating, y_hat_edx),
                           MSE  = MSE(validation$rating, y_hat_edx),
                           MAE  = MAE(validation$rating, y_hat_edx)))

result 

#Mejores eventos
validation %>% 
  left_join(b_i_edx, by = "event_id") %>%
  left_join(b_u_edx, by = "user_id") %>% 
  mutate(pred = mu_edx + b_i + b_u) %>% 
  arrange(-pred) %>% 
  group_by(title) %>% 
  select(title) %>%
  head(5)

#Matriz de factorizaci�n

set.seed(1234, sample.kind = "Rounding")

# Convirtiendo los dataframes de 'edx' y 'validation' a a un formato de entrada recosystem 
edx_reco <-  with(edx, data_memory(user_index = user_id, 
                                   item_index = event_id, 
                                   rating = rating))
validation_reco  <-  with(validation, data_memory(user_index = user_id, 
                                                  item_index = event_id, 
                                                  rating = rating))

# Creando el modelo objeto
r <-  recosystem::Reco()

# Tuneando los par�metros
opts <-  r$tune(edx_reco, opts = list(dim = c(10, 20, 30), 
                                      lrate = c(0.1, 0.2),
                                      costp_l2 = c(0.01, 0.1), 
                                      costq_l2 = c(0.01, 0.1),
                                      nthread  = 4, niter = 10))

# Modelo de entrenamiento
r$train(edx_reco, opts = c(opts$min, nthread = 4, niter = 20))

# Calculamos la predicci�n
y_hat_final_reco <-  r$predict(validation_reco, out_memory())

# Actualizamos la tabla de predicciones
result <- bind_rows(result, 
                    tibble(Method = "Matriz Final de Factorizaci�n - recosystem", 
                           RMSE = RMSE(validation$rating, y_hat_final_reco),
                           MSE  = MSE(validation$rating, y_hat_final_reco),
                           MAE  = MAE(validation$rating, y_hat_final_reco)))

result

#mejores 10 eventos
  
tibble(title = validation$title, rating = y_hat_final_reco) %>%
arrange(-rating) %>% 
group_by(title) %>% 
select(title) %>%
head(10)
A