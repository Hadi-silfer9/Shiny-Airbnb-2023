library(caret)
library(randomForest)


data_cities<-read.table("Airbnb_Cities_Europ.csv",header=TRUE,sep=";",stringsAsFactors = TRUE,dec=",")
data_cities<-data_cities[,-1]
View(data_cities)

set.seed(1234)
train_indices <- createDataPartition(data_cities$Price, p = 0.8, list = FALSE)
train_data <- data_cities[train_indices, ]
test_data <- data_cities[-train_indices, ]


model_rf <- randomForest(Price ~ City + Weekdays_WE + room_type + person_capacity + lng + lat  , data = train_data)

# Prédictions sur l'ensemble de test
predictions <- predict(model_rf, newdata = test_data)

# Calcul de l'erreur quadratique moyenne (RMSE)
rmse <- sqrt(mean((predictions - test_data$Price)^2))
cat("RMSE:", rmse, "\n") 

#En moyenne, les prédictions de notre modèle diffèrent de 189.0294 euros par rapport aux vraies valeurs, ceci peut être satisfaisant compte tenu de la large plage des Prix.


# Calcul de l'erreur absolue moyenne (MAE)
mae <- mean(abs(predictions - test_data$Price))
cat("MAE:", mae, "\n")

#RMSE (Root Mean Squared Error) et MAE (Mean Absolute Error) sont utilisés pour quantifier l'écart entre 
#les prédictions d'un modèle et les vraies valeurs. 

saveRDS(model_rf, "model_rf.rds")







