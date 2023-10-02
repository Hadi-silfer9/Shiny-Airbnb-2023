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

saveRDS(model_rf, "model_rf.rds")





