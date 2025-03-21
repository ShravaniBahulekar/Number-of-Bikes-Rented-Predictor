setwd("C:/Data science")

#libraries----
library(caTools)
library(rpart)
library(rpart.plot)
library(Metrics)
library(ggplot2)
library(dslabs)
library(stats)
library(dplyr)
library(quantmod)
library(randomForest)
library(caret)
library(tidyverse)

f = read.csv("SeoulBikeData.csv")
f
summary(f)
str(f)
any(is.na(f)) #finding null values in dataset


#Dropping non useful Columns----
f<-f[!(f$Functioning.Day=="No"),]
drop<-c("Date","Functioning.Day")
f1=f[,!(names(f) %in% drop)]
print(f1)
dim(f1)


#Converting column input into numeric----
f2<-transform(f1,Seasons=as.numeric(as.factor(Seasons)),Holiday=as.numeric(as.factor(Holiday)))
print(f2)
boxplot(f2,las=2)


#Normalisation of dataset
n2<-function(b){(b-min(b))/(max(b)-min(b))}
fnor<-as.data.frame(lapply(f2[1:12], n2))
fnor
boxplot(fnor,las=2)
summary(fnor)

#Finding Outliers----

IQR_Rented_Bike_Count = 0.29956-0.05371
up_freq_Rented_Bike_Count = 0.29956+1.5*IQR_Rented_Bike_Count
low_freq_Rented_Bike_Count = 0.29956-1.5*IQR_Rented_Bike_Count

IQR_Temperature = 0.7045-0.3724
up_freq_Temperature = 0.7045+1.5*IQR_Temperature
low_freq_Temperature = 0.7045-1.5*IQR_Temperature

IQR_Humidity = 0.7551-0.4286
up_freq_Humidity = 0.7551+1.5*IQR_Humidity
low_freq_Humidity = 0.7551-1.5*IQR_Humidity

IQR_Wind_speed = 0.3108-0.1216
up_freq_Wind_speed = 0.3108+1.5*IQR_Wind_speed
low_freq_Wind_speed = 0.3108-1.5*IQR_Wind_speed

IQR_Visibility = 1.000-0.4627
up_freq_Visibility = 1.000+1.5*IQR_Visibility
low_freq_Visibility = 1.000-1.5*IQR_Visibility

IQR_Dew_point_temperature = 0.7855+0.4481
up_freq_Dew_point_temperature = 0.7855+1.5*IQR_Dew_point_temperature
low_freq_Dew_point_temperature = 0.7855-1.5*IQR_Dew_point_temperature

IQR_Solar_Radiation = 0.264205-0.0000
up_freq_Solar_Radiation = 0.264205+1.5*IQR_Solar_Radiation
low_freq_Solar_Radiation = 0.264205-1.5*IQR_Solar_Radiation

IQR_Rainfall = 0.000-0.000
up_freq_Rainfall = 0.000+1.5*IQR_Rainfall
low_freq_Rainfall = 0.000-1.5*IQR_Rainfall

IQR_Snowfall = 0.000-0.000
up_freq_Snowfall = 0.000+1.5*IQR_Snowfall
low_freq_Snowfall = 0.000-1.5*IQR_Snowfall



#dataset after removal of outliers----
df = subset(fnor,Rented.Bike.Count<=up_freq_Rented_Bike_Count & Rented.Bike.Count>=low_freq_Rented_Bike_Count 
                  & Temperature<=up_freq_Temperature & Temperature>=low_freq_Temperature 
                  & Humidity<=up_freq_Humidity & Humidity>=low_freq_Humidity 
                  & Wind.speed<=up_freq_Wind_speed & Wind.speed>=low_freq_Wind_speed 
                  & Visibility<=up_freq_Visibility & Visibility>=low_freq_Visibility 
                  & Dew.point.temperature<=up_freq_Dew_point_temperature & Dew.point.temperature>=low_freq_Dew_point_temperature 
                  & Solar.Radiation<=up_freq_Solar_Radiation & Solar.Radiation>=low_freq_Solar_Radiation 
                  & Rainfall<=up_freq_Rainfall & Rainfall>=up_freq_Rainfall 
                  & Snowfall<=up_freq_Snowfall & Snowfall<=up_freq_Snowfall
)

boxplot(df,las=2)
dim(df)


#Partition Data
set.seed(1502)
sample_data <- sample.split(df, SplitRatio = 0.8)
train_data <- subset(df, sample_data == TRUE)
test_data <- subset(df, sample_data == FALSE)


#mlr
ml1.mlr<-lm(Rented.Bike.Count ~ Hour + Temperature + Humidity+ Wind.speed + Visibility+ Dew.point.temperature + Solar.Radiation +Rainfall+ Snowfall+ Seasons+ Holiday ,data=train_data)
s1<-summary(ml1.mlr)$coef
print(s1)
predictions1 <- ml1.mlr %>% predict(test_data)
rmse.1 <- RMSE(predictions1, test_data$Rented.Bike.Count)
r2.1 <- R2(predictions1, test_data$Rented.Bike.Count)
mae.1 <- MAE(predictions1, test_data$Rented.Bike.Count)
cat("Metric count for MLR is :\nRMSE = ",rmse.1)
cat("\nR Square = ",r2.1)
cat("\nMAE = ",mae.1)


#pred1<-predict(l1,newdata=test_data)
#pred1

#Random Forest----
set.seed(1234)
ml2.rf <- randomForest(Rented.Bike.Count ~ ., data = train_data, ntree = 1000, importance = TRUE, type = "regression")
ml2.rf
s2<-summary(ml2.rf)
print(s2)
plot(ml2.rf)
predictions2 <- ml2.rf %>% predict(test_data)
rmse.2 <- RMSE(predictions2, test_data$Rented.Bike.Count)
cat("Metric count for RF is :\nRMSE = ",rmse.2)
r2.2 <- R2(predictions2, test_data$Rented.Bike.Count)
cat("\nR Square = ",r2.2)
mae.2 <- MAE(predictions2, test_data$Rented.Bike.Count)
cat("\nMAE = ",mae.2)

