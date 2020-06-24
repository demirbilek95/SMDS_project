library(tidyverse)

data <- read.csv("data/ind_data_merged.csv")
head(data)

colnames(data)

data$X = NULL

str(data)

# normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }
# 
# data$Population <- normalize(data$Population)
# data$Rural.population <- normalize(data$Rural.population)
# data$TotalPublicHealthFacilities_HMIS <- normalize(data$TotalPublicHealthFacilities_HMIS)
# data$Urban.population <- normalize(data$Urban.population)
# data$num_lab <- normalize(data$num_lab)

# First model for Maharash

maharash <- filter(data,State == "Maharashtra")

# Adding days and yesterday's confirmed case to dataframe
liste <- c(0)
liste <- c(liste,maharash$Confirmed[c(1:(length(maharash$Confirmed)-1))])
maharash$yesterday_confirmed <- liste
maharash$num_day <- seq(1:nrow(maharash))

# train-test split, last 1 week as a test set

#smp_size <- floor(0.80 * nrow(maharash)) # 80% of the sample size
train_ind <- nrow(maharash) - 7 
train <- maharash[seq(1:train_ind), ]
train$Date <- as.character(train$Date)
train$yesterday_confirmed <- as.integer(train$yesterday_confirmed)
train <- train[order(train$Date),]
test <- maharash[-seq(1:train_ind), ]

model <- glm(Confirmed ~ 0 + yesterday_confirmed+num_day,train,family = Gamma(link = "identity"),start = c(0,110))
summary(model)
# plot(model)

pred_train <- predict(model, newdata = train)
plot(train$Confirmed)
lines(pred_train,col="red")

pred_test <- predict(model, newdata = test)
options("scipen"=100, "digits"=4)
test$Confirmed
pred_test
plot(test$Confirmed)
lines(pred_test,col="red")

mean((test$Confirmed - pred_test)^2/(nrow(test)))
mad((test$Confirmed - pred_test)/(nrow(test)))

colnames(maharash)

pred <- predict(model, newdata = maharash)

nrow(maharash)

library(ggplot2)

ggplot(data=maharash,aes(x=num_day,y=Confirmed)) + geom_line(col='blue') + geom_line(aes(y=pred),col='red')

unique(data$State)

# Try same model for West bengal

west_bengal <- filter(data,State == "West Bengal")

# Adding days and yesterday's confirmed case to dataframe
liste <- c(0)
liste <- c(liste,west_bengal$Confirmed[c(1:(length(west_bengal$Confirmed)-1))])
west_bengal$yesterday_confirmed <- liste
west_bengal$num_day <- seq(1:nrow(west_bengal))

#smp_size <- floor(0.80 * nrow(maharash)) # 80% of the sample size
train_ind <- nrow(west_bengal) - 7 
train <- west_bengal[seq(1:train_ind), ]
train$Date <- as.character(train$Date)
train <- train[order(train$Date),]
test <- west_bengal[-seq(1:train_ind), ]


model <- glm(Confirmed ~ 0 + yesterday_confirmed+num_day,train,family = Gamma(link = "identity"),start = c(0,110))

?glm
pred_train <- predict(model, newdata = train)
pred_test <- predict(model, newdata = test)

options("scipen"=100, "digits"=4)
test$Confirmed
pred_test

pred <- predict(model, newdata = west_bengal)

ggplot(data=west_bengal,aes(x=num_day,y=Confirmed)) + geom_line(col='blue') + geom_line(aes(y=pred),col='red')
















# Basic Time Series Model (Just in Case)

# Prophet
prop_data <- filter(data,State == "Maharashtra")
prop_data <- prop_data %>% select("Date","Confirmed")

prop_data$Date <- as.character(prop_data$Date)
prop_data$Date <-  as.Date(prop_data$Date)

colnames(prop_data)

prop_data <- rename(prop_data, ds = Date,y = Confirmed)

library(prophet)

m <- prophet(prop_data)
future <- make_future_dataframe(m, periods = 14)
tail(future)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m, forecast)
prophet_plot_components(m, forecast)

