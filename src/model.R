library(tidyverse)
library(lubridate)

# read the data
data <- read.csv("data/covid_19_india.csv")
# dmy and ymd is just for bringing date columns in to same format
data$Date <- dmy(data$Date)
testing_details <- read.csv("data/testing_filtered_filled.csv")
testing_details$Date <- ymd(testing_details$Date)
str(data)
str(testing_details)

#merging testing details and covid_india
data_merged <- merge(data, testing_details,by = c("Date","State"))

head(data)

# change the column name X to num_day

colnames(data_merged)

str(data_merged)

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

maharash <- filter(data_merged,State == "Maharashtra")

# Adding days and yesterday's confirmed case to dataframe
liste <- c(0)
liste <- c(liste,maharash$Confirmed[c(1:(length(maharash$Confirmed)-1))])
maharash$yesterday_confirmed <- liste
maharash$num_day <- seq(1:nrow(maharash))

plot(maharash$Confirmed)

# train-test split, last 1 week as a test set

#smp_size <- floor(0.80 * nrow(maharash)) # 80% of the sample size
train_ind <- nrow(maharash) - 7 
train <- maharash[seq(1:train_ind), ]
test <- maharash[-seq(1:train_ind), ]

# first model

model <- glm(Confirmed ~ yesterday_confirmed+num_day+TotalSamples,train,family = gaussian())
?glm
summary(model)
# plot(model)
model$coefficients

# here Pietro's idea is used but it is changed a bit,
# predictions are done by taking the yesterday's prediction because we don't know normally 
# yesterday's confirmed cases so best we can do is taking our prediction it is done by modifying
# "yesterday_confirmed" column also predictions are stored in pred vector

pred <- c()
for(i in 2:8){
  # reinsert the predicted value into the model to predict the next one
  predicted_value <- predict(model, newdata = test[i-1, ])
  pred <- c(pred,predicted_value)
  if (i != 8) {
    test$yesterday_confirmed[i] <- predicted_value  
  }
}

# comparing predictions and real values
pred
test$Confirmed
#test$yesterday_confirmed




# we underestimate the trend maybe good idea to do something to increase it
plot(test$Confirmed)
lines(pred,col="red")

# MSE and MAD check
mean((test$Confirmed - pred)^2/(nrow(test)))
mad((test$Confirmed - pred)/(nrow(test)))

library(ggplot2)

# here observation + predictions are created for plotting purpose otherwise I got error since
# two dataframe don't have same length
obs_pred <- append(train$Confirmed,pred)
ggplot(data=maharash,aes(x=num_day,y=Confirmed)) + geom_line(col='blue') + geom_line(aes(y=obs_pred),col='red')


# Try same model for West bengal

west_bengal <- filter(data_merged,State == "West Bengal")

# Adding days and yesterday's confirmed case to dataframe
liste <- c(0)
liste <- c(liste,west_bengal$Confirmed[c(1:(length(west_bengal$Confirmed)-1))])
west_bengal$yesterday_confirmed <- liste
west_bengal$num_day <- seq(1:nrow(west_bengal))

plot(west_bengal$Confirmed)

# train-test split, last 1 week as a test set

#smp_size <- floor(0.80 * nrow(maharash)) # 80% of the sample size
train_ind <- nrow(west_bengal) - 7 
train <- west_bengal[seq(1:train_ind), ]
test <- west_bengal[-seq(1:train_ind), ]

# first model
model <- glm(Confirmed ~ yesterday_confirmed+num_day+TotalSamples,train,family = gaussian())
summary(model)
# plot(model)

# here Pietro's idea is used but it is changed a bit,
# predictions are done by taking the yesterday's prediction because we don't know normally 
# yesterday's confirmed cases so best we can do is taking our prediction it is done by modifying
# "yesterday_confirmed" column also predictions are stored in pred vector

pred <- c()
for(i in 2:8){
  # reinsert the predicted value into the model to predict the next one
  predicted_value <- predict(model, newdata = test[i-1, ])
  pred <- c(pred,predicted_value)
  if (i != 8) {
    test$yesterday_confirmed[i] <- predicted_value  
  }
}

# comparing predictions and real values
pred
test$Confirmed
#test$yesterday_confirmed

# we underestimate the trend maybe good idea to do something to increase it
plot(test$Confirmed)
lines(pred,col="red")

# MSE and MAD check
mean((test$Confirmed - pred)^2/(nrow(test)))
mad((test$Confirmed - pred)/(nrow(test)))

library(ggplot2)

# here observation + predictions are created for plotting purpose otherwise I got error since
# two dataframe don't have same length
obs_pred <- append(train$Confirmed,pred)
ggplot(data=west_bengal,aes(x=num_day,y=Confirmed)) + geom_line(col='blue') + geom_line(aes(y=obs_pred),col='red')













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

