library(tidyverse) # data manipulatinon
library(lubridate) # for date formatting
library(ggplot2) # for plotting
library(ggpubr) # to make plots side-byside

# Preparing Data

# read the data
data <- read.csv("data/covid_19_india.csv")  # do not forget to change path
# dmy and ymd is just for bringing date columns in to same format
data$Date <- dmy(data$Date)
testing_details <- read.csv("data/testing_filtered_filled.csv")
testing_details$Date <- ymd(testing_details$Date)
str(data)
str(testing_details)

#merging testing details and covid_india by Date and State
data_merged <- merge(data, testing_details,by = c("Date","State"))

head(data)

colnames(data_merged)

str(data_merged)

# This part can be useful later to bring covariates to same scale
# normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }
# 
# data$Population <- normalize(data$Population)
# data$Rural.population <- normalize(data$Rural.population)
# data$TotalPublicHealthFacilities_HMIS <- normalize(data$TotalPublicHealthFacilities_HMIS)
# data$Urban.population <- normalize(data$Urban.population)
# data$num_lab <- normalize(data$num_lab)



# It is the function that prepare the data 
# for model and get some results (plots, tests, predictions etc) from trained model
# for state you can use the unique(data_merged$State)
# for family and link please chech the run ?glm

model <- function(state, glm_model_family, link_func) {
  # filter the given state from general dataframe
  df <- filter(data_merged,State == state)
  
  # Adding days and yesterday's confirmed case to dataframe
  liste <- c(0)
  liste <- c(liste,df$Confirmed[c(1:(length(df$Confirmed)-1))])
  df$yesterday_confirmed <- liste
  df$num_day <- seq(1:nrow(df))
  df$daily <- df$Confirmed - df$yesterday_confirmed
  df$daily[df$daily < 0] <- 0
  
  # plot what we may try to forecast
  p1 <- ggplot(df, aes(x = 1:length(daily), y = daily)) + geom_point() +
        labs(x = "days", y = "cases", subtitle = "daily")
  
  p2 <- ggplot(df, aes(x = 1:length(Confirmed), y = Confirmed)) + geom_point() +
    labs(x = "days", y = "cases", subtitle = "cumulative")
  
  parr <- ggarrange(p1, p2, ncol = 2, nrow = 1)
  p <- annotate_figure(parr, top = text_grob(paste("Confirmed cases in", state)))
  
  print(p)
  
  # train-test split, last 1 week as a test set can be 10 days or 14 days
  train_ind <- nrow(df) - 7 
  train <- df[seq(1:train_ind), ]
  test <- df[-seq(1:train_ind), ]
  
  # most basic model that we can have, we all need to study theory for this part
  # simply we may play with covariates,formula, family and link func
  model <- glm(Confirmed ~ yesterday_confirmed+num_day+TotalSamples,train,
               family = gaussian())
  
  # to see if coefficients are significant as well as model etc
  summary(model)
  
  # here Pietro's idea is used but it is changed a bit,
  # predictions are done by taking the yesterday's prediction because we don't know normally 
  # yesterday's confirmed cases so best we can do is taking our prediction, it is done by modifying
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
  cat("predictions:",pred)
  cat("\nreal values:",test$Confirmed)
  
  # just plotting test set and predictins together
  p <- ggplot(test, aes(x = 1:length(Confirmed), y = Confirmed)) + geom_point() +
    geom_line(aes(y = pred), color = "red") + 
    labs(x = "counter", y = "cases", title = "visual check on predicted range")
  
  print(p)
  
  # MSE and MAD check
  mean((test$Confirmed - pred)^2/(nrow(test)))
  mad((test$Confirmed - pred)/(nrow(test)))
  
  # here observation + predictions are created for plotting purpose otherwise I got error since
  # two dataframe don't have same length
  # this part just to see our model and observations, can be plotted better for sure, adding legend etc
  # red is model, blue is observations
  
  obs_pred <- append(train$Confirmed,pred)
  
  real_pred_data <- data.frame(days = df$num_day, 
                               real_confirmed = df$Confirmed, 
                               pred_confirmed = obs_pred)
  
  ggplot(data=real_pred_data, aes(x=num_day, y=Confirmed)) + 
    geom_line(col='blue') + 
    geom_line(aes(y=obs_pred), col='red') + 
    labs(title = "Prediction over cumulative cases")
  
}

model("Maharashtra",gaussian,"identity")
model("Jharkhand", gaussian, "identity")
