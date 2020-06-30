library(tidyverse) # data manipulatinon
library(lubridate) # for date formatting
library(ggplot2) # for plotting

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

# filter state and add covariates will be used

filter_add_daily <- function(state) {
  
  df <- filter(data_merged,State == state)
  
  # Adding days and yesterday's confirmed case to dataframe
  liste <- c(0)
  liste <- c(liste,df$Confirmed[c(1:(length(df$Confirmed)-1))])
  df$yesterday_confirmed <- liste
  df$num_day <- seq(1:nrow(df))
  df$daily <- df$Confirmed - df$yesterday_confirmed
  # since our data is noisy, values smaller than 0 are set to 0.
  df$daily[df$daily < 0] <- 0
  # Adding yesterday's daily cases
  liste <- c(0)
  liste <- c(liste,df$daily[c(1:(length(df$daily)-1))])
  df$yesterday_daily <- liste
  return(df)
}




## We shouldn't consider basic glm or lm approach because first assumption of glm (y must distributed independently)
## is not appropriate for our case. Hence we considere tscount (time series count), it is a package provides 
## likelihood-based estimtion methods for analysis and modeling of count time series following generalized 
## linear models. We considered poisson model since out target variable is discrete and [0,inf) advantages of 
## glm-based models
# (a) They can describe covariate effects and negative correlations in a straightforward way.
# (b) There is a rich toolkit available for this class of models.


# HERE TSCOUNT STARTS


library(tscount)

df <- filter_add_daily("Maharashtra")
df <- df %>% select(Confirmed,daily,yesterday_confirmed,num_day, ,daily,yesterday_daily,TotalSamples)

?tsglm

target_cumul <- df$Confirmed
target_daily <- df$daily

regressors_cumul <- cbind(Swabs=df$TotalSamples,daily = df$daily)
regressors_dail <- cbind(Swabs =df$TotalSamples, Cum= df$Confirmed)

model1 <- tsglm(target_cumul,model = list(past_obs=3, past_mean = 7), xreg = regressors_cumul, distr = "poisson",link="log")
model2 <- tsglm(target_cumul,model = list(past_obs=3, past_mean = 7), xreg = regressors_cumul, distr = "nbinom",link="log")
model3 <- tsglm(target_cumul,model = list(past_obs=1, past_mean = 14), xreg = regressors_cumul, distr = "nbinom",link="log")

analyze_residuals <- function(model){
  # good model should show random 0 concentrated, no systematic behaviour
  plot(model$residual,main="Residual Plot")
  cat("Mean of residuals", mean(model$residuals))
  cat("\nVariance of residuals", var(model$residuals))
  
  # normal hist is desired
  hist(model$residuals)
  
  # non systematic acf is desired
  acf(model$residuals)
}

summary(model3)
analyze_residuals(model3)

par(mfrow=c(1,1))
plot(df$Confirmed, main = "Target vs Fitted")
lines(model3$fitted.values,col="red")

par(mfrow=c(2,2))
plot(model3)


?tsglm









# IGNORE THIS PART

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
  par(mfrow=c(1,2))
  plot(df$daily)
  plot(df$Confirmed)
  
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
  cat("\nreal values",test$Confirmed)
  
  plot(model$residual,main="Residuals")
  acf(model$residual,main="Auto Correlation Function")
  
  # just plotting test set and predictins together
  plot(test$Confirmed)
  lines(pred,col="red")
  
  # MSE and MAD check
  cat(mean((test$Confirmed - pred)^2/(nrow(test))))
  cat(mad((test$Confirmed - pred)/(nrow(test))))
  
  # here observation + predictions are created for plotting purpose otherwise I got error since
  # two dataframe don't have same length
  # this part just to see our model and observations, can be plotted better for sure, adding legend etc
  # red is model, blue is observations
  
  obs_pred <- append(train$Confirmed,pred)
  ggplot(data=df,aes(x=num_day,y=Confirmed)) + geom_line(col='blue') + geom_line(aes(y=obs_pred),col='red')
  
}

model("Maharashtra",gaussian,"identity")

model("Jharkhand", gaussian, "identity")