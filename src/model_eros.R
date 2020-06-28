library(tidyverse)
library(lubridate)

# read the data
data <- read.csv("data/covid_19_india.csv")
# dmy and ymd is just for bringing date columns in to same format
data$Date <- dmy(data$Date)
testing_details <- read.csv("data/testing_filtered_filled.csv")
testing_details$Date <- ymd(testing_details$Date)

#merging testing details and covid_india
data_merged <- merge(data, testing_details,by = c("Date","State"))

states <- c("Gujarat","Maharashtra","Madhya Pradesh",
            "Chhattisgarh","Jharkhand","Odisha",
            "West Bengal")

prediction <- function(test_data, model){
  pred <- c()
  for(i in 2:8){
    # reinsert the predicted value into the model to predict the next one
    predicted_value <- predict(model, newdata = test_data[i-1, ], type = "response")
    pred <- c(pred, predicted_value)
    if (i != 8) {
      test$yesterday_confirmed[i] <- predicted_value  
    }
  }
  return(pred)
}

add_daily_and_yesterd_cols <- function(data_f){
  liste <- c(0)
  liste <- c(liste, data_f$Confirmed[c(1:(length(data_f$Confirmed)-1))])
  data_f$yesterday_confirmed <- liste
  data_f$num_day <- seq(1:nrow(data_f))
  data_f$daily <- data_f$Confirmed - data_f$yesterday_confirmed
  data_f$daily[data_f$daily < 0] <- 0
  return(data_f)
}


data_frames <- list()
models <- list()
models_sqrd <- list()
i <- 1
for (state in states) {
  filtered <- 
    data_merged %>% 
    filter(State == state) %>% 
    add_daily_and_yesterd_cols

  data_frames[[i]] <- filtered
 
  train_ind <- nrow(filtered) - 7 
  train <- filtered[seq(1:train_ind), ]
  test <- filtered[-seq(1:train_ind), ]
  
  model <- lm(Confirmed ~ yesterday_confirmed+num_day+TotalSamples,
                   train
  )
  
  model_sqrd <- lm(Confirmed ~ yesterday_confirmed+num_day+TotalSamples+I(num_day^2),
               train
               )
  
  
  models[[i]] <- model
  models_sqrd[[i]] <-  model_sqrd
  
  png(paste0("plots/model_diagnostics/normal/", state, "_diagnostics.png"))
  par(mfrow=c(2,2))
  plot(model)
  dev.off
  
  png(paste0("plots/model_diagnostics/normal/", state, "_diagnostics_sqrd.png"))
  par(mfrow=c(2,2))
  plot(model_sqrd)
  dev.off()
  
  
  predict <- prediction(test, model)
  predict_sqrd <- prediction(test, model_sqrd)
  
  
  obs_pred <- append(train$Confirmed, predict)
  ggplot(data=filtered, aes(x=num_day, y=Confirmed)) +
    geom_line(col = 'blue') +
    geom_line(aes(y = obs_pred), col = 'red') +
    labs(x = "days", y = "Confirmed", title = state, legend = "Blue = real data; Red: prediction")
  ggsave(paste0("plots/pred/normal/", state, "_predictions.png"))
  
  obs_pred_sqrd <- append(train$Confirmed, predict_sqrd)
  ggplot(data=filtered, aes(x=num_day, y=Confirmed)) +
    geom_line(col = 'blue') +
    geom_line(aes(y = obs_pred_sqrd), col = 'red') +
    labs(x = "days", y = "Confirmed", title = state, legend = "Blue = real data; Red: prediction")
  ggsave(paste0("plots/pred/normal/", state, "_predictions_sqrd.png"))
        
  i <- i + 1
}

for (index in 1:7) {
  summary(models[[index]], correlation = TRUE)
  train_ind <- nrow(data_frames[[index]]) - 7 
  train <- data_frames[[index]][seq(1:train_ind), ]
  test <- data_frames[[index]][-seq(1:train_ind), ]
  
  model <- lm(Confirmed ~ yesterday_confirmed+num_day+TotalSamples,
              train
  )
  
  model2 <- lm(Confirmed ~ num_day+TotalSamples,
               train
  )
  
  print(anova(model, model2))
}

for (index in 1:7) {
  train_ind <- nrow(data_frames[[index]]) - 7 
  train <- data_frames[[index]][seq(1:train_ind), ]
  test <- data_frames[[index]][-seq(1:train_ind), ]
  
  model <- lm(Confirmed ~ yesterday_confirmed+num_day+TotalSamples,
              train
  )
  
  model2 <- lm(Confirmed ~ yesterday_confirmed+num_day+TotalSamples+I(num_day^2),
               train
  )
  
  predict <- prediction(test, model)
  predict_sqrd <- prediction(test, model2)
  print("")
  (data_frames[[index]] %>%
      select(State) %>%
      unique)[1,] %>% 
      paste("STATE:", .) %>% 
      print
  print("MSE and MAD of predictor")
  print(mean((test$Confirmed - predict)^2/(nrow(test))))
  print(mad((test$Confirmed - predict)/(nrow(test))))
  
  print("MSE and MAD of sqrd predictor")
  print(mean((test$Confirmed - predict_sqrd)^2/(nrow(test))))
  print(mad((test$Confirmed - predict_sqrd)/(nrow(test))))
  
  print("Summary of model")
  summary(model, corr = TRUE) %>%  print
  print("Summary of model with squared term")
  summary(model2, corr = TRUE) %>%  print
  print("Analysis of variance (F-test)")
  print(anova(model, model2))
}


state <- "West Bengal"
indx <- 7
filtered <- 
  data_merged %>% 
  filter(State == state) %>% 
  add_daily_and_yesterd_cols

train_ind <- nrow(data_frames[[indx]]) - 7 
train <- data_frames[[indx]][seq(1:train_ind), ]
test <- data_frames[[indx]][-seq(1:train_ind), ]

model <- lm(Confirmed ~ 0+yesterday_confirmed+TotalSamples,
            train
)
summary(model)
predict <- prediction(test, model)
obs_pred <- c()
obs_pred <- append(train$Confirmed, predict)
ggplot(data=filtered, aes(x=num_day, y=Confirmed)) +
  geom_line(col = 'blue') +
  geom_line(aes(y = obs_pred), col = 'red') +
  labs(x = "days", y = "Confirmed", title = state, legend = "Blue = real data; Red: prediction")
ggsave(paste0("plots/pred/normal/", state, "_predictions_final.png"))