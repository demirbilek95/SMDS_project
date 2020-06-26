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

head(data)

# change the column name X to num_day

colnames(data_merged)

str(data_merged)

states <- c("Gujarat","Maharashtra","Madhya Pradesh",
            "Chhattisgarh","Jharkhand","Odisha",
            "West Bengal")

prediction <- function(test_data, model){
  pred <- c()
  for(i in 2:8){
    # reinsert the predicted value into the model to predict the next one
    predicted_value <- predict(model, newdata = test_data[i-1, ])
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
  
  
  model <- glm(Confirmed ~ yesterday_confirmed+num_day+TotalSamples, train,
               family = gaussian())
  models[[i]] <- model
  
  png(paste0("plots/model_diagnostics/", state, "_diagnostics.png"))
  par(mfrow=c(2,2))
  plot(model)
  dev.off()
  
  predict <- prediction(test, model)
  
  obs_pred <- append(train$Confirmed, predict)
  ggplot(data=filtered, aes(x=num_day, y=Confirmed)) +
    geom_line(col = 'blue') +
    geom_line(aes(y = obs_pred), col = 'red') +
    labs(x = "days", y = "Confirmed", title = state, legend = "Blue = real data; Red: prediction")
  ggsave(paste0("plots/pred/", state, "_predictions.png"))
  
        
  i <- i + 1
}



