# POISSON MODEL (SIMPLE CASE)

library(tidyr)
library(MASS) 
library(dplyr)

# load datasets
covid_19_india <- read.csv('~/Documents/Statistical_method_data_science/smds_exercises/SMDS_project/data/re_arranged_covid_19_india_daily_infected_filtered.csv', row.names = 1)
testing <- read.csv('~/Documents/Statistical_method_data_science/smds_exercises/SMDS_project/data/testing_filtered.csv', row.names = 1)

prediction_period <- 7 # one week of prediction
end <- length(covid_19_india$Maharashtra) # number of days

# define train and test sets
train_ind <- end-prediction_period

# responce partition
train_confirmed <- covid_19_india$Maharashtra[0:train_ind]
test_confirmed <- covid_19_india$Maharashtra[(train_ind+1):end]

# covariates partition
train_negative <- testing$Negative[0:train_ind]
test_negative <- testing$Negative[(train_ind+1):end]

train_positive <- testing$Positive[0:train_ind]
test_positive <- testing$Positive[(train_ind+1):end]

train_tot_sample <- testing$TotalSamples[0:train_ind]
test_tot_sample <- testing$TotalSamples[(train_ind+1):end]

# prepare train dataset
train_data = data.frame(date = 1:train_ind, confirmed = train_confirmed, 
                  yest_confirmed = c(0, covid_19_india$Maharashtra[0:(train_ind-1)]),
                  tot_sample = train_tot_sample, negative = train_negative, positive = train_positive)

# make the poisson model
poisson_model <- glm(data = train_data, confirmed ~ 
                       yest_confirmed + tot_sample + positive, 
                     family = poisson(link = "log"))

summary(poisson_model)

# prepare test dataset
test_data = data.frame(date = (train_ind+1):end, 
                       yest_confirmed = c(covid_19_india$Maharashtra[train_ind], rep(0, prediction_period-1)),
                       tot_sample = test_tot_sample, negative = test_negative, positive = test_positive)

# make prediction
for(i in 2:prediction_period){
  # reinsert the predicted value into the model to predict the next one
  predicted_value <- predict(poisson_model, newdata = test_data[i-1, ])
  test_data$yest_confirmed[i] <- predicted_value
}

# visual comparison between real confirmed and predictated
test_data$yest_confirmed
test_confirmed

# make plot
pred_train <- predict(poisson_model, newdata = train_data)
plot(train_confirmed)
lines(pred_train,col="red")

pred_test <- predict(poisson_model, newdata = test_data)
options("scipen"=100, "digits"=4)
test_confirmed
pred_test
plot(test_confirmed)
lines(pred_test,col="red")

mean((test_confirmed - pred_test)^2/(nrow(test_data)))
mad((test_confirmed - pred_test)/(nrow(test_data)))

data <- bind_rows(train_data, test_data)
select(data, date, yest_confirmed, tot_sample, positive)

pred <- predict(poisson_model, newdata = select(data, date, yest_confirmed, tot_sample, positive))

library(ggplot2)

ggplot(data = covid_19_india, aes(x = date, y = Maharashtra)) + 
  geom_line(col = 'blue') + 
  geom_line(aes(y = pred), col = 'red')

# compute informative plot
library(jtools)
library(ggstance)
library(broom)

plot_summs(poisson_model, scale = TRUE, exp = TRUE)
