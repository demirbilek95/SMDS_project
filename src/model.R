library(dplyr)

# Naive Model
data <- read.csv("data/ind_data_merged.csv")
head(data)

colnames(data)

data$X = NULL

str(data)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data$Population <- normalize(data$Population)
data$Rural.population <- normalize(data$Rural.population)
data$TotalPublicHealthFacilities_HMIS <- normalize(data$TotalPublicHealthFacilities_HMIS)
data$Urban.population <- normalize(data$Urban.population)
data$TotalPublicHealthFacilities_HMIS <- normalize(data$TotalPublicHealthFacilities_HMIS)
data$num_lab <- normalize(data$num_lab)



maharash <- filter(data,State == "Maharashtra")
maharash
colnames(maharash)

maharash <- maharash %>% select("State","Confirmed","Population","Rural.population","Urban.population","num_lab","TotalPublicHealthFacilities_HMIS")
maharash$State = NULL

liste <- c(0)
liste <- c(liste,maharash$Confirmed[c(1:length(maharash$Confirmed)-1)])
maharash$yesterday_confirmed <- liste

str(maharash)
model <- glm(Confirmed ~ .,maharash, family = poisson)
summary(model)
plot(model)

plot(maharash$Confirmed)


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

