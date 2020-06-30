library(tidyverse) # data manipulatinon
library(DAAG)
library(lubridate)
library(AER)

# read the data
data <- read.csv("data/ind_data_merged.csv")  # do not forget to change path
# dmy and ymd is just for bringing date columns in to same format
data$Date <- ymd(data$Date)
testing_details <- read.csv("data/testing_filtered_filled.csv")
testing_details$Date <- ymd(testing_details$Date)


#merging testing details and covid_india by Date and State
data_merged <- merge(data, testing_details,by = c("Date","State"))


# filter state and add covariates will be used

add_daily <- function(state) {
    
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

df <- add_daily("Odisha")
  
# possible covariates
df <- df %>% select(daily,yesterday_daily,yesterday_confirmed,num_day, TotalSamples)

# checking dist of covariates and target variables

par(mfrow=c(3,2))
histout=apply(df,2,hist)

# checking covariates relation wtih target variable

plot(df)

par(mfrow=c(1,2))
# scatterplot matrix with data
splom(~df[,c("daily","yesterday_daily","num_day","yesterday_confirmed","TotalSamples")],
      varnames = c("daily","yesterday_daily","num_day","yesterday_confirmed","TotalSamples"))

# train-test split, last 1 week as a test set can be 10 days or 14 days
train_ind <- nrow(df) - 7 
train <- df[seq(1:train_ind), ]
test <- df[-seq(1:train_ind), ]

# so according to this relations I think it is better to have num_day^2 and sqrt(yesterday_confirmed) but in model this 
# ideas will be checked

model0 <- glm(daily ~ yesterday_daily+TotalSamples+yesterday_confirmed+num_day,
              family = poisson(),train)

analyze_model <- function(model){
  
  print(summary(model, corr = TRUE))
  
  # in lab files those plot were used to make better comments (for slides) lab files will be used
  par(mfrow=c(1,2))
  plot(model, which=c(1,3))
  
  # again from lab files  
  # fitted values from model
  pred.pois <- model$fitted.values
  # standardize residuals
  res.st <- (train$daily-pred.pois)/sqrt(pred.pois)
  
  plot(pred.pois,res.st)
  abline(h=0,lty=3,col="gray75")
  
  print(model$converged)
}

# here all the coeff seems significant, though there are some problems with residuals, maybe correlation among covariates
# need to study theory more to make better comments
analyze_model(model0)

# checking dispersion of this model, and it says we have dispersion so let's use quasipoisson
dispersiontest(model0,alternative = "greater")


model1 <- glm(daily ~ yesterday_daily+TotalSamples+yesterday_confirmed+num_day,
              family = quasipoisson(),
              train)

# here some of the coeff are not significant so previously mentioned transforms will be done and there are some problems 
# with residuals again need to study theory
analyze_model(model1)

model2 <- glm(daily ~ yesterday_daily+TotalSamples+yesterday_confirmed+sqrt(yesterday_confirmed)+num_day+I(num_day^2),
              family = quasipoisson(),
              train)

# for now all coeff seems significant maybe additional transformations can be done with reasoning, but still there are some 
# problems in this model too, for now let's use this
analyze_model(model2)


# I hope it is not overfitting :(, but will try for test set as well and need to check train MSE, MAD and compare with 
# test MSE MAD

plot(train$daily)
lines(model2$fitted.values, col = "red")

# this is the problematic part that we need to find solution so for yesterday's daily covariates I used our predictions
# but still we don't know TotalSamples

make_prediction <- function(model) {
  
  pred <- c()
  for(i in 2:8){
    # reinsert the predicted value into the model to predict the next one
    predicted_value <- predict(model, newdata = test[i-1, ],type="response")
    #print(predicted_value)
    pred <- c(pred,predicted_value)
    if (i != 8) {
      test$yesterday_daily[i] <- predicted_value
      test$yesterday_confirmed[i] <- predicted_value + test$yesterday_confirmed[i-1]
    }
  }
  cat("predictions:",pred)
  cat("\nreal values",test$daily)
  return(pred)
}

pred <- make_prediction(model2)

# just plotting test set and predictions together

plot(df$daily)
lines(c(model2$fitted.values,pred),col="red")

obs_pred <- append(model2$fitted.values,pred)
ggplot(data=df,aes(x=num_day,y=daily)) +
  geom_line(col='blue') +
  geom_line(aes(y=obs_pred),col='red')

mse_mad <- function(df,pred) {
  # MSE and MAD check
  mse <- sum((df$daily - pred)^2)/(nrow(df))
  mad <- sum(abs(df$daily - pred))/(nrow(df))
  cat("MSE:", mse)
  cat("\nMAD", mad)
}

# maybe overfit donno :(
mse_mad(train, model2$fitted.values)
mse_mad(test, pred)

# LIMITATIONS

# I am not sure if VIF should be checked, I need to study about it
# Assumptions of models will be checked -> actions will be taken according to this like transformation etc
# I am not sure if model like this can be generalized for all states need to check.options
# I hope there is no overfitting but model worked more or less okay for test data, just prediction problem that we have about
# covariates that we don't have need to be solved

states <- c("Gujarat","Maharashtra","Madhya Pradesh",
            "Chhattisgarh","Jharkhand","Odisha",
            "West Bengal")

#this part is for multilevel model data
deneme <- data.frame(matrix(ncol = length(data_merged), nrow = 0))
colnames(deneme) <- colnames(data_merged)

for (i in states) {
  #print(i)
  df <- add_daily(i)
  #print(df)
  png(paste("plots/daily_",i,".png",sep=''))
  plot(df$daily,type="l")
  dev.off()
  deneme <- rbind(deneme,df)
}


# Try to try very same model for all states and check the results
# study bit theory about lm and glm
# maybe plot
# presentation

# Following the Egidi's suggestion multilevel model will be tried by following this link 
# https://rpubs.com/rslbliss/r_mlm_ws#:~:text=To%20run%20a%20multilevel%20linear,we%20have%20used%20thus%20far.&text=Note%20that%20R%20uses%20restricted%20maximum%20likelihood%20to%20fit%20the%20model.
# This part is missing

library(lme4)

?glmer

model0 <- glmer(daily ~ yesterday_daily+TotalSamples+yesterday_confirmed+num_day + (1 | daily),family = poisson(),deneme)

summary(model0)


m1 <- glm(daily ~ num_day+yesterday_confirmed+yesterday_daily+TotalSamples,
          family = quasipoisson(),
          train)
m2 <- glm(daily ~ num_day+yesterday_confirmed+TotalSamples,
          family = quasipoisson(),
          train)
anova(m2, m1, test = "F")

m1 <- glm(daily ~ num_day+yesterday_confirmed,
          family = quasipoisson(),
          train)
anova(m1, m2, test = "F")

m2 <- glm(daily ~ yesterday_confirmed,
          family = quasipoisson(),
          train)
anova(m2, m1, test ="F")
