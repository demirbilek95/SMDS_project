library(tidyverse) # data manipulatinon
library(lubridate) # for date formatting
library(ggplot2) # for plotting
library(dplyr)
library(tscount)

# read the data
covid19 <- read.csv("data/covid_19_india_filtered.csv") %>% mutate(Date=as.Date(Date))
testing <- read.csv("data/testing_filtered_filled.csv") %>% mutate(Date=as.Date(Date))

#merging testing details and covid_india by Date and State
nadefault <- function(x,y) ifelse(is.na(x),y,x)

data_merged <- full_join(covid19,testing, by= c("State", "Date")) %>%
  arrange(State, Date) %>% 
  group_by(State) %>%
  transmute(Date,
            Confirmed, dConf = Confirmed - lag(Confirmed, default=0),
            Deaths, Cured,
            
            TotalSamples = nadefault(TotalSamples,nadefault(lag(TotalSamples),0)),
            dSamp = TotalSamples - lag(TotalSamples, default = 0),
            Positive,Negative) %>% 
  mutate(dConf = pmax(dConf,0), dSamp=pmax(dSamp,0)) %>% ungroup()

## We shouldn't consider basic glm or lm approach because first assumption of glm (y must distributed independently)
## is not appropriate for our case. Hence we considere tscount (time series count), it is a package provides 
## likelihood-based estimtion methods for analysis and modeling of count time series following generalized 
## linear models. We considered poisson model since out target variable is discrete and [0,inf) advantages of 
## glm-based models
# (a) They can describe covariate effects and negative correlations in a straightforward way.
# (b) There is a rich toolkit available for this class of models.


# HERE TSCOUNT STARTS

summary.scores <- function(model){
  sumry = summary(model)
  return(c( LogLkh=as.numeric(sumry$logLik),
            AIC= sumry$AIC,
            BIC= sumry$BIC,
            QIC= sumry$QIC,
            MeanRes = mean(model$residuals),
            VarRes = var(model$residuals)))
}

scores = c()
models = list()

for(s in levels(data_merged$State)){
  
  #filter data
  df <- data_merged %>% filter(State==s)
  
  #create models
  model1 <- tsglm(df$Confirmed,
                  model = list(past_obs=1, past_mean = 14),
                  xreg = (df %>% select(TotalSamples,dConf) ),
                  init.method="firstobs",
                  distr = "poisson",link="log")
  
  model2 <- tsglm(df$Confirmed,
                  model = list(past_obs=1, past_mean = 7),
                  init.method="firstobs",
                  xreg = (df %>% select(TotalSamples,dConf) ),
                  distr = "poisson",link="log")
  
  #store models
  this.models=list(model1,
                   model2)
  
  models[[s]] = this.models
  
  #store scores
  score = c()
  score = rbind(summary.scores(model1),
                summary.scores(model2))
  rownames(score)<-c(paste0(s,".model1"),
                     paste0(s,".model2"))
  
  scores = rbind(scores,score)
}

#see scores
scores


options(scipen = 100)
summary(model2)
analyze_residuals(model1)
analyze_residuals(model2)

par(mfrow=c(1,1))
plot(df$Confirmed, main = "Target vs Fitted")
lines(model2$fitted.values,col="red")

par(mfrow=c(2,2))

plot(model2,ask=FALSE)
plot(model1,ask=FALSE)
