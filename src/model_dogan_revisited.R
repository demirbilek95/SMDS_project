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

#function that extracts scores
summary.scores <- function(model){
  sumry = summary(model)
  return(c( LogLkh=as.numeric(sumry$logLik),
            AIC= sumry$AIC,
            BIC= sumry$BIC,
            QIC= sumry$QIC,
            MeanRes = mean(model$residuals),
            VarRes = var(model$residuals)))
}

#train models, gather scores for each state
scores = c()
models = list()
for(s in levels(data_merged$State)){
  
  #filter data, remove last week
  df <- data_merged %>% filter(State==s)
  df <- df[1:(nrow(df)-7),]
  
  #create models
  model1 <- tsglm(df$Confirmed,
                  model = list(past_obs=1, past_mean = 14),
                  xreg = (df %>% select(TotalSamples,dConf) ),
                  init.method="firstobs",
                  distr = "nbinom",link="log")
  
  model2 <- tsglm(df$Confirmed,
                  model = list(past_obs=1, past_mean = 14),
                  xreg = (df %>% select(TotalSamples,dConf) ),
                  init.method="firstobs",
                  distr = "poisson",link="log")
  
  model3 <- tsglm(df$Confirmed,
                  model = list(past_obs=1, past_mean = 14),
                  init.method="firstobs",
                  xreg = (df %>% select(TotalSamples) ),
                  distr = "nbinom",link="log")
  
  model4 <- tsglm(df$Confirmed,
                  model = list(past_obs=1, past_mean = 7),
                  init.method="firstobs",
                  xreg = (df %>% select(TotalSamples) ),
                  distr = "poisson",link="log")
  
  #store models
  this.models=list(model1,
                   model2,
                   model3,
                   model4)
  
  models[[s]] = this.models
  
  #store scores
  score = c()
  score = rbind(summary.scores(model1),
                summary.scores(model2),
                summary.scores(model3),
                summary.scores(model4))
  rownames(score)<-c(paste0(s,".model1"),
                     paste0(s,".model2"),
                     paste0(s,".model3"),
                     paste0(s,".model4"))
  
  scores = rbind(scores,score)
}

#see scores
scores

#chose best models according to scores
#Model 2 appears to be the best in terms of scores for all states!

#TODO:get residuals/predictions plot 

preds = data.frame()
for (s in levels(data_merged$State)){
  df<-(data_merged %>% filter(State==s) %>% select(Date,Confirmed,TotalSamples,dConf))
  
  a = data.frame(State = s,
                 Date = df$Date[1:(nrow(df)-7)],
                 Confirmed = df$Confirmed[1:(nrow(df)-7)],
                 Training = "Training",
                 Predictions = models[[s]][[2]]$fitted.values,
                 Upper=NA,Lower=NA)
  
  this.pred.obj =  predict(models[[s]][[2]],
                           n.ahead=7,
                           newxreg=(df %>% select(TotalSamples,dConf))[(nrow(df)-6):nrow(df),])
  
  b = data.frame(State = s,
                 Date = df$Date[(nrow(df)-6):nrow(df)],
                 Confirmed = df$Confirmed[(nrow(df)-6):nrow(df)],
                 Training = "Test",
                 Predictions = this.pred.obj$pred,
                 Upper=this.pred.obj$interval[,2],
                 Lower=this.pred.obj$interval[,1])
  
  preds = preds %>% rbind(a) %>% rbind(b)
}

#predictions
preds %>% ggplot()+
  geom_point(aes(Date,Confirmed,color=Training))+
  geom_line(aes(Date,Predictions,color="Fitted"), size=1)+
  geom_ribbon(aes(Date,ymin=Lower,ymax=Upper),color="#3366ff",fill="green",alpha=.5) +
  scale_color_manual(values=c("#ff0000","#000000"))+
  labs(x="",y="Confirmed + Fit")+
  scale_color_manual(name="",values=c("Fitted"="#3366ff","Training"="black","Test"="red"))+
  theme(legend.position = "bottom")+
  facet_wrap(vars(State),scales="free_y")
ggsave("plots/Confirmed_pred_facets.png", width = 150, height=120,units="mm" )

#std residuals
preds %>% group_by(State) %>% mutate(StdRes = (Confirmed-Predictions)/sd(Confirmed-Predictions)) %>% 
  ggplot()+
  geom_hline(yintercept = 0, linetype="dashed",color="grey")+
  geom_point(aes(Predictions,StdRes,color=Training))+
  labs(x="",y="Standardized Residuals")+
  scale_color_manual(name="",values=c("Training"="black","Test"="red"))+
  theme(legend.position = "bottom")+
  facet_wrap(vars(State),scales="free_y")
ggsave("plots/Confirmed_stdres_facets.png", width = 150, height=120,units="mm" )

#TODO: diagnostic plots

summary(model1)

summary(models[[s]][[1]])

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
