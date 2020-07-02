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
            lag_dConf = lag(dConf,default=0),
            Deaths, Cured,
            lag_Deaths= lag(Deaths,default=0),lag_Cured = lag(Cured,default=0),
            
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
            LogScore = as.numeric(scoring(model)[1]),
            SqrRes = as.numeric(scoring(model)[7]),
            BIC= sumry$BIC,
            AIC= sumry$AIC,
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
                  xreg = (df %>% select(TotalSamples,lag_dConf,lag_Cured,lag_Deaths) ),
                  init.method="firstobs",
                  distr = "poisson",link="log")
  
  model2 <- tsglm(df$Confirmed,
                  model = list(past_obs=1, past_mean = 14),
                  xreg = (df %>% select(TotalSamples,lag_dConf,lag_Cured) ),
                  init.method="firstobs",
                  distr = "poisson",link="log")
  
  model3 <- tsglm(df$Confirmed,
                  model = list(past_obs=1, past_mean = 14),
                  init.method="firstobs",
                  xreg = (df %>% select(TotalSamples,lag_dConf) ),
                  distr = "poisson",link="log")
  
  model4 <- tsglm(df$Confirmed,
                  model = list(past_obs=1, past_mean = 14),
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

#get residuals/predictions plot 
preds = data.frame()
#for (mod_no in 1:4){
for (s in levels(data_merged$State)){
  df<-(data_merged %>% filter(State==s) %>% select(Date,Confirmed,TotalSamples,lag_dConf,lag_Cured,lag_Deaths))
  
  a = data.frame(State = s,
                 Date = df$Date[1:(nrow(df)-7)],
                 Confirmed = df$Confirmed[1:(nrow(df)-7)],
                 Training = "Training",
                 Upper=NA,Lower=NA)
  
  
  #only those mods
  if(s == levels(data_merged$State)[7]){
    mod_no=1
  }else if(s == levels(data_merged$State)[3]){
    mod_no=2
  }else if(s %in% levels(data_merged$State)[c(2,4,6)]){
    mod_no=3
  } else {
    mod_no=4
  }
  
  if(mod_no==1){
    this.pred.obj =  predict(models[[s]][[mod_no]],
                             n.ahead=7,
                             newxreg=(df %>% select(TotalSamples,lag_dConf,lag_Cured, lag_Deaths))[(nrow(df)-6):nrow(df),])
  }else if(mod_no==2){
    this.pred.obj =  predict(models[[s]][[mod_no]],
                             n.ahead=7,
                             newxreg=(df %>% select(TotalSamples,lag_dConf,lag_Cured))[(nrow(df)-6):nrow(df),])
  }else if(mod_no==3){
    this.pred.obj =  predict(models[[s]][[mod_no]],
                             n.ahead=7,
                             newxreg=(df %>% select(TotalSamples,lag_dConf))[(nrow(df)-6):nrow(df),])
  } else {
    this.pred.obj =  predict(models[[s]][[mod_no]],
                             n.ahead=7,
                             newxreg=(df %>% select(TotalSamples))[(nrow(df)-6):nrow(df),])
  }
 
  a = a %>% mutate(Predictions = models[[s]][[mod_no]]$fitted.values)
  b = data.frame(State = s,
                 Date = df$Date[(nrow(df)-6):nrow(df)],
                 Confirmed = df$Confirmed[(nrow(df)-6):nrow(df)],
                 Training = "Test",
                 Predictions = this.pred.obj$pred,
                 Upper=this.pred.obj$interval[,2],
                 Lower=this.pred.obj$interval[,1])
  
  preds = preds %>% rbind(a %>% rbind(b) %>% mutate(Model = mod_no))
}
#}

msq = preds %>% group_by(State, Model) %>% summarize(SqErr = mean((Confirmed-Predictions)^2))



#predictions
preds %>%
  ggplot()+
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
  geom_point(aes(D,StdRes,color=Training))+
  labs(x="",y="Standardized Residuals")+
  scale_color_manual(name="",values=c("Training"="black","Test"="red"))+
  theme(legend.position = "bottom")+
  facet_wrap(vars(State),scales="free_x")
ggsave("plots/Confirmed_stdres_facets.png", width = 150, height=120,units="mm" )

#std residuals
preds %>% group_by(State) %>% mutate(StdRes = (Confirmed-Predictions)/sd(Confirmed-Predictions)) %>% 
  ggplot()+
  geom_hline(yintercept = 0, linetype="dashed",color="grey")+
  geom_point(aes(Date,StdRes,color=Training))+
  labs(x="",y="Standardized Residuals")+
  scale_color_manual(name="",values=c("Training"="black","Test"="red"))+
  theme(legend.position = "bottom")+
  facet_wrap(vars(State),scales="free_x")
ggsave("plots/Confirmed_timeres_facets.png", width = 150, height=120,units="mm" )

acf(preds %>% filter(State==1))

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
