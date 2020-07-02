library(dplyr)
library(ggplot2)
library(tidyverse)

covid19 <- read.csv("data/covid_19_india_filtered.csv") %>% mutate(Date=as.Date(Date))
testing <- read.csv("data/testing_filtered_filled.csv") %>% mutate(Date=as.Date(Date))
census <- read.csv("data/pop_census_filtered.csv")

nadefault <- function(x,y) ifelse(is.na(x),y,x)

complete <- full_join(covid19,testing, by= c("State", "Date")) %>%
  full_join(census,by="State") %>% 
  arrange(State, Date) %>% 
  group_by(State) %>%
  transmute(Date= as.Date(Date),
            Day= as.numeric(Date-min(Date)),
            Confirmed, dConf = Confirmed - lag(Confirmed, default=0),
            Deaths, Cured, Population, Density,
            
            Removed = Deaths + Cured, dRem = Removed - lag(Removed,default=0),
            Infected = Confirmed - Removed, dInf = Infected-lag(Infected, default = 0),
            Susceptible = Population - Confirmed, dSus = nadefault(Susceptible - lag(Susceptible),0),

            TotalSamples = nadefault(TotalSamples,nadefault(lag(TotalSamples),0)), dSamp = TotalSamples - lag(TotalSamples, default = 0),
            Positive = Positive, dPos = Positive - lag(Positive, default=0),
            Negative = Negative, dNeg = Negative - lag(Negative, default=0)) %>% 
  mutate(dConf = pmax(dConf,0), dSamp=pmax(dSamp,0),dSus = pmin(dSus,0))


#SIR plot... meh
#complete %>% ggplot() +
  #geom_line(aes(Confirmed,lag_Conf))+
  #geom_line(aes(Date, Infected/Population),color="red") +
  #geom_line(aes(Date, Removed/Population),color="green") +
  #geom_line(aes(Date, Susceptible/Population),color="yellow") +
  #geom_line(aes(Date, NewPositive),color="blue") +
  
  #geom_line(aes(Date, TotalSamples), color="cyan") +
  #geom_line(aes(Date, Negative), color="green") +
  #geom_line(aes(Date, Positive), color="blue") +
  
  #geom_point(aes(Date, Infected), color="red") +
  #facet_wrap(vars(State), scales = "free_y")


#ACF of daily confirmed
plt.acfs <- function() {
  par(mfrow=c(3,3))
  for(s in levels(complete$State)){
    acf((complete %>% filter(State==s))$dConf)
  }
  par(mfrow=c(1,1))
}

plt.acfs()


#daily models

summaries = list()
anovas = list()
modls = list()
resplots = list()
predplots = list()
for(s in levels(complete$State)){
  comp2 <- complete %>% filter(State == s) %>% mutate(Day = as.numeric(Date- min(Date)))
  last_train = (nrow(comp2)-7)
  
  modl <- glm(dConf ~ Day + lag(Confirmed, default=0),
                data=comp2[1:last_train,],
                family = quasipoisson)
  modls[[s]] <- modl
  summaries[[s]] <- summary(modl)
  anovas[[s]]<- anova(modl)
  
  comp2 <- comp2 %>% cbind(Pred=predict(modl,newdata=comp2,type = "response"))
  
  SqE = (comp2$dConf - comp2$Pred)^2
  train_MSE = mean(SqE[1:last_train])
  test_MSE = mean(SqE[(last_train+1):length(SqE)])
  
  #residual plots
  resplots[[s]] <- comp2 %>% mutate(Training = Day<=last_train) %>%
    ggplot()+
    geom_hline(yintercept = 0, linetype="dashed",color="grey")+
    geom_point(aes(Pred,dConf-Pred, color=Training))+
    scale_color_manual(values=c("#ff0000","#000000"))+
    labs(title=s,x="Pred",y="Residuals")+
    #labs(title = sprintf("TrainMSE: %.3f Test_MSE: %.3f", train_MSE, test_MSE))+
    guides(color=FALSE)
  
  #preds plot
  predplots[[s]] <- comp2 %>% mutate(Training = Day<=last_train) %>%
    ggplot()+
    geom_point(aes(Date,dConf,color=Training))+
    geom_line(aes(Date,Pred), color="#3366ff",size=1)+
    scale_color_manual(values=c("#ff0000","#000000"))+
    labs(title=s,x="",y="Daily Confirmed")+
    guides(color=FALSE)
  
}
#multiplot(plotlist=resplots, cols=3)
#multiplot(plotlist=predplots, cols=3)


summaries2 = list()
anovas2 = list()
modls2 = list()
resplots2 = list()
predplots2 = list()
for(s in levels(complete$State)){
  comp2 <- complete %>% filter(State == s) %>% mutate(Day = as.numeric(Date- min(Date)))
  last_train = (nrow(comp2)-7)
  
  modl <- glm(dConf ~ Day + I(Day^2) + lag(Confirmed, default=0) + lag(TotalSamples,default=0),
              data=comp2[1:last_train,],
              family = quasipoisson)
  modls2[[s]] <- modl
  summaries2[[s]] <- summary(modl)
  anovas2[[s]]<- anova(modl)
  
  comp2 <- comp2 %>% cbind(Pred=predict(modl,newdata=comp2,type = "response"))
  
  SqE = (comp2$dConf - comp2$Pred)^2
  train_MSE = mean(SqE[1:last_train])
  test_MSE = mean(SqE[(last_train+1):length(SqE)])
  
  #residual plots
  resplots2[[s]] <- comp2 %>% mutate(Training = Day<=last_train) %>%
    ggplot()+
    geom_hline(yintercept = 0, linetype="dashed",color="grey")+
    geom_point(aes(Pred,dConf-Pred, color=Training))+
    scale_color_manual(values=c("#ff0000","#000000"))+
    labs(title=s,x="Pred",y="Residuals")+
    #labs(title = sprintf("TrainMSE: %.3f Test_MSE: %.3f", train_MSE, test_MSE))+
    guides(color=FALSE)
  
  #preds plot
  predplots2[[s]] <- comp2 %>% mutate(Training = Day<=last_train) %>%
    ggplot()+
    geom_point(aes(Date,dConf,color=Training))+
    geom_line(aes(Date,Pred), color="#3366ff",size=1)+
    scale_color_manual(values=c("#ff0000","#000000"))+
    labs(title=s,x="",y="Daily Confirmed")+
    guides(color=FALSE)
  
}
#multiplot(plotlist=resplots2, cols=3)
#multiplot(plotlist=predplots2, cols=3)

#no save... go manual (ie zoom the viewport, resize window and copy the image)
multiplot(plotlist=c(resplots2[1:3],resplots[4:6],resplots2[7]), cols=3)
multiplot(plotlist=c(predplots2[1:3],predplots[4:6],predplots2[7]), cols=3)

# print deviances
devs=c()
for(s in names(summaries)){
  print(s)
  print(c(summaries[[s]]$deviance,summaries2[[s]]$deviance ))
}

#print pvals
pvals=c()
for(s in names(summaries)){
  print(s)
  pvals=cbind(pvals,s=c(summaries[[s]]$coefficients[,4],summaries2[[s]]$coefficients[,4]))
}
names(summaries)

#print anovas
for(s in names(summaries)){
  print(s)
  print(anova(modls[[s]],modls2[[s]],test = "F"))
}

#cumulative predictions plot
predconfplots = list()
for(s in levels(complete$State)){
  comp2 <- complete %>% filter(State == s) %>% mutate(Day = as.numeric(Date- min(Date)))
  
  predconfplots[[s]] <- 
    comp2 %>%
    cbind(predConfirmed1 = cumsum(predict(modls[[s]],
                                         newdata=comp2,
                                         type = "response"  ) ),
          predConfirmed2 = cumsum(predict(modls2[[s]],
                                          newdata=comp2,
                                          type = "response"  ) )) %>% 
    ggplot()+
    geom_line(aes(Date,predConfirmed1),color="blue",size=1)+
    geom_line(aes(Date,predConfirmed2),color="red")+
      geom_point(aes(Date,Confirmed),shape=20)+
    labs(title=s)
  
}
multiplot(plotlist=predconfplots, cols=3)


#FINAL MODELS
preds = data.frame()
modls = list()
{
s = levels(complete$State)[1]
modl <- glm(dConf ~ -1 + lag(Confirmed, default=0) + lag(TotalSamples,default=0),
            data=(complete %>% filter(State == s) %>% filter(row_number()<n()-6)),
            family = quasipoisson)
preds = rbind(preds,
              data.frame(State=s,
                         DailyPred =predict(modl,
                                            newdata=(complete %>% filter(State == s)),
                                            type = "response")) %>%
                mutate(CumPred = cumsum(DailyPred),
                       Day = (row_number()-1),
                       Train = ifelse(row_number()<(n()-6),"Training","Test" )))
modls[[s]] = modl

s = levels(complete$State)[2]
modl <- glm(dConf ~ Day + I(Day^2) + lag(Confirmed, default=0) + lag(TotalSamples,default=0),
            data=(complete %>% filter(State == s) %>% filter(row_number()<n()-6)),
            family = quasipoisson)
preds = rbind(preds,
              data.frame(State=s,
                         DailyPred =predict(modl,
                                            newdata=(complete %>% filter(State == s)),
                                            type = "response")) %>%
                mutate(CumPred = cumsum(DailyPred),
                       Day = (row_number()-1),
                       Train = ifelse(row_number()<(n()-6),"Training","Test" )))
modls[[s]] = modl

s = levels(complete$State)[3]
modl <- glm(dConf ~ -1+ Day + I(Day^2) + lag(Confirmed, default=0) + lag(TotalSamples,default=0),
            data=(complete %>% filter(State == s) %>% filter(row_number()<n()-6)),
            family = quasipoisson)
preds = rbind(preds,
              data.frame(State=s,
                         DailyPred =predict(modl,
                                            newdata=(complete %>% filter(State == s)),
                                            type = "response")) %>%
                mutate(CumPred = cumsum(DailyPred),
                       Day = (row_number()-1),
                       Train = ifelse(row_number()<(n()-6),"Training","Test" )))
modls[[s]] = modl

for(s in levels(complete$State)[4:5]){
  modl <- glm(dConf ~  Day  + lag(Confirmed, default=0),
              data=(complete %>% filter(State == s) %>% filter(row_number()<n()-6)),
              family = quasipoisson)
  preds = rbind(preds,
                data.frame(State=s,
                           DailyPred =predict(modl,
                                              newdata=(complete %>% filter(State == s)),
                                              type = "response")) %>%
                  mutate(CumPred = cumsum(DailyPred),
                         Day = (row_number()-1),
                         Train = ifelse(row_number()<(n()-6),"Training","Test" )))
  modls[[s]] = modl
}

s = levels(complete$State)[6]
modl <- glm(dConf ~ -1+ I(Day^2) + lag(Confirmed, default=0),
            data=(complete %>% filter(State == s) %>% filter(row_number()<n()-6)),
            family = quasipoisson)
preds = rbind(preds,
              data.frame(State=s,
                         DailyPred =predict(modl,
                                            newdata=(complete %>% filter(State == s)),
                                            type = "response")) %>%
                mutate(CumPred = cumsum(DailyPred),
                       Day = (row_number()-1),
                       Train = ifelse(row_number()<(n()-6),"Training","Test" )))
modls[[s]] = modl

s = levels(complete$State)[7]
modl <- glm(dConf ~ -1+ Day + I(Day^2) + lag(TotalSamples,default=0),
            data=(complete %>% filter(State == s) %>% filter(row_number()<n()-6)),
            family = quasipoisson)
preds = rbind(preds,
              data.frame(State=s,
                         DailyPred =predict(modl,
                                            newdata=(complete %>% filter(State == s)),
                                            type = "response")) %>%
                mutate(CumPred = cumsum(DailyPred),
                       Day = (row_number()-1),
                       Train = ifelse(row_number()<(n()-6),"Training","Test" )))
modls[[s]] = modl
}

#check right models
parameters = data.frame()
for(s in names(modls)){
  print(s)
  #print(modls[[s]]$call)
  #print(summary(modls[[s]]))
  parameters = rbind(parameters,
                     cbind(
                     cbind(data.frame(summary(modls[[s]])$coefficients),
                           Covariate = row.names(summary(modls[[s]])$coefficients)),
                     confint(modls[[s]]))
                     %>%  mutate(State=s))
}



colnames(parameters) <- c("Estimate","StdErr","tvalue","pvalue","Covariate","low","high","State")
parameters %>% mutate(Covariate = factor(Covariate, labels= c("Y.Conf","Y.TotSamp","b_0","Day","Day^2"))) %>% 
  filter(Covariate %in%  c("Day","Day^2")) %>% 
  ggplot()+
  geom_point(aes(Covariate, Estimate))+
  geom_errorbar(aes(Covariate, ymax=high, ymin=low))+
  facet_wrap(vars(State),scales="free_x")
ggsave("plots/Daily_params_days.png", width = 150, height=120,units="mm" )

final = full_join(complete,preds, by= c("State", "Day"))

#preds
final %>% ggplot()+
  geom_point(aes(Date,dConf,color=Train))+
  geom_line(aes(Date,DailyPred,color="Fitted"), size=1)+
  #geom_ribbon(aes(Date,ymin=Lower,ymax=Upper),color="#3366ff",fill="green",alpha=.5) +
  scale_color_manual(values=c("#ff0000","#000000"))+
  labs(x="",y="Daily Confirmed + Fit")+
  scale_color_manual(name="",values=c("Fitted"="#3366ff","Training"="black","Test"="red"))+
  theme(legend.position = "bottom")+
  facet_wrap(vars(State),scales="free_y")
ggsave("plots/Daily_pred_facets.png", width = 150, height=120,units="mm" )

#stdres
final %>% group_by(State) %>% mutate(StdRes = (dConf-DailyPred)/sd(dConf-DailyPred)) %>% 
  ggplot()+
  geom_hline(yintercept = 0, linetype="dashed",color="grey")+
  geom_point(aes(DailyPred,StdRes,color=Train))+
  labs(x="",y="Standardized Residuals")+
  scale_color_manual(name="",values=c("Training"="black","Test"="red"))+
  theme(legend.position = "bottom")+
  facet_wrap(vars(State),scales="free_x")
ggsave("plots/Daily_stdres_facets.png", width = 150, height=120,units="mm" )

#confirmed preds
final %>% ggplot()+
  geom_point(aes(Date,Confirmed,color=Train))+
  geom_line(aes(Date,CumPred,color="Fitted"), size=1)+
  #geom_ribbon(aes(Date,ymin=Lower,ymax=Upper),color="#3366ff",fill="green",alpha=.5) +
  scale_color_manual(values=c("#ff0000","#000000"))+
  labs(x="",y="Daily Confirmed + Fit")+
  scale_color_manual(name="",values=c("Fitted"="#3366ff","Training"="black","Test"="red"))+
  theme(legend.position = "bottom")+
  facet_wrap(vars(State),scales="free_y")
ggsave("plots/Confirmed_predDaily_facets.png", width = 150, height=120,units="mm" )

#stdres
final %>% group_by(State) %>% mutate(StdRes = (Confirmed-CumPred)/sd(Confirmed-CumPred)) %>% 
  ggplot()+
  geom_hline(yintercept = 0, linetype="dashed",color="grey")+
  geom_point(aes(DailyPred,StdRes,color=Train))+
  labs(x="",y="Standardized Residuals")+
  scale_color_manual(name="",values=c("Training"="black","Test"="red"))+
  theme(legend.position = "bottom")+
  facet_wrap(vars(State),scales="free_x")
ggsave("plots/Confirmed_Daily_stdres_facets.png", width = 150, height=120,units="mm" )
