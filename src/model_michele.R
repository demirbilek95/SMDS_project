library(dplyr)
library(ggplot2)

covid19 <- read.csv("data/covid_19_india_filtered.csv")
testing <- read.csv("data/testing_filtered_filled.csv")
census <- read.csv("data/pop_census_filtered.csv")


complete <- full_join(covid19,testing, by= c("State", "Date")) %>%
  full_join(census,by="State") %>% 
  arrange(State, Date) %>% 
  group_by(State) %>%
  transmute(Date= as.Date(Date),
            Confirmed, lag_Conf = lag(Confirmed, default=0),
            NewConf = Confirmed - lag(Confirmed, default=0),
            Infected = Confirmed - Deaths - Cured, lag_Inf = lag(Infected, default = 0),
            Susceptible = Population - Confirmed, lag_Sus = lag(Susceptible),
            SusRatio = (Population - Confirmed)/Population, lag_SusRatio = lag(SusRatio),
            TotalSamples,
            NewSamples = TotalSamples - lag(TotalSamples, default = 0),
            NewSampRatio = NewSamples/Susceptible, lag_NewSampRatio = lag(NewSampRatio, default = 0),
            Positive, NewPositive = Positive - lag(Positive, default=0), Negative) %>% 
  rowwise() %>% 
  mutate(NewSamples = max(NewSamples,0),
         NewSampRatio = max(NewSampRatio,0),
         lag_NewSampRatio = max(lag_NewSampRatio,0))

complete %>% ggplot() +
  #geom_line(aes(Confirmed,lag_Conf))+
  geom_line(aes(Date, NewConf),color="red") +
  geom_line(aes(Date, NewSamples),color="blue") +
  #geom_line(aes(Date, NewPositive),color="blue") +
  #geom_point(aes(Date, Negative), color="green") +
  #geom_point(aes(Date, Positive+Negative), color="blue") +
  #geom_point(aes(Date, TotalSamples), color="cyan") +
  #geom_point(aes(Date, Infected), color="red") +
  facet_wrap(vars(State), scales = "free")

states <- levels(complete$State)

comp2 <- complete %>% filter(State == states[3]) %>% 
  mutate()

comp2$lag_Sus[1] = comp2$lag_Sus[2]
comp2$lag_SusRatio[1] = comp2$lag_SusRatio[2]

comp2 <- comp2 %>% mutate(lags =  lag_Inf * lag_NewSampRatio * lag_SusRatio)

comp2 %>% ggplot() +
  geom_line(aes(Confirmed,lag_Conf))
  #geom_line(aes(Date, NewConf),color="red") +
  #geom_line(aes(Date, NewPositive),color="blue") +
  #geom_point(aes(Date, Negative), color="green") +
  #geom_point(aes(Date, Positive+Negative), color="blue") +
  #geom_point(aes(Date, TotalSamples), color="cyan") +
  #geom_point(aes(Date, Infected), color="red") +



comp2$t = 1:nrow(comp2)
#idx <- floor(nrow(comp2)*.8)
train <- comp2[1: (nrow(comp2)-7),]
test <- comp2[(nrow(comp2)-6):nrow(comp2),]

sirmodel <- glm(NewConf ~ -1 + NewSamples, data=comp2, family = poisson)
summary(sirmodel)


predict(sirmodel)

plot(comp2$Date,comp2$Confirmed)
lines(comp2$Date,predict(sirmodel,newdata = comp2),col="red")
