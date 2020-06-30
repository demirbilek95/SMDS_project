library(dplyr)
library(ggplot2)
library(tidyr)

#plotting the curve of the infected people for each state
covid_19 <- read.csv("data/covid_19_india_filtered.csv") %>% mutate(Date = as.Date(Date))
states <- unique(covid_19$State)

covid_19 %>% 
  ggplot()+
  geom_line(aes(Date,Confirmed, color="Confirmed"), size=1)+
  geom_line(aes(Date,Cured, color="Cured")) +
  geom_line(aes(Date,Deaths,color="Deaths")) +
  labs(title="Confirmed cases / Cured / Deaths per Region", y="Confirmed/Cured/Deaths", x="")+
  scale_color_manual(name= "", values=c("Confirmed"="orange","Deaths"="red","Cured"="blue"))+
  theme(legend.position = "bottom")+
  facet_wrap(vars(State), scales="free_y")

#Interesting note: Cured+Deaths lags behind confirmed of about 14 days

ggsave("plots/confirmed+death+cured_facets.png", width = 150, height=120,units="mm" )

#plot testing data facets
testing_details <- read.csv("data/testing_filtered_filled.csv") %>% mutate(Date=as.Date(Date))
testing_details %>%
  ggplot()+
  geom_line(aes(Date,TotalSamples, color="Total"), size=1)+
  geom_line(aes(Date,Positive, color="Positive")) +
  geom_line(aes(Date,Negative, color="Negative")) +
  scale_color_manual(name= "Swabs:", values=c("Total"="black","Negative"="magenta","Positive"="red"))+
  theme(legend.position = "bottom")+
  labs(title="Total/Positive/Negative cumulative swabs count per Region", y="Samples", x="")+
  facet_wrap(vars(State), scales="free_y")
ggsave("plots/tests_full_facets.png", width = 150, height=120,units="mm" )

#Notes:
# 1.Lots of missing data for positive/negative swabs.
# 2.Counts don't add up when both present (see next graph)
# 3.cumulation is broken in some cases (see next two graphs)

#TotalTests != positive+negative demonstration
testing_details %>% transmute(Date,State,diff=TotalSamples-Positive-Negative) %>%
  ggplot() +
  geom_line(aes(Date,diff), size=1)+
  labs(title="Swabs data is inconsistent!", y="Total Samples - (Positive+Negative)", x="")+
  facet_wrap(vars(State), scales="free_y")
ggsave("plots/tests_problems1_facets.png", width = 150, height=120,units="mm" )

#new cases and new samples
td <- testing_details %>%
  arrange(State, Date)%>%
  group_by(State) %>%
  mutate(NewSamples = TotalSamples - lag(TotalSamples, default=0))
cd <- covid_19 %>% 
  group_by(State) %>%
  mutate(NewCases = Confirmed - lag(Confirmed,default=0),
         Infected = Confirmed - Deaths - Cured)

#totaltests is not always cumulative!
ggplot(td)+
  geom_line(aes(Date,NewSamples)) +
  geom_point(aes(Date,NewSamples),data=td[td$NewSamples<0,], color="red", shape=4)+
  geom_text(aes(Date,NewSamples,label=NewSamples),data=td[td$NewSamples<0,],
            size=3, fontface="bold", hjust=1, nudge_x = -2, color="red")+
  labs(title="Daily swabs count",subtitle = "(dirty datapoints in red)", y="Daily samples", x="")+
  facet_wrap(vars(State),scales="free_y")
ggsave("plots/tests_problems2_facets.png", width = 150, height=120,units="mm" )

#and in some cases neither do confirmed cases! (hard to spot)
ggplot(cd)+
  geom_line(aes(Date, NewCases),size=1) +
  geom_point(aes(Date,NewCases),data=cd[cd$NewCases<0,], color="red", shape=4)+
  geom_text(aes(Date,c(500,500,25),label=NewCases),data=cd[cd$NewCases<0,],
            size=3, fontface="bold", color="red")+
  labs(title="Daily novel confirmed cases", subtitle = "(dirty datapoints in red)", y="Confirmed cases", x="")+
  facet_wrap(vars(State),scales="free_y")
ggsave("plots/newcases_facets.png", width = 150, height=120,units="mm" )

# new cases and new samples rescaled
tdcd <- full_join(cd,td, by= c("State", "Date")) %>% arrange(State,Date) %>% 
  group_by(State) %>%
  mutate(Removed = Deaths + Cured,
         nNewCases = pmax(NewCases,0) / max(NewCases, na.rm = TRUE),
         nNewSamples = pmax(NewSamples,0) / max(NewSamples, na.rm = TRUE))

ggplot(tdcd)+
  geom_line(aes(Date, nNewSamples,color="Daily Swabs"), ) +
  geom_line(aes(Date, nNewCases, color="Daily Confirmed")) +
  labs(title="Rescaled Daily swabs vs Daily Confirmed", y="Normalized Daily Counts", x="")+
  scale_color_manual(name="",values=c("Daily Swabs"="blue", "Daily Confirmed"="red"))+
  theme(legend.position = "bottom")+
  facet_wrap(vars(State),scales="free_y")+
ggsave("plots/newsamps+newcases_scaled_facets.png", width = 150, height=120,units="mm" )



# tests-------------
ggplot(tdcd)+
  geom_line(aes(Date, Removed,color="Removed"), ) +
  geom_line(aes(Date, Confirmed, color="Confirmed")) +
  labs(title="Samples and Confirmed", y="Daily Cumulative", x="")+
  scale_color_manual(name="",values=c("Removed"="blue", "Confirmed"="red"))+
  theme(legend.position = "bottom")+
  facet_wrap(vars(State),scales="free_y")

library(stats)

lags_nCases.nSamples= c()
for(ss in states){
  testdf = tdcd[tdcd$State==ss,]
  testdf = testdf[!is.na(testdf$nNewSamples),]
  corrs = ccf(testdf$NewCases, testdf$NewSamples,lag.max = 30)
  lags_nCases.nSamples <- lags_nCases.nSamples %>% 
    rbind(data.frame(State=ss,lagnCases=corrs$lag,ACF=corrs$acf))
}

lags_nCases.nSamples %>% ggplot()+
  geom_line(aes(lagnCases,ACF,color=State,alpha=ACF),size=1)+
  scale_alpha_continuous( guide="none")


#-------

testdf = tdcd[tdcd$State==states[6],] %>% mutate(NewCases = pmax(NewCases,0))
testdf$State[1]
modl = glm(NewCases~ Date +
             lag(NewSamples,6,default = 0)+
             lag(NewSamples,5,default = 0)+
             lag(NewSamples,4,default = 0)+
             lag(NewSamples,3,default = 0)+
             lag(NewSamples,2,default = 0)+
             lag(NewSamples,1,default = 0)+
             lag(NewCases,default = 0),
           data=testdf,
           family=poisson)
summary(modl)
testdf %>% cbind(pred=modl$fitted.values) %>%
  ggplot()+
  geom_line(aes(Date,nNewSamples*max(NewCases)),color="green",alpha=.4)+
  geom_point(aes(Date,NewCases))+
  geom_line(aes(Date,pred),color="red")
  
#population and density
census <- read.csv("data/pop_census_filtered.csv")
census %>% select(State, Rural.population, Urban.population, Density) %>%
  mutate(labpos = pmax(Rural.population,Urban.population)) %>% 
  gather("Stat", "Val",-State, -Density, -labpos) %>% 
  ggplot()+
  geom_bar(aes(State,Val, fill=Stat, alpha = Density), stat="identity", position = "dodge",color="black") +
  scale_fill_hue( labels=c("Rural","Urban"))+
  scale_alpha(guide="none")+
  geom_text(aes(State, labpos, label=paste(Density,"~pop/m^2"), group=State), parse=TRUE, size=2.5, position = position_dodge(0.9), vjust=-.25)+
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        legend.key.size = unit(.5,"cm"),
        legend.justification = a,
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.position = c(.01,.75)) +
  labs(x="",y="Population", fill="Type", alpha = expression(paste("Density (",pop/m^2,")")))
ggsave("plots/pop+density_hist.png", width = 150, height=120,units="mm" )

census %>% ggplot(aes(State,Density)) +
  geom_bar(stat="identity")
ggsave("plots/density_hist.png", width = 150, height=120,units="mm" )
