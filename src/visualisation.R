library(dplyr)
library(ggplot2)

#plotting the curve of the infected people for each state
covid_19 <- read.csv("data/covid_19_india_filtered.csv")
states <- unique(covid_19$State)

ggplot(covid_19)+
  geom_point(aes(X,Confirmed))+
  geom_point(aes(X,Deaths), color="red", shape=20) +
  geom_point(aes(X,Cured), color="green", shape=20) +
  facet_wrap(vars(State), scales="free_y")

ggsave("plots/confirmed+death+cured_facets.png", width = 10, height=8 )

#plot testing data facets
testing_details <- read.csv("data/testing_filtered_filled.csv")
ggplot(testing_details)+
  geom_point(aes(Date,TotalSamples))+
  geom_point(aes(Date,Positive), color="red", shape=20) +
  geom_point(aes(Date,Negative), color="green", shape=20) +
  facet_wrap(vars(State), scales="free_y")

ggsave("plots/tests_facets.png", width = 10, height=8 )


#new cases and new samples
td <- testing_details %>%
  arrange(State, Date)%>%
  group_by(State) %>%
  mutate(NewSamples = TotalSamples - lag(TotalSamples))
cd <- covid_19 %>% 
  group_by(State) %>%
  mutate(NewCases = Confirmed - lag(Confirmed), Infected = Confirmed - Deaths - Cured)

ggplot(td)+
  geom_point(aes(Date,NewSamples), color="Blue") +
  facet_wrap(vars(State),scales="free_y")
ggsave("plots/newsamples_facets.png", width = 10, height=8 )
ggplot(cd)+
  geom_point(aes(Date, NewCases), color="Red") +
  facet_wrap(vars(State),scales="free_y")
ggsave("plots/newcases_facets.png", width = 10, height=8 )

# new cases and new samples rescaled
tdcd <- full_join(cd,td, by= c("State", "Date"))
tdcd <- tdcd %>% group_by(State) %>%
  mutate(nNewCases = NewCases / max(NewCases, na.rm = TRUE),
         nNewSamples = NewSamples / max(NewSamples, na.rm = TRUE))
ggplot(tdcd)+
  geom_point(aes(Date, nNewSamples), color="Blue") +
  geom_point(aes(Date, nNewCases), color="Red") +
  facet_wrap(vars(State),scales="free_y")
ggsave("plots/newsamps+newcases_scaled_facets.png", width = 10, height=8 )

#population and density
census <- read.csv("data/pop_census_filtered.csv")
census %>% select(State, Rural.population, Urban.population, Density) %>% 
  gather("Stat", "Val",-State, -Density) %>% 
  ggplot(aes(State,Val, fill=Stat, alpha = Density))+
  geom_bar(stat="identity", position = "dodge",color="black")
ggsave("plots/pop+density_hist.png", width = 10, height=8 )

census %>% ggplot(aes(State,Density)) +
  geom_bar(stat="identity")
ggsave("plots/density_hist.png", width = 10, height=8 )
