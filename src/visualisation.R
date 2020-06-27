library(dplyr)
library(ggplot2)

#plotting the curve of the infected people for each state
covid_19 <- read.csv("data/covid_19_india_filtered.csv")
states <- unique(covid_19$State)

ggplot(covid_19)+
  geom_point(aes(X,Confirmed-Deaths-Cured))+
  geom_point(aes(X,Deaths), color="red") +
  geom_point(aes(X,Cured), color="green") +
  facet_wrap(vars(State), scales="free_y")

ggsave("plots/infect+death+cured_facets.png", width = 10, height=8 )


ggplot(covid_19)+
  geom_point(aes(X,Confirmed))+
  facet_wrap(vars(State), scales="free_y")

ggsave("plots/confirmed_facets.png", width = 10, height=8 )

for (state in states) {
  covid_19 %>% 
    ggplot(aes(x = X, y = !!ensym(state))) + # !!ensym transforms a string to a symbol
    geom_point() +
    labs(x = "Time", y = "Infected", title = paste("State", state))
  ggsave(paste0("plots/", "infected_", state, ".png"))
}