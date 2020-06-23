library(dplyr)
library(ggplot2)
library(purrr)
#plotting the curve of the infected people for each state
covid_19 <- read.csv("data/re_arranged_covid_19_india_filtered.csv")
states <- c("Gujarat","Maharashtra","Madhya.Pradesh",
            "Chhattisgarh","Jharkhand","Odisha",
            "West.Bengal")

for (state in states) {
  covid_19 %>% 
    ggplot(aes(x = X, y = !!ensym(state))) + # !!ensym transforms a string to a symbol
    geom_point() +
    labs(x = "Time", y = "Infected", title = paste("State", state))
  ggsave(paste0("plots/", "infected_", state, ".png"))
}


