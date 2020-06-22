library(dplyr)
library(ggplot2)
#library(egg)

covid_19 <- read.csv("data/covid_19_india_filtered.csv")
states <- (covid_19 %>% select(State) %>% unique)$State

for (state in states) {
    covid_19 %>% 
    filter(State == state) %>% 
    ggplot(aes(1:nrow(.), Confirmed, colours = State)) +
    geom_point() +
    labs(x = "Time", y = "Confirmed", title = paste("State", state))
  ggsave(paste0("plots/confirmed_", state, ".png"))
  #append(plots, c(p))
}

#ggarrange(p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8])
#grid.arrange(p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], nrow = 3)

