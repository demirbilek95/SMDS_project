library(dplyr)
covid_19_india <- read.csv("data/datasets_557629_1267081_covid_19_india.csv")
covid_19_india <- rename(covid_19_india, State = State.UnionTerritory)
pop_census <- read.csv("data/datasets_557629_1267081_population_india_census2011.csv")
pop_census <- rename(pop_census, State = State...Union.Territory)
testing_details <- read.csv("data/datasets_557629_1267081_StatewiseTestingDetails.csv")

states <- c("Gujarat","Maharashtra","Madhya Pradesh",
            "Chhattisgarh","Jharkhand","Odisha",
            "West Bengal")

state_filter <- function(data, states){
  filtered_data_set <- 
    data %>% filter(State %in% states)
  return(filtered_data_set)
}

covid_19_india <- state_filter(covid_19_india, states)
pop_census <- state_filter(pop_census, states)
testing_details <- state_filter(testing_details, states)

covid_19_india <- 
  covid_19_india %>% 
  select(Date, State, Confirmed)

write.csv(covid_19_india, "data/covid_19_india_filtered.csv")
write.csv(pop_census, "data/pop_census_filtered.csv")
write.csv(testing_details, "data/testing_filtered.csv")