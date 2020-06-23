library(dplyr)
covid_19_india <- read.csv("data/datasets_557629_1267081_covid_19_india.csv")
covid_19_india <- rename(covid_19_india, State = State.UnionTerritory)
pop_census <- read.csv("data/datasets_557629_1267081_population_india_census2011.csv")
pop_census <- rename(pop_census, State = State...Union.Territory)
testing_details <- read.csv("data/datasets_557629_1267081_StatewiseTestingDetails.csv")
testing_lab <- read.csv("data/ICMRTestingLabs.csv")
testing_lab <- rename(testing_lab, State = state)
hospital <- read.csv("data/HospitalBedsIndia.csv")
hospital <- rename(hospital, State = State.UT)

states <- c("Gujarat","Maharashtra","Madhya Pradesh",
            "Chhattisgarh","Jharkhand","Odisha",
            "West Bengal")

state_filter <- function(data, states){
  filtered_data_set <- 
    data %>% filter(State %in% states)
  return(filtered_data_set)
}

select_columns <- function(data, columns){
  df <- data %>% select(columns)
  return(df)
}


covid_19_india <- state_filter(covid_19_india, states)
pop_census <- state_filter(pop_census, states)
testing_details <- state_filter(testing_details, states)
testing_lab <- state_filter(testing_lab,states)
hospital <- state_filter(hospital,states)

covid_19_india <- select_columns(covid_19_india,c("Date","State","Confirmed"))
pop_census <- select_columns(pop_census, 
                             c("State","Population","Rural.population","Urban.population",
                               "Area","Density","Gender.Ratio"))
testing_lab <- select_columns(testing_lab, c("lab","State"))
## Aggregate the number of testing lab
testing_lab <- testing_lab %>% group_by(State) %>% summarise(num_lab = n())


hospital <- select_columns(hospital,c("State", "TotalPublicHealthFacilities_HMIS"))

# Merging with other files
temp <- merge(covid_19_india,pop_census,by.x = "State", by.y = "State")
temp2 <- merge(temp, testing_lab, by.x = "State", by.y = "State" )
ind_data_merged <- merge(temp2, hospital, by.x = "State", by.y = "State")

covid_19_india <- 
  covid_19_india %>% 
  mutate(Confirmed = Confirmed - Cured - Deaths) %>% 
  select(Date, State, Confirmed)


write.csv(covid_19_india, "data/covid_19_india_filtered.csv")
write.csv(pop_census, "data/pop_census_filtered.csv")
write.csv(testing_details, "data/testing_filtered.csv")
write.csv(testing_lab, "data/lab_filtered.csv")
write.csv(hospital, "data/hospital.csv")
write.csv(ind_data_merged, "data/ind_data_merged.csv")


