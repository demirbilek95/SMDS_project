# RE-ARRANGE THE CONVID_19_INDIA_FILTERED DATASET

library(tidyr)

# inport covid_india_19_filtered
india_19 <- read.csv('~/Documents/Statistical_method_data_science/smds_exercises/SMDS_project/data/covid_19_india_filtered.csv', row.names=1)

states <- as.list(levels(india_19$State)) # take the states
states_list <- vector("list", length(states)) # prepare a list of dataframes

for(state in states){ # for each state, build a dataframe composed by the days and confirmed cases
  Date <- as.character.Date(india_19$Date[india_19$State == state]) # take days
  confirmed <- india_19$Confirmed[india_19$State == state] # take confirmed cases
  
  region <- data.frame(Date, confirmed) # build dataframe
  colnames(region)[2] <- state # rename the "confirmed" field with the name of the state
  
  states_list[[state]] <- region # put the dataframe into the list
}

merged <- Reduce(function(...) merge(..., all = TRUE), states_list) # make an outerjoint according date fields
merged <- merged[order(as.Date(merged$Date, format="%d/%m/%Y")),] # order by date

write.csv(merged, "data/re_arranged_covid_19_india_filtered.csv") # export it

# at the end we have a dataframe composed by dates and, for each date,
# the corrisponding confirmed cases state by state.

