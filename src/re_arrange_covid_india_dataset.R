# RE-ARRANGE THE CONVID_19_INDIA_FILTERED DATASET

library(tidyr)

# import covid_india_19_filtered
india_19 <- read.csv('data/filtered_data/covid_19_india_filtered.csv',
                     row.names=1)

states <- as.list(levels(india_19$State)) # take the states
states_list <- vector("list", length(states)) # prepare a list of dataframes

for(state in states){ # for each state, build a dataframe composed by the days and confirmed cases
  Date <- india_19$Date[india_19$State == state] # take days
  confirmed <- india_19$Confirmed[india_19$State == state] # take confirmed cases
  
  region <- data.frame(Date, confirmed) # build dataframe
  region$Date <- as.Date(region$Date, format = "%d/%m/%Y") # conversion to EU format
  colnames(region)[2] <- state # rename the "confirmed" field with the name of the state
  
  states_list[[state]] <- region # put the dataframe into the list
}

merged <- merge(states_list[["Chhattisgarh"]], states_list[["Gujarat"]], by = "Date", all = TRUE)
merged <- merge(merged, states_list[["Jharkhand"]], by = "Date", all = TRUE)
merged <- merge(merged, states_list[["Madhya Pradesh"]], by = "Date", all = TRUE)
merged <- merge(merged, states_list[["Maharashtra"]], by = "Date", all = TRUE)
merged <- merge(merged, states_list[["Odisha"]], by = "Date", all = TRUE)
merged <- merge(merged, states_list[["West Bengal"]], by = "Date", all = TRUE)

# make an outerjoint according date fields (ERROR: "Chhattisgarh" is empty... consider the above code)
# merged <- Reduce(function(x, y, ...) merge(x, y, all = TRUE, sort = TRUE, ...), states_list)

merged[is.na(merged)] <- 0 # set to zero all NA values

write.csv(merged, "data/re_arranged_covid_19_india_filtered.csv") # export it

# at the end we have a dataframe composed by dates and, for each date,
# the corrisponding confirmed cases state by state.
