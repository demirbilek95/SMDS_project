# AVERAGE PERCENT CHANGE CONFIRMED CASES BAR-PLOT

library(tidyr)
library(dplyr)

# load re_arranged_covid_19_india_filtered.csv
covid_19_india <- read.csv('~/Documents/Statistical_method_data_science/smds_exercises/SMDS_project/data/re_arranged_covid_19_india_filtered.csv', row.names = 1)

# discard date column (it is not necessary here)
covid_19_india <- covid_19_india[, -1]

period <- 7 # one week

# find the row for each week
indexes <- which((as.numeric(rownames(covid_19_india)) - 1) %% period == 0) 

# take the cumulative confirmed each week
covid_19_india_per_week <- covid_19_india[indexes, ] 

# prepare the dataframe to the increment percentage for each state
increment_percentage <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), colnames(covid_19_india))

# for each state, compute the increment percentage per week
for(state in colnames(covid_19_india)){
  for(i in 2:nrow(covid_19_india_per_week)){
    if(covid_19_india_per_week[i-1, state] != 0){
      increment_percentage[i-1, state] <- (((covid_19_india_per_week[i, state] - 
                                               covid_19_india_per_week[i-1, state]) / covid_19_india_per_week[i-1, state]) / period) * 100
    }
  }
}

# discard NaN belong from dividing for zero
increment_percentage[is.na(increment_percentage)] <- 0

ciccio <- data.matrix(increment_percentage, rownames.force = NA)
colnames(ciccio) <- NULL

# compute the final percentage (we exclude Date from increment_percentage)
ciccio <- colSums(ciccio) / (nrow(covid_19_india_per_week) - 1)
ciccio <- ciccio[-8]

increment_percentage <- data.frame(states = colnames(covid_19_india), percentage = ciccio)

increment_percentage <- gather(increment_percentage, state, percent, colnames(increment_percentage), na.rm = FALSE, convert = FALSE)

ggplot(dtm, aes(states, percentage, label = states,
                hjust = hjust)) + geom_text(aes(y = 0,
                colour = colour)) + geom_bar(stat = "identity",
                aes(fill = colour))