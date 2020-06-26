# AVERAGE PERCENT CHANGE CONFIRMED CASES
# This script provide the average percent change of confirmed cases for each
# week, and also the difference in respect the previous week (so it is a delta);
# at the end provide a "delt"a bar plot for one specific week.

library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)

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
                                              covid_19_india_per_week[i-1, state]) / 
                                              covid_19_india_per_week[i-1, state]) / period) * 100
    }
  }
}

# discard NaN belong from dividing for zero
increment_percentage[is.na(increment_percentage)] <- 0

# now we want to make a plot to see the general percentage trends...
percent_nrow <- nrow(increment_percentage) # number of rows
col_names <- colnames(increment_percentage) # coulmn names

# prepare data for plot
gplot <- gather(increment_percentage, key = "states", value = "percentage")
gplot$week <- rep(1:percent_nrow, length(col_names)) # add weeks

ggplot(gplot, aes(x = week, y = percentage)) + 
  geom_line(aes(group = states, color = states)) +
  labs(x = "Weeks", y = "Percentage", 
       title = "Average percent change", subtitle = "week by week")


# increment/decrement percentage in respect the previous week
difference_increment <- data.frame(diff(as.matrix(increment_percentage)))
rownames(difference_increment) <- NULL # reset row indexes

# now we want to make a plot to see the general different percentage trends...
percent_nrow <- nrow(difference_increment)

# prepare data for plot
gplot <- gather(difference_increment, key = "states", value = "percentage")
gplot$week <- rep(1:percent_nrow, length(col_names)) # add weeks

ggplot(gplot, aes(x = week, y = percentage)) + 
  geom_line(aes(group = states, color = states)) +
  labs(x = "Weeks", y = "Percentage", 
       title = "Average percent change", subtitle = "delta weeks")

# now we want to make a plot for one specific week...
# increment/decrement percentage of last week
week_plot <- percent_nrow -1 # selected before last week
week_plot <- percent_nrow # selected last week

# re-shape to have state and percentage columns
last_increment <- gather(difference_increment[week_plot, ], 
                         states, percentage,col_names, 
                         na.rm = FALSE, convert = FALSE)

# add fill column to distinguish percentage less than zero and greater than zero
last_increment$fill <- ifelse(last_increment$percentage < 0, "orange", "red")

# plot last difference increment percentage
ggplot(last_increment, aes(states, percentage, label = states, fill = fill)) + 
  geom_bar(position = 'dodge', stat = "identity") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "state", y = "percentage", title = "average variation of confirmed cases", 
       subtitle = paste("week", week_plot)) + 
  coord_flip() + theme_bw() + theme(axis.text.y = element_blank()) + 
  theme(legend.position = "none") + 
  geom_text(aes(y = 0, label=states, hjust = ifelse(fill == "orange", 0, 1)), 
            position=position_dodge(width = 0), vjust = 0.5)

# this bar plot show us the percentage difference between contiguous week,
# so we have the percent variation
