# Filtering data for states just to try

library(dplyr)
covid_19_india <- read.csv("data/ind_data_merged.csv")
covid_19_india

str(covid_19_india)

covid_19_india$State

jharkhand_data  <-  covid_19_india %>% filter(State == "Jharkhand")
jharkhand_data
########################################################################

# Plotting total confirmed cases for states

library(ggplot2)

covid_19_india <- read.csv("data/covid_19_india_filtered.csv")

tot_states <- covid_19_india %>% group_by(State) %>% summarise(
  tot_confirmed = sum(Confirmed)
)

plot(jharkhand_data$Confirmed)

options(scipen=100)
ggplot(tot_states,aes(x=State, y = tot_confirmed)) + geom_bar(stat = "identity") + scale_y_continuous("Total Confirmed Cases")
#ggsave(paste0("plots/total_confirmed_states.png"))

#########################################################################

# Map Plot

library(sp)
library(RColorBrewer)
ind1=readRDS("data/IND_adm/IND_adm1.rds")

spplot(ind1, "NAME_1", scales=list(draw=T), colorkey=F, main="India")
ind1

?merge

ind1$NAME_1 = as.factor(ind1$NAME_1)
ind1$fake.data = merge(ind1@data,tot_states,by.x = "NAME_1", by.y = "State",all.x = TRUE)$tot_confirmed


spplot(ind1,"fake.data",main = "Central India and Total Confirmed Cases",
       colorkey=T, scales=list(draw=T))

ind1$fake.data[is.na(ind1$fake.data)] <- 0

?spplot


merge(ind1@data,tot_states,by.x = "NAME_1", by.y = "State",all.x = TRUE)



