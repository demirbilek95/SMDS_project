library(dplyr)
library(ggplot2)

covid_19_india <- read.csv("data/re_arranged_covid_19_india_filtered.csv")

# Plotting total confirmed cases for states
data_plot <- covid_19_india[nrow(covid_19_india),] # Taking the last date(row) of the dataset

# Deleting unnecessery coolumns
data_plot$X = NULL
data_plot$Date = NULL

colnames <- colnames(data_plot)
colnames[4] <- "Madhya Pradesh"
colnames[7] <- "West Bengal"

data_plot[2,] <- colnames # Adding state names as row

data_plot <- data.frame(t(data_plot))
data_plot <- rename(data_plot, State = "X2",Confirmed = "X105")
data_plot$Confirmed <- as.numeric(paste(data_plot$Confirmed))

ggplot(data_plot,aes(x=State, y = Confirmed)) + geom_bar(stat = "identity") 
#ggsave(paste0("plots/total_confirmed_states.png"))

#########################################################################

# Map Plot

library(sp)
library(RColorBrewer)
ind1=readRDS("data/IND_adm/IND_adm1.rds")

ind1$NAME_1 = as.factor(ind1$NAME_1)
ind1$Confirmed = merge(ind1@data,data_plot,by.x = "NAME_1", by.y = "State",all.x = TRUE)$Confirmed
png("plots/map_plot.png")
spplot(ind1,"Confirmed",main = "Central India and Total Confirmed Cases",
       colorkey=T, scales=list(draw=T))
dev.off()


