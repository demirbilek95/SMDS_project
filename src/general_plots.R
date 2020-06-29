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
colnames(data_plot)
data_plot[2,] <- colnames # Adding state names as row

data_plot <- data.frame(t(data_plot))
data_plot <- rename(data_plot, State = "X2",Confirmed = "X105")
data_plot$Confirmed <- as.numeric(paste(data_plot$Confirmed))
data_plot$Confirmed_perc <- paste(round((data_plot$Confirmed/sum(data_plot$Confirmed))*100, 2), "%")
data_plot$Abbreviation <- c("CG", "GJ", "JH", "MP", "MH", "OD", "WB")

ggplot(data_plot,aes(x=State, y = Confirmed, fill = State)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = Confirmed_perc), vjust = -0.25) +
  theme(legend.position="none") +
  labs(title = "Confirmed cases in central India")

#ggsave(paste0("plots/total_confirmed_states.png"))

#########################################################################

# Map Plot

library(sp)
library(RColorBrewer)
ind1=readRDS("data/IND_adm/IND_adm1.rds")

ind1$NAME_1 = as.factor(ind1$NAME_1)
ind1$Confirmed = merge(ind1@data, data_plot, by.x = "NAME_1", by.y = "State",all.x = TRUE)$Confirmed
png("plots/map_plot.png")

spplot(ind1,"Confirmed", main = "Central India and Total Confirmed Cases",
       colorkey=T, scales=list(draw=T))

dev.off()

#####

my.palette <- brewer.pal(n = 7, name = "Reds")

name_state <- function(x,y,z,subscripts,...){
  
  # used to select just the used states, so filter the levels
  filter_state <- droplevels(ind1$NAME_1[!is.na(ind1$Confirmed)], 
                             exclude = if(anyNA(levels(ind1$NAME_1[!is.na(ind1$Confirmed)]))) NULL else NA)
  
  panel.polygonsplot(x,y,z,subscripts,...)
  sp.text(coordinates(ind1[!is.na(ind1$Confirmed),]), 
          paste(data_plot$Abbreviation[unique(data_plot$State) == filter_state],
                "\n",
                data_plot$Confirmed_perc)) 
}

png("plots/map_plot.png")

spplot(ind1, "Confirmed", col.regions = my.palette, cuts = 6,
       panel = name_state, colorkey=T, scales=list(draw=T))

dev.off()

# same map plot but with just the names...
sp.label <- function(x, label) {
  list("sp.text", coordinates(x), label)
}

NAME.sp.label <- function(x) {
  
  # select names states
  filter_state <- ind1$NAME_1[!is.na(ind1$Confirmed)]
  filter_state <- droplevels(filter_state, 
                             exclude = if(anyNA(levels(ind1$NAME_1[!is.na(ind1$Confirmed)]))) NULL else NA)
  states <- unlist(as.list(levels(data_plot$State)),recursive=FALSE)
  
  # select rows of our states and names of states
  sp.label(subset(ind1, NAME_1 %in% states), filter_state)
}

draw.sp.label <- function(x) {
  do.call("list", NAME.sp.label(x))
}

spplot(ind1, 'Confirmed', col.regions = my.palette, 
       cuts = 6, colorkey=T, sp.layout = draw.sp.label(ind1))
