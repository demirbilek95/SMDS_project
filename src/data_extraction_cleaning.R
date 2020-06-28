library(dplyr)

#import csv and rename columns
covid_19_india <- read.csv("data/datasets_557629_1267081_covid_19_india.csv")
covid_19_india <- rename(covid_19_india, State = State.UnionTerritory)
pop_census <- read.csv("data/datasets_557629_1267081_population_india_census2011.csv")
pop_census <- rename(pop_census, State = State...Union.Territory)
testing_details <- read.csv("data/datasets_557629_1267081_StatewiseTestingDetails.csv")

#filter by state and drop unused factor levels
states <- c("Gujarat","Maharashtra","Madhya Pradesh",
            "Chhattisgarh","Jharkhand","Odisha",
            "West Bengal")
state_filter <- function(data, states){
  filtered_data_set <- 
    data %>% filter(State %in% states)
  return(filtered_data_set)
}
covid_19_india <- droplevels(state_filter(covid_19_india, states))
pop_census <- droplevels(state_filter(pop_census, states))
testing_details <- droplevels(state_filter(testing_details, states))

# Unify Date representation fields in covid_19_inda and testing_details
covid_19_india$Date <- as.Date(covid_19_india$Date, format = "%d/%m/%y")
testing_details$Date <- as.Date(testing_details$Date)

#get date spans
for (s in states){

  cat(sprintf("%s:\n covid_19_india \t%s\n testing_details\t%s\n",
               s,
               paste(min(filter(covid_19_india,State==s)$Date)," - ",
                     max(filter(covid_19_india,State==s)$Date)),
               paste(min(filter(testing_details,State==s)$Date)," - ",
                     max(filter(testing_details,State==s)$Date))))
}

# testing details filling
for (s in states){
  
  #get start/end dates of the two datasets
  sdate_conf = min(filter(covid_19_india,State==s)$Date)
  edate_conf = max(filter(covid_19_india,State==s)$Date)
  sdate_swab = min(filter(testing_details,State==s)$Date)
  edate_swab = max(filter(testing_details,State==s)$Date)
  
  #get the data for interpolating
  temp <- testing_details %>%
    filter(State == s) %>% 
    select(Date,TotalSamples) %>% 
    transmute(x=as.numeric(Date),y=TotalSamples) 
  temp$x = temp$x - temp$x[1] +1
  
  #MODELS
  #interp=lm(y ~ I(x^2), data=temp)
  #interp=lm(y ~ x + I(x^2)+ I(x^3)+ I(x^4) + I(x^5)+ I(x^6), data=temp)
  #interp=nls(y ~ A*exp(B*x)+C, data=temp, start=list(A=1,B=1,C=0))
  #DEBUG
  #summary(interp)
  #plot(temp$x, temp$y)
  #lines(1:max(temp$x),predict(interp,newdata=data.frame(x=1:max(temp$x))),col="red")
  #seq(sdate_conf,len=as.numeric(sdate_swab-sdate_conf),by="day")
  
  #0-fill TotalSamples starting gap
  for(d in seq(sdate_conf,len=as.numeric(sdate_swab-sdate_conf),by="day")){
    newrow=data.frame(
      Date = as.Date(d, origin="1970-01-01") ,
      State=s,
      TotalSamples=0,
      Negative=0,
      Positive=0)
    
    #DEBUG
    #print(newrow)

    testing_details=rbind(testing_details,newrow)
  }
  
  #linearly interpolate other gaps
  for(idx in c(1:temp$x[length(temp$x)])[!1:temp$x[length(temp$x)] %in% temp$x]){
    before =(temp %>% filter(x<idx))[1,]
    after =(temp %>% filter(x>idx))[1,]
    val = as.numeric(before[2]+round((idx-before[1])*(after[2] - before[2])/(after[1] - before[1])))
    newrow=data.frame(
      Date = as.Date(idx-1, origin=sdate_swab),
      State=s,
      TotalSamples=val,
      Negative=NA,
      Positive=NA)
    
    #DEBUG
    #print(newrow)
    
    testing_details=rbind(testing_details,newrow)
  }
  
  
}

#fix pop_census
pop_census<- pop_census %>% 
  select(State, Population, Rural.population, Urban.population,
         Area, Density, Gender.Ratio) %>%
  extract(Area, "Area", "(\\d+,\\d+).*", convert= TRUE) %>%
  extract(Density, "Density", "(.+)/k.*", convert= TRUE) 

# translate strings into numbers eg "12,345" -> num 12345
strcommaToNum <- function(s){
  return(as.numeric(gsub(",","",s)))
}  
pop_census$Density <- strcommaToNum(pop_census$Density)
pop_census$Area <- strcommaToNum(pop_census$Area)

#save files
write.csv(covid_19_india, "data/covid_19_india_filtered.csv")
write.csv(pop_census, "data/pop_census_filtered.csv")
write.csv(testing_details, "data/testing_filtered_filled.csv")
