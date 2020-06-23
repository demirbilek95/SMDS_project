# POISSON MODEL (SIMPLE CASE)

library(tidyr)
library(MASS) 

# load datasets
covid_19_india <- read.csv('~/Documents/Statistical_method_data_science/smds_exercises/SMDS_project/data/re_arranged_covid_19_india_daily_infected_filtered.csv', row.names = 1)
testing <- read.csv('~/Documents/Statistical_method_data_science/smds_exercises/SMDS_project/data/testing_filtered.csv', row.names = 1)

# solve conflict lengths among responce and covariates: confirmed cases start
# one mounth before store the negative and positive in testing dataset (Maharashtra)
start <- length(covid_19_india$Maharashtra) - 
  length(testing$Negative[testing$State == "Maharashtra"])

zeros <- rep(0, start) # used to fill the first negative and positive with zeros
negative <- testing$Negative[testing$State == "Maharashtra"]
for (v in zeros){negative <- c(v, negative)}

positive <- testing$Positive[testing$State == "Maharashtra"]
for (v in zeros){positive <- c(v, positive)}

# define train and test sets
set.seed(123)
smp_size <- floor(0.75 * length(covid_19_india$Maharashtra)) # 75% of the sample size
train_ind <- sample(seq_len(length(covid_19_india$Maharashtra)), size = smp_size)

# responce partition
train_y <- covid_19_india$Maharashtra[train_ind]
test_y <- covid_19_india$Maharashtra[-train_ind]

# covariates partition
train_x1 <- negative[train_ind]
test_x1 <- negative[-train_ind]

train_x2 <- positive[train_ind]
test_x2 <- positive[-train_ind]

# make the poisson model
data = data.frame(y = train_y, negative = train_x1, positive = train_x2)
poisson_model <- glm(data = data, y ~ negative + positive, family = poisson(link = "log"))
summary(poisson_model)

# make prediction
newdata = data.frame(negative = test_x1, positive = test_x2)
predict(poisson_model, newdata = newdata)

# compute informatice plot
library(jtools)
library(ggstance)
library(broom)

plot_summs(poisson_model, scale = TRUE, exp = TRUE)
