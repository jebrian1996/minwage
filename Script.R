## load data sets
# household.income <- read.csv("Household_Income.csv")
# labor.pop <- read.csv("Labor_Pop.csv")
# minimum.wage <- read.csv("Minimum_Wage.csv")
# unemployment.rates <- read.csv("Unnemployment_Rates.csv")

## subset minimum wage data
# minimum.wage <- subset(minimum.wage, select = (c("Year", "State", "Effective.Minimum.Wage")), subset = 
# (Year >= 1985 & Year <= 2018 & State != "District of Columbia" & State != "Puerto Rico" &
# State != "Guam" & State != "U.S. Virgin Islands"))
# minimum.wage <- minimum.wage[order(minimum.wage$State),]
# rownames(minimum.wage) <- NULL

## compile data sets into single data frame
# data <- data.frame(1:1700)
# data$year <- household.income$years
# data$state <- household.income$state
# data$house.income <- household.income$X.1
# data$labor.pop <- labor.pop$labor.pop
# data$unem.rate <- unemployment.rates$unemployment
# data$min.wage <- minimum.wage$Effective.Minimum.Wage
# data <- subset(data, select = (c("year", "state", "house.income", "labor.pop", "unem.rate", "min.wage")))
# write.csv(data, "Compiled_Data.csv")
library(tidyverse)
data <- read.csv("Compiled_Data.csv")

# create a sequence of elements to run in the following for loops, contents doesn't actually matter just requires
# the number of elements, but 1-50 represents index for troubleshooting
years <- seq(1985, 2018, 1)

num.states <- seq(1, 50, 1)

# set min and max acceptable proportions
min <- .9
max <- 1 / .9


comp <- data.frame(matrix(nrow = 0, ncol = 3))

# first for loop runs fifty times
for (i in years) {
  
  year.sub <- subset(data, year == i)
  
  for (j in num.states) {
    
    
    # because states don't need to be compared twice, starting at x + 1 means each state will be compared to every state
    # after it in the data frame
    for (k in num.states[j + 1: 50]) {
      
      # this if else statement takes the state at the index of the first for loop and compares it to the state at the index 
      # of the second for loop, logical statements ensure that proportion of each covariate for each state is within the 
      # specified min and max range, if they are comparable on each covariate it prints the name of each state to the console
      # I tried to create a string variable and this ifelse statement would add state pairs to the variable and then that 
      # variable would contain all valid comparable states but it kept redefining the variable instead of adding to it, figured
      # printing to the console and recording them manually would be fine
      
      
      
      ifelse(year.sub$unem.rate[j] / year.sub$unem.rate[k] >= min & year.sub$unem.rate[j] / year.sub$unem.rate[k] <= max
             & year.sub$min.wage[j] / year.sub$min.wage[k] >= min & year.sub$min.wage[j] / year.sub$min.wage[k] <= max
             & year.sub$house.income[j] / year.sub$house.income[k] >= min & year.sub$house.income[j] / year.sub$house.income[k] <= max
             & year.sub$labor.pop[j] / year.sub$labor.pop[k] >= min & year.sub$labor.pop[j] / year.sub$labor.pop[k] <= max, 
             comp <- rbind(comp, c(year.sub$year[1], year.sub$state[j], year.sub$state[k])), NA)
    }
  }
}


names(comp) <- c("year", "state1", "state2")
comp <- subset(comp, year != 1990 & year != 1991 & year != 2001 & year != 2008 & year != 2009)
rownames(comp) <- NULL

# write.csv(comp, "Comparable_States.csv")

length <- seq(1, nrow(comp), 1)
comparable <- data.frame(matrix(nrow = 0, ncol = 3))
comp$year <- as.integer(comp$year)

for (i in length) {
  possible.match <- subset(data, (year == comp$year[i] | year == comp$year[i] + 1) & (state == comp$state1[i] | state == comp$state2[i]))
  ifelse(((possible.match$min.wage[2] - possible.match$min.wage[1]) > 0 & possible.match$min.wage[4] == possible.match$min.wage[3]) | 
           ((possible.match$min.wage[4] - possible.match$min.wage[3]) > 0 & possible.match$min.wage[2] == possible.match$min.wage[1]),
         comparable <- rbind(comparable, c(possible.match$year[1], possible.match$state[1], possible.match$state[3])), NA)
  
}
colnames(comparable) <- c("year", "state1", "state2")


for (i in 1:nrow(comparable)) {
  matched <- subset(data, (year == as.integer(comparable$year[i]) | year == as.integer(comparable$year[i]) + 1) & 
                      (state == comparable$state1[i] | state == comparable$state2[i]))
  
  comparable$change.min.wage[i] <- max(matched$min.wage[2] - matched$min.wage[1], matched$min.wage[4] - matched$min.wage[3])
  
  place.holder <- matched
  
  comparable$state1[i] <- ifelse((matched$min.wage[2] - matched$min.wage[1] == 0), 
                                 place.holder$state[3], place.holder$state[1])
  
  comparable$state2[i] <- ifelse((matched$min.wage[2] - matched$min.wage[1] == 0), 
                                 place.holder$state[1], place.holder$state[3])
}





# write.csv(comparable, "Comparable_States.csv")

comparable2 <- data.frame(matrix(nrow = 0, ncol = 4))
comparable$year<- as.integer(comparable$year)

for (i in 1:nrow(comparable)) {
  possible.match <- subset(data, (year == comparable$year[i] | year == comparable$year[i] + 1) & 
                             (state == comparable$state1[i] | state == comparable$state2[i]))
  
  
  ifelse(((possible.match$labor.pop[2] - possible.match$labor.pop[1]) > 0 & possible.match$labor.pop[4] > possible.match$labor.pop[3]) | 
           ((possible.match$labor.pop[4] - possible.match$labor.pop[3]) < 0 & possible.match$labor.pop[2] < possible.match$labor.pop[1]) &
           ((possible.match$unem.rate[2] - possible.match$unem.rate[1]) > 0 & possible.match$unem.rate[4] > possible.match$unem.rate[3]) | 
           ((possible.match$unem.rate[4] - possible.match$unem.rate[3]) < 0 & possible.match$unem.rate[2] < possible.match$unem.rate[1]) &
           ((possible.match$house.income[2] - possible.match$house.income[1]) > 0 & possible.match$house.income[4] > possible.match$house.income[3]) | 
           ((possible.match$house.income[4] - possible.match$house.income[3]) < 0 & possible.match$house.income[2] < possible.match$house.income[1]),
         comparable2 <- rbind(comparable2, c(possible.match$year[1], possible.match$state[1], possible.match$state[3], round(comparable$change.min.wage[i], 2))), NA)
}


colnames(comparable2) <- c("year", "state1", "state2", "change.min.wage")
comparable2 <- subset(comparable2, year <= 2016)


final.after <- data.frame(matrix(nrow = nrow(comparable2), ncol = 10))
colnames(final.after) <- c("year", "state1", "state1.min.wage", "state1.unem.rate", "state1.house.income",
                           "state2", "state2.min.wage", "state2.unem.rate", "state2.house.income", "change.min.wage")
final.after$year <- as.integer(comparable2$year) + 2
final.after$state1 <- comparable2$state1
final.after$state2 <- comparable2$state2
for (i in 1:nrow(final.after)) {
  final.after$state1.house.income[i] <- data$house.income[data$year == final.after$year[i] & data$state == final.after$state1[i]]
  final.after$state1.min.wage[i] <- data$min.wage[data$year == final.after$year[i] & data$state == final.after$state1[i]]
  final.after$state1.unem.rate[i] <- data$unem.rate[data$year == final.after$year[i] & data$state == final.after$state1[i]]
  
  final.after$state2.house.income[i] <- data$house.income[data$year == final.after$year[i] & data$state == final.after$state2[i]]
  final.after$state2.min.wage[i] <- data$min.wage[data$year == final.after$year[i] & data$state == final.after$state2[i]]
  final.after$state2.unem.rate[i] <- data$unem.rate[data$year == final.after$year[i] & data$state == final.after$state2[i]]
  
  final.after$change.min.wage[i] <- comparable2$change.min.wage[i]
  
}


final.before <- data.frame(matrix(nrow = nrow(comparable2), ncol = 9))
colnames(final.before) <- c("year", "state1", "state1.min.wage", "state1.unem.rate", "state1.house.income",
                            "state2", "state2.min.wage", "state2.unem.rate", "state2.house.income")
final.before$year <- as.integer(comparable2$year)
final.before$state1 <- comparable2$state1
final.before$state2 <- comparable2$state2

for (i in 1:nrow(final.before)) {
  final.before$state1.house.income[i] <- data$house.income[data$year == final.before$year[i] & data$state == final.before$state1[i]]
  final.before$state1.min.wage[i] <- data$min.wage[data$year == final.before$year[i] & data$state == final.before$state1[i]]
  final.before$state1.unem.rate[i] <- data$unem.rate[data$year == final.before$year[i] & data$state == final.before$state1[i]]
  
  final.before$state2.house.income[i] <- data$house.income[data$year == final.before$year[i] & data$state == final.before$state2[i]]
  final.before$state2.min.wage[i] <- data$min.wage[data$year == final.before$year[i] & data$state == final.before$state2[i]]
  final.before$state2.unem.rate[i] <- data$unem.rate[data$year == final.before$year[i] & data$state == final.before$state2[i]]
  
  
}

# write.csv(final.after, "states_after_treatment.csv")
# write.csv(final.before, "states_before_treatment.csv")

# dataBefore <- read.csv("states_before_treatment.csv")
# dataAfter <- read.csv("states_after_treatment.csv")
dataBefore <- final.before
dataAfter <- final.after

ATEstate1 <- dataAfter[1:nrow(dataAfter), 5:6] - dataBefore[1:nrow(dataBefore), 5:6]
ATEstate2 <- dataAfter[1:nrow(dataAfter), 9:10] - dataBefore[1:nrow(dataBefore), 9:10]
combineState1 <- cbind(dataAfter[ , 1:3], ATEstate1)
combineAll <- cbind(combineState1, dataAfter$state2, ATEstate2)
colnames(combineAll)[6] <- "state2"

#difference in difference
difference <- combineAll[ , 7:8] - combineAll[ , 4:5]
colnames(difference) <- c("difference.unem.rate", "difference.house.income")
difference$change.min.wage <- as.numeric(final.after$change.min.wage)

#plot data
ggplot(difference, aes(x = change.min.wage, y = difference.unem.rate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(x = "Change in Minimum Wage", y = "Change in Unemployment Rate", title = "Difference in Differences for Unemployment Rate")

ggplot(difference, aes(x = change.min.wage, y = difference.house.income)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(x = "Change in Minimum Wage", y = "Change in Median Household Income", title = "Difference in Differences for Median Household Income")

# plot dependent variables
states <- unique(c(final.after$state1, final.after$state2))
used.states <- subset(data, subset = (data$state %in% states))
ggplot(used.states, aes(x = year, y = house.income, color = state)) +
  geom_line() +
  labs(x = "Year", y = "Median Household Income", title = "Median Household Income of Used States Over Time")

ggplot(used.states, aes(x = year, y = unem.rate, color = state)) +
  geom_line() +
  labs(x = "Year", y = "State Unemployment Rate", title = "Unemployment Rate of Used States Over Time")


# look at outer values and see what could be causing them
# run again at a different threshold and mention comparison
# mention where data comes from but work with before and after treatment files