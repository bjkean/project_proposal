#loading packages
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
library(lubridate)
library(chron)
library(caTools)
library(ROCR)

#setting local paths
flight_path <- "/Users/rhino/downloads/flights.csv"
airport_path <- "/Users/rhino/downloads/airports.csv"
airlines_path <- "/Users/rhino/downloads/airlines.csv"

#reading data from path into Dataframes
flight_data <- read_csv(flight_path)
glimpse(flight_data)
length(flight_data) #31 cols
nrow(flight_data) #5,819,079 rows

#reading airport data
airport <- read_csv(airport_path)
nrow(airport)

#Purging null values for Departure Delay as these won't help in analysis
flight_data <- flight_data %>% 
  filter(!is.na(flight_data$DEPARTURE_DELAY))

#2b purging null values for Arrival Delay as these won't help in analysis
flight_data <- flight_data %>% 
  filter(!is.na(flight_data$ARRIVAL_DELAY))

#creating the flight time
dummy = '00'
flight_data <- flight_data %>% 
  mutate(DEP_TIME_REAL = chron(times = paste(substr(SCHEDULED_DEPARTURE,1,2),substr(SCHEDULED_DEPARTURE,3,4),dummy,sep=":")))

#converting the hour of the flight to a seperate field.
flight_data <- flight_data %>% 
  mutate(Flight_hour =as.numeric(substr(SCHEDULED_DEPARTURE,1,2)))

#setting boolean on delay columns
flight_data <- flight_data  %>% mutate(DEPARTURE_DELAY_BOOL = case_when(DEPARTURE_DELAY  > 0 ~ 1,TRUE ~ 0))
flight_data <- flight_data  %>% mutate(ARRIVAL_DELAY_BOOL = case_when(ARRIVAL_DELAY  > 0 ~ 1,TRUE ~ 0))
flight_data <- flight_data  %>% mutate(AIR_SYSTEM_BOOL = case_when(AIR_SYSTEM_DELAY  > 0 ~ 1,TRUE ~ 0))
flight_data <- flight_data  %>% mutate(SECURITY_DELAY_BOOL = case_when(SECURITY_DELAY  > 0 ~ 1,TRUE ~ 0))
flight_data <- flight_data  %>% mutate(AIRLINE_DELAY_BOOL = case_when(AIRLINE_DELAY  > 0 ~ 1,TRUE ~ 0))
flight_data <- flight_data  %>% mutate(LATE_AIRCRAFT_DELAY_BOOL = case_when(LATE_AIRCRAFT_DELAY  > 0 ~ 1,TRUE ~ 0))
flight_data <- flight_data  %>% mutate( WEATHER_DELAY_BOOL = case_when(WEATHER_DELAY > 0 ~ 1,TRUE ~ 0))

#Changing the name of column in the airport data to match the one in the flight data
airport <- airport %>% rename(ORIGIN_AIRPORT = IATA_CODE)
head(airport)
nrow(flight_data)
flight_data <- semi_join(flight_data, airport)

#getting airport data
flight_data <- inner_join(flight_data, airport)
glimpse(flight_data)

#finding the % of delays for each type of delay
all = sum(flight_data$DEPARTURE_DELAY_BOOL) 
weather = sum(flight_data$WEATHER_DELAY_BOOL) #3%
late = sum(flight_data$LATE_AIRCRAFT_DELAY_BOOL) #26%
airline = sum(flight_data$AIRLINE_DELAY_BOOL) #27%
security = sum(flight_data$SECURITY_DELAY_BOOL) #<1%
system = sum(flight_data$AIR_SYSTEM_BOOL) #27%

#creating time_of_day variable
flight_data <- 
  flight_data  %>% 
  mutate(time_of_day = case_when(Flight_hour %in% c(5,6,7,8,9,10) ~ 'morning',
                                 Flight_hour %in% c(11,12,13,14,15,16) ~ 'afternoon',
                                 Flight_hour %in% c(17,18,19,20,21,22) ~ 'evening',
                                 Flight_hour %in% c(23,0,1,2,3,4) ~ 'overnight'))

#creating season varialbe
flight_data <- 
  flight_data  %>% 
  mutate(season = case_when(MONTH %in% c(12,1,2) ~ 'winter',
                            MONTH %in% c(3,4,5) ~ 'spring',
                            MONTH %in% c(6,7,8) ~ 'summer',
                            MONTH %in% c(9,10,11) ~ 'fall'))


#setting the seed & training set
set.seed(37)
split <- sample.split(flight_data$DEPARTURE_DELAY_BOOL, SplitRatio = 0.25)
flight_train <- subset(flight_data, split == TRUE)
flight_test <- subset(flight_data, split ==FALSE)

#baseline predictions
#predicts that there will be no delays. Accurate 61%
table(flight_train$DEPARTURE_DELAY_BOOL)

#converting these fields to factors since they are numeric but should not be weighted.
flight_train <- flight_train %>% mutate(fact_month = as.factor(MONTH))
flight_train <- flight_train %>% mutate(fact_flight_hour = as.factor(Flight_hour))
flight_train <- flight_train %>% mutate(fact_day_week = as.factor(DAY_OF_WEEK))

glimpse(flight_train)
nrow(flight_train)


#predicting the delays
delayLog <- glm(DEPARTURE_DELAY_BOOL ~ season + time_of_day + DAY_OF_WEEK, 
                family = "binomial", data = flight_train)
summary(delayLog)

predictTrain = predict(delayLog, type="response") #tells predict to give us probability.
summary(predictTrain)

tapply(predictTrain, flight_train$DEPARTURE_DELAY_BOOL, mean) 

tab = table(flight_train$DEPARTURE_DELAY_BOOL, predictTrain > 0.30)
tab
accurarcy = (tab[1,1]+tab[2,2])/(tab[1,1]+tab[2,2]+tab[1,2]+tab[2,1])
delay = tab[2,2]/(tab[2,1]+tab[2,2])
print(paste('accuracy =',accurarcy))
print(paste('delay accuracy=',delay))

rocrpred <- prediction(predictTrain, flight_train$DEPARTURE_DELAY_BOOL)

rocrperf = performance(rocrpred, "tpr","fpr")

plot(rocrperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2, 1.7))
print('all done')

#linear Regression
glimpse(flight_train)
linmod <- lm(ARRIVAL_DELAY ~ ELAPSED_TIME + DISTANCE, data = flight_train)
summary(linmod)


ggplot(flight_train, aes(x = ARRIVAL_DELAY, y = ELAPSED_TIME)) +
  geom_point(shape = 1, alpha = 0.6)

#inster graph plots here

#creating a table to determine when a delay has occured vs when it has not based on time of day
flight_data %>% 
  group_by(time_of_day) %>%
  select(ARRIVAL_DELAY_BOOL) %>% 
  table()

#getting percentages
742411/(1153514+742411)
646063/(808775+646063)
534477/(534477+1284789)
22695/(22695+38406)

#looking at average delay based on time of day when a delay has occured
flight_data %>% 
  group_by(time_of_day, ARRIVAL_DELAY_BOOL) %>% 
  summarise(cnt = mean(ARRIVAL_DELAY)) #%>% 
  filter(ARRIVAL_DELAY_BOOL == 1)#%>% View()

#creating a table to determin if there is a best day of the week to travel
flight_data %>% 
  group_by(DAY_OF_WEEK, ARRIVAL_DELAY_BOOL) %>%  
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(ARRIVAL_DELAY_BOOL==1)

#finding the average delay based on day of the week
flight_data %>% 
  group_by(DAY_OF_WEEK, ARRIVAL_DELAY_BOOL) %>%  
  summarise(n = mean(ARRIVAL_DELAY)) %>% 
  #mutate(freq = n/sum(n)) %>% 
  filter(ARRIVAL_DELAY_BOOL==1)


#looking at airports to avoid, based on top airports in terms of traffic
delays <- flight_data %>% 
  group_by(DESTINATION_AIRPORT, ARRIVAL_DELAY_BOOL) %>% 
  summarise(cnt = n()) %>% 
  mutate(avg_delay = cnt/sum(cnt)) %>% 
  filter(ARRIVAL_DELAY_BOOL==1, cnt>6000)

delays %>% 
  arrange((avg_delay))


#reading in carrier data
AIR_CARRIER <- read_csv(airlines_path)

#getting results when a delay occured
delay_duration <- flight_data %>% 
  select(AIRLINE, ARRIVAL_DELAY, ARRIVAL_DELAY_BOOL) %>%
  filter(ARRIVAL_DELAY_BOOL == 1) %>% 
  group_by(AIRLINE) %>% 
  summarise(avg=mean(ARRIVAL_DELAY)) %>% 
  arrange(desc(avg))

delay_duration <- delay_duration %>% rename(IATA_CODE = AIRLINE)

#joining to get carrier name
carr_delays_durr <- inner_join(delay_duration, AIR_CARRIER)
carr_delays_durr %>% select(AIRLINE, avg) %>% View()
