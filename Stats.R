#installing packages
library(readr)
library(tidyr)
library(dplyr)
library(readxl)
library(lubridate)
library(chron)
library(ggplot2)

#setting local paths
flight_path <- "/Users/rhino/downloads/flights.csv"
airport_path <- "/Users/rhino/downloads/airports.csv"
airlines_path <- "/Users/rhino/downloads/airlines.csv"

#reading in data
flight_data <- read_csv(flight_path)
length(flight_data) #31 cols
nrow(flight_data) #5,819,079 rows

#reading airport data
airport <- read_csv(airport_path)
nrow(airport)

#2a purging null values for Departure Delay as these won't help in analysis
flight_data <- flight_data %>% 
  filter(!is.na(flight_data$DEPARTURE_DELAY))

#2b purging null values for Arrival Delay as these won't help in analysis
flight_data <- flight_data %>% 
  filter(!is.na(flight_data$ARRIVAL_DELAY))

#3 purging all airports with numeric values
airport <- airport %>% rename(DESTINATION_AIRPORT = IATA_CODE)
head(airport)
nrow(flight_data)
flight_data <- semi_join(flight_data, airport)

#4 setting aiprots with less than 3650 flights (10 per day)
airport_cnts <- 
flight_data %>% 
  group_by(ORIGIN_AIRPORT) %>% 
  summarise(cnt = n()) %>% 
  filter(cnt>10000)

#5 filter airports that did NOT make the cut above
flight_data <- semi_join(flight_data, airport_cnts)
nrow(flight_data) #4,973,276

#Getting 10% sample
flight_sample <- sample_n(flight_data,as.integer(nrow(flight_data)*0.10))
nrow(flight_sample) #497,327

#reviewing data setup
glimpse(flight_sample)

#setting boolean on delay columns
flight_sample <- 
flight_sample %>% 
  mutate(ARRIVAL_BOOL_DELAY = case_when(ARRIVAL_DELAY > 0 ~ 1,
                                        TRUE ~ 0))

flight_sample <- 
  flight_sample %>% 
  mutate(DEPARTURE_BOOL_DELAY = case_when(DEPARTURE_DELAY > 0 ~ 1,
                                        TRUE ~ 0))

#Percent, by airport, for delays
flight_sample %>% 
  group_by(DESTINATION_AIRPORT) %>% 
  summarise(Percent_Delay = (sum(ARRIVAL_BOOL_DELAY)/n()), cnt=n()) %>% 
  arrange(desc(Percent_Delay))

#Avg Delay time when BOOL == 1
flight_sample %>% 
  group_by(DESTINATION_AIRPORT, ARRIVAL_BOOL_DELAY) %>% 
  summarise(Average_Delay = (mean(ARRIVAL_DELAY)), cnt=n()) %>% 
  filter(ARRIVAL_BOOL_DELAY==1) %>%
  arrange(desc(Average_Delay))

#percent delay by airport ARRIVAL
flight_sample %>% 
  #filter(DESTINATION_AIRPORT == "MSP") %>% 
  #group_by(DESTINATION_AIRPORT) %>% 
  group_by(Flight_hour) %>% 
  summarise(Percent_Delay = (sum(ARRIVAL_DELAY>0)/n()), cnt=n()) %>% 
  View()
glimpse(flight_sample)

  #percent delay by airport DEPARTURE
flight_sample %>% 
    #filter(DESTINATION_AIRPORT == "MSP") %>% 
    #group_by(DESTINATION_AIRPORT) %>% 
  group_by(DEPARTURE_BOOL_DELAY) %>% 
  summarise(Percent_Delay = (sum(DEPARTURE_DELAY>0)/n()), cnt=n())  


  
glimpse(flight_sample)
#creating the time variable b/c chron needs colon & seconds
#adding dummy
dummy = '00'
flight_sample <- flight_sample %>% 
  mutate(DEP_TIME_REAL = chron(times = paste(substr(SCHEDULED_DEPARTURE,1,2),substr(SCHEDULED_DEPARTURE,3,4),dummy,sep=":")))

flight_sample <- flight_sample %>% 
  mutate(Flight_hour =as.numeric(substr(SCHEDULED_DEPARTURE,1,2)))

glimpse(flight_sample)

#Does Size of Airport impact delays? No
vizSample1 <- flight_sample %>% 
  group_by(DESTINATION_AIRPORT) %>% 
  summarise(Percent_Delay = (sum(ARRIVAL_DELAY>0)/n()), cnt=n())

ggplot(vizSample1, aes(x = Percent_Delay, y = cnt)) +
  geom_point()

#Does Time of day impact delays? Seems like it...
vizSample2 <- flight_sample %>% 
  group_by(Flight_hour) %>% 
  summarise(Percent_Delay = (sum(ARRIVAL_DELAY>0)/n()), cnt=n())

View(vizSample2)
ggplot(vizSample2, aes(x = Flight_hour, y = Percent_Delay)) +
  geom_point()

glimpse(vizSample2)

vizSample3 <- flight_sample %>% 
  group_by(Flight_hour) %>% 
  filter(Flight_hour>3) %>% 
  summarise(Percent_Delay = (sum(ARRIVAL_DELAY>0)/n()), cnt=n())
#delete
c <- lm(formula=Flight_hour ~ Percent_Delay, data=vizSample3)
summary(c)

d <- glm(Flight_hour ~ binomial(DEPARTURE_BOOL_DELAY))


mean_data <- flight_sample %>% 
  filter(ARRIVAL_BOOL_DELAY==1)

summary(mean_data$ARRIVAL_DELAY)
nrow(mean_data)/nrow(flight_sample)

ggplot(mean_data, aes(ARRIVAL_DELAY)) +
  geom_histogram(binwidth = 75)
View(mean_data)

sd(mean_data$ARRIVAL_DELAY)
