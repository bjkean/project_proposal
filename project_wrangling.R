#installing packages
library(readr)
library(tidyr)
library(dplyr)
library(readxl)

#setting local paths
flight_path <- "/Users/rhino/downloads/flights.csv"
airport_path <- "/Users/rhino/downloads/airports.csv"
airlines_path <- "/Users/rhino/downloads/airlines.csv"

#reading in data
flight_data <- read_csv(flight_path)
length(flight_data) #31 cols
nrow(flight_data) #5,819,079 rows

#checking for null values. Null values in the Deparature Delay column exist in 
sapply(flight_data, function(x) sum(is.na (x))) #86,153 records

#2 purging null values for Departure Delay as these won't help in analysis
flight_data <- flight_data %>% 
  filter(!is.na(flight_data$DEPARTURE_DELAY))

#checking row numbers 
nrow(flight_data) #5,732,926 (==5819079 original - 86153 NULL in Dep Delay)

colnames(flight_data)

#converting cancel cols to a table to be used for lu
vec1 <- c("A","B","C","D")
vec2 <- c("Airline/Carrier","Weather","National Air System","Security")
mat <- matrix(c(vec1, vec2), ncol = 2)
as.table(mat)
mat <- as.data.frame(mat)

#renaming col names
colnames(mat) <- c("CANCELLATION_REASON","can_value")
mat

flight_data <- left_join(flight_data, mat, by = "CANCELLATION_REASON")

#reading in airline data
airline_data <- read_csv(airlines_path)
airline_data

#changing column name in flight data to match that in airlines data
colnames(flight_data)[colnames(flight_data) =="AIRLINE"] <- 'IATA_CODE'

#connecting flight data to airline data
flight_data <- inner_join(flight_data, airline_data, by = "IATA_CODE")
nrow(flight_data)


#Setting a boolean that there was a delay or NOT
delay_vec <- flight_data$ARRIVAL_DELAY > 0


bind_cols(flight_data,delay_vec)

length(flight_data_5)
for (num in 1:length(flight_delay_4)){
  if (flight_delay_4$BOOL_DELAY[num]==TRUE){flight_delay_4[num]=1}
  else{flight_data_4$BOOL_DELAY[num]=0}
}

length(flight_data_4)
