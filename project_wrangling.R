#installing packages
library(readr)
library(tidyr)
library(dplyr)
library(readxl)


flight_path <- "/Users/rhino/downloads/flights.csv"
airport_path <- "/Users/rhino/downloads/airports.csv"
airlines_path <- "/Users/rhino/downloads/airlines.csv"

flight_data <- read_csv(flight_path)
length(flight_data) #31 cols
nrow(flight_data) #5,819,079 rows


sapply(flight_data, function(x) sum(is.na (x)))
unique(flight_data$TAIL_NUMBER)

(5819079 - 4755640)/5819079
