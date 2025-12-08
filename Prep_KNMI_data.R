############################################ #########################################
###################### Cleaning temperature / KNMI weather data ###################### 
############################################ #########################################

# Goal of this script is to clean the 'temperature' variable for downstream analyses. 

### Step 1: load monitors for each year (2015 until 2021) and clean data 
rm(list = ls()) # clears the environment
set.seed(2025) # set seed

# Load libraries
libraries <- c('foreign', 'haven', 'dplyr') 

invisible(lapply(libraries, require, character.only = T))

# Set working directory
setwd("set_path")

# Download data 2015-2021, select all monitors and var 'TG' at https://daggegevens.knmi.nl/klimatologie/daggegevens (openly available)
# Selected variable, X = TG: Etmaalgemiddelde temperatuur (in 0.1 graden Celsius)
# Load data for each KNMI monitor for each year 
monitor_2015 <- read.table('temperatuur_2015.txt', header = T, sep = ",", colClasses = 'character', strip.white = T, na.strings = "") # strip.white = REMOVE WHITE SPACE, na.string set NA in empty spaces
monitor_2016 <- read.table('temperatuur_2016.txt', header = T, sep = ",", colClasses = 'character', strip.white = T, na.strings = "")
monitor_2017 <- read.table('temperatuur_2017.txt', header = T, sep = ",", colClasses = 'character', strip.white = T, na.strings = "")
monitor_2018 <- read.table('temperatuur_2018.txt', header = T, sep = ",", colClasses = 'character', strip.white = T, na.strings = "")
monitor_2019 <- read.table('temperatuur_2019.txt', header = T, sep = ",", colClasses = 'character', strip.white = T, na.strings = "")
monitor_2020 <- read.table('temperatuur_2020.txt', header = T, sep = ",", colClasses = 'character', strip.white = T, na.strings = "")
monitor_2021 <- read.table('temperatuur_2021.txt', header = T, sep = ",", colClasses = 'character', strip.white = T, na.strings = "")

# Fix structure of monitor (from chr to factor)
make_factor <- function(df, column) {
  df[[column]] <- as.factor(df[[column]])
  return(df)
}

monitor_2015 <- make_factor(monitor_2015, "X209")
monitor_2016 <- make_factor(monitor_2016, "X209")
monitor_2017 <- make_factor(monitor_2017, "X209")
monitor_2018 <- make_factor(monitor_2018, "X209")
monitor_2019 <- make_factor(monitor_2019, "X209")
monitor_2020 <- make_factor(monitor_2020, "X209")
monitor_2021 <- make_factor(monitor_2021, "X209")

# Convert date to vector
date_vector <- function(df, date_var) {
  df$date <- as.Date(df[[date_var]], format = "%Y%m%d")
  df[[date_var]] <- NULL
  return(df)
}

monitor_2015 <- date_vector(monitor_2015, "X20150101")
monitor_2016 <- date_vector(monitor_2016, "X20160101")
monitor_2017 <- date_vector(monitor_2017, "X20170101")
monitor_2018 <- date_vector(monitor_2018, "X20180101")
monitor_2019 <- date_vector(monitor_2019, "X20190101")
monitor_2020 <- date_vector(monitor_2020, "X20200101")
monitor_2021 <- date_vector(monitor_2021, "X20210101")

# Create temperature variables that are in Celsius instead of 0.1 Celsius
clean_monitor <- function(df) {
  df <- df %>%
    mutate_at(vars(-c("X209", "date")), list(~as.numeric(.))) %>%
    mutate(avg_temp = X / 10) %>%
    select(-X)
  return(df)
}

monitor_2015 <- clean_monitor(monitor_2015)
monitor_2016 <- clean_monitor(monitor_2016)
monitor_2017 <- clean_monitor(monitor_2017)
monitor_2018 <- clean_monitor(monitor_2018)
monitor_2019 <- clean_monitor(monitor_2019)
monitor_2020 <- clean_monitor(monitor_2020)
monitor_2021 <- clean_monitor(monitor_2021)

# Rename station variable 
monitor_2015 <- monitor_2015 %>% rename(station = X209)
monitor_2016 <- monitor_2016 %>% rename(station = X209)
monitor_2017 <- monitor_2017 %>% rename(station = X209)
monitor_2018 <- monitor_2018 %>% rename(station = X209)
monitor_2019 <- monitor_2019 %>% rename(station = X209)
monitor_2020 <- monitor_2020 %>% rename(station = X209)
monitor_2021 <- monitor_2021 %>% rename(station = X209)

# Merge dataframes into final cleaned weather dataset
weather_data <- rbind(monitor_2015, monitor_2016, monitor_2017, monitor_2018, monitor_2019, monitor_2020, monitor_2021)

# Number of records per weather station for 2015 to 2021
NumRecords_w <- weather_data %>% group_by(station) %>% summarise(NumRecords= sum(station==station))

### Step 2: Create bins 
# Create the following bins to ensure comparability to prior studies: < -4 °C, -4 – 0 °C, 0 – 4 °C, 4 – 8 °C, 8 – 12 °C, 12 – 16 °C, 16 – 20 °C, and > 20 °C
weather_data_bins <- weather_data %>% mutate(temp_bin = cut(avg_temp, breaks = c(-Inf, -4, 0, 4, 8, 12, 16, 20, Inf), labels = c("< -4", "-4 to 0", "0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "> 20"), right = TRUE, include.lowest = TRUE))

# Check distribution temperature 
hist(weather_data_bins$avg_temp) # normally distributed 

### Step 3: save final files
save(weather_data_bins, weather_data_bins, file="weather_data.RData")

# This df contains the following vars: station = KNMI weather monitor station, date = date (yymmdd), avg.temperature = average daily temperature, temp_bin = the cut-off in which the specific temperature lines. 

#\ End of script
