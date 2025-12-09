############################################ #########################################
############################ Cleaning air pollution data ############################# 
############################################ #########################################

# Goal of this script is to clean the air pollution data for downstream analyses 

### Environment preparation 
rm(list = ls()) # clears the environment
set.seed(2025) # set seed

# Load libraries
libraries <- c('foreign', 'haven', 'dplyr', 'readr') 
invisible(lapply(libraries, require, character.only = T))

# Set working directory
setwd("set_path")

### Load air pollution data for Rotterdam Rijnmond from 2015 to 2021 
# Elemental carbon 
EC_2015 <- read.csv("EC2015.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')
EC_2016 <- read.csv("EC2016.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')
EC_2017 <- read.csv("EC2017.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')
EC_2018 <- read.csv("EC2018.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')
EC_2019 <- read.csv("EC2019.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')
EC_2020 <- read.csv("EC2020.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')
EC_2021 <- read.csv("EC2021.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')

EC_data <- rbind(EC_2015, EC_2016, EC_2017, EC_2018, EC_2019, EC_2020, EC_2021)
EC_data$postcode <- EC_data$Postcode

# PM2.5
PM2.5_2015 <- read.csv("PM2.5_2015.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')
PM2.5_2016 <- read.csv("PM2.5_2016.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')
PM2.5_2017 <- read.csv("PM2.5_2017.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')
PM2.5_2018 <- read.csv("PM2.5_2018.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')
PM2.5_2019 <- read.csv("PM2.5_2019.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')
PM2.5_2020 <- read.csv("PM2.5_2020.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')

PM2.5_data <- rbind(PM2.5_2015, PM2.5_2016, PM2.5_2017, PM2.5_2018, PM2.5_2019, PM2.5_2020)
PM2.5_data$X.1 <- NULL
PM2.5_data$X <- NULL
PM2.5_data$postcode <- PM2.5_data$Postcode

# AQ = O3, NO2, PM10
AQ_2015 <- read.csv("AQ2015.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')
AQ_2016 <- read.csv("AQ2016.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')
AQ_2017 <- read.csv("AQ2017.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')
AQ_2018 <- read.csv("AQ2018.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')
AQ_2019 <- read.csv("AQ2019.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')
AQ_2020 <- read.csv("AQ2020.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')

AQ_data <- rbind(AQ_2015, AQ_2016, AQ_2017, AQ_2018, AQ_2019, AQ_2020)
AQ_data$postcode <- AQ_data$Postcode

# Load file: population per postal code
pop_nrs <- read.csv("Pop_Nrs.csv", header = T, stringsAsFactors = F, check.names = T, sep = ';')
pop_nrs$Year <- pop_nrs$year

### Merge files
air_pollution_merged <- merge(EC_data, PM2.5_data, by = 'date', all.x = T)
air_pollution_data <- merge(air_pollution_merged, AQ_data, by = 'date', all.x = T)

save(air_pollution_data, air_pollution_data, file="raw_air_pollution_data.RData")

# Check structure and fix if needed 
str(air_pollution_data)
air_pollution_data <- air_pollution_data %>% mutate(Date = as.Date(date, format = "%Y%m%d"))
air_pollution_data <- air_pollution_data %>% mutate(across(!c(date, postcode), ~ as.numeric(.)))

# Create 4 digit postal code var (remove final 2 letters)
air_pollution_data$postal_digits <- as.factor(substr(df$postcode, 1, 4))
air_pollution_data <- as.factor(air_pollution_data$postcode)

# Create average for each air pollution var per 4 digit postal code
air_pollution_data <- air_pollution_data %>% group_by(postal_digits) %>% mutate(avg_EC = mean(EC, na.rm = TRUE)) %>% ungroup()
air_pollution_data <- air_pollution_data %>% group_by(postal_digits) %>% mutate(avg_PM2.5 = mean(PM2.5, na.rm = TRUE)) %>% ungroup()
air_pollution_data <- air_pollution_data %>% group_by(postal_digits) %>% mutate(avg_O3 = mean(O3, na.rm = TRUE)) %>% ungroup()
air_pollution_data <- air_pollution_data %>% group_by(postal_digits) %>% mutate(avg_PM10 = mean(PM10, na.rm = TRUE)) %>% ungroup()
air_pollution_data <- air_pollution_data %>% group_by(postal_digits) %>% mutate(avg_NO2 = mean(NO2, na.rm = TRUE)) %>% ungroup()

### Create bins
# Create bins based on quartiles for each air pollution var: O3, PM10, NO2, PM2.5, elemental carbon
air_pollution_data_bins <- air_pollution_data %>% mutate(avg_O3_bin = cut(avg_O3, breaks = quantile(avg_O3, probs = seq(0, 1, 0.25), na.rm = T), labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE))
air_pollution_data_bins <- air_pollution_data %>% mutate(avg_PM10_bin = cut(avg_PM10, breaks = quantile(avg_PM10, probs = seq(0, 1, 0.25), na.rm = T), labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE))
air_pollution_data_bins <- air_pollution_data %>% mutate(avg_NO2_bin = cut(avg_NO2, breaks = quantile(avg_NO2, probs = seq(0, 1, 0.25), na.rm = T), labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE))
air_pollution_data_bins <- air_pollution_data %>% mutate(avg_PM2.5_bin = cut(avg_PM2.5, breaks = quantile(avg_PM2.5, probs = seq(0, 1, 0.25), na.rm = T), labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE))
air_pollution_data_bins <- air_pollution_data %>% mutate(avg_EC_bin = cut(avg_EC, breaks = quantile(avg_EC, probs = seq(0, 1, 0.25), na.rm = T), labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE))

### Save total dataframe
setwd("set_path")
save(pop_nrs, pop_nrs, file="population_per_postal_code.RData")
save(air_pollution_data_bins, air_pollution_data_bins, file="air_pollution_data_bins.RData")
# Contains vars: date (yymmdd), 6 digit postal code, 4 digit postal code, daily value for O3, PM10, NO2, PM2.5, EC, and bins per quartile for each air pollution value

#\ End of script
