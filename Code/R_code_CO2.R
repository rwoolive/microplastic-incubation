


rm(list=ls())

# Load necessary libraries
install.packages("tibble")
library(tibble)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("forecast")
library(forecast)
install.packages("tidyr")
library(tidyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("readxl")
library(readxl)
emission_data <- read_excel("C:/UTK one drive/OneDrive - University of Tennessee/Biosystems Engineering/Microplastic Project/Incubation data/Microplastic_CO2_R.xlsx", sheet='Sheet1L')

# Assuming your data is loaded into a data frame named 'emission_data'
# Make sure you have columns 'date', 'co2_emission', 'plastic_type', and 'nitrogen_level'

# Convert 'timestamp' column to a proper datetime format
emission_data$date <- as.POSIXct(emission_data$date)

#emission_data$date <- as.POSIXct(emission_data$date, origin = "03-07-2023") 

# Aggregating data to daily level for each treatment combination
daily_emission <- emission_data %>%
  group_by(date = as.Date(date), plastic_type, nitrogen_level) %>%
  summarize(total_emission = sum(CO2))

# Create time series objects for each treatment combination
ts_list <- list()

for (plastic_type in unique(daily_emission$plastic_type)) {
  for (nitrogen_level in unique(daily_emission$nitrogen_level)) {
    subset_data <- daily_emission %>%
      filter(plastic_type == plastic_type, nitrogen_level == nitrogen_level) %>%
      select(date, total_emission)
    
    ts_data <- ts(subset_data$total_emission, frequency = 365)
    ts_list[[paste(plastic_type, nitrogen_level)]] <- ts_data
  }
}

# Plot the time series for each treatment combination
for (ts_name in names(ts_list)) {
  plot(ts_list[[ts_name]], main = paste("CO2 Emission Time Series for", ts_name))
}

# Fit forecasting models for each treatment combination (using auto.arima as an example)
forecast_results <- list()

for (ts_name in names(ts_list)) {
  fit <- auto.arima(ts_list[[ts_name]])
  forecast_results[[ts_name]] <- forecast(fit, h = 365)
}


# Access and plot the forecasted emissions for each treatment combination
for (ts_name in names(forecast_results)) {
  plot(forecast_results[[ts_name]], main = paste("Forecasted CO2 Emissions for", ts_name))
  # Access forecasted values for each treatment combination
  forecast_values <- forecast_results[[ts_name]]$mean
  # You can save the forecasted values and other relevant information as needed
}
