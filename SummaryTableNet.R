#this file calculates the table for aggregate crime data

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))

crime_data <- read.csv("crime_data.csv")

#function to calculate overall crime rate
calculate_overall_crime_rate <- function(data) {
  data %>%
    group_by(Year) %>%
    summarise(Overall_Crime_Count = n()) %>%
    arrange(Year)
}

#data for all years
summary_table <- crime_data %>%
  mutate(Occurred.Date = as.Date(Occurred.Date, format = "%m/%d/%Y"),
         Year = as.numeric(format(Occurred.Date, "%Y"))) %>%
  group_by(Year, Primary.Offense.Description) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  spread(key = Primary.Offense.Description, value = Count, fill = 0) %>%
  arrange(Year) %>%
  mutate(Overall_Crime_Count = rowSums(across(where(is.numeric)))) %>%
  select(Year, Overall_Crime_Count, everything()) %>%
  rename_with(~gsub("\\.", " ", .), everything())

#data for years after 2011
summary_table_after_2011 <- crime_data %>%
  mutate(Occurred.Date = as.Date(Occurred.Date, format = "%m/%d/%Y"),
         Year = as.numeric(format(Occurred.Date, "%Y"))) %>%
  filter(Year >= 2011) %>%
  group_by(Year, Primary.Offense.Description) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  spread(key = Primary.Offense.Description, value = Count, fill = 0) %>%
  arrange(Year) %>%
  mutate(Overall_Crime_Count = rowSums(across(where(is.numeric)))) %>%
  select(Year, Overall_Crime_Count, everything()) %>%
  rename_with(~gsub("\\.", " ", .), everything())

#print tables
print(summary_table)
