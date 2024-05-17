suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))

crime_data <- read.csv("crime_data.csv")

# Function to calculate overall crime rate
calculate_overall_crime_rate <- function(data) {
  data %>%
    group_by(Year) %>%
    summarise(Overall_Crime_Count = n()) %>%
    arrange(Year)
}

# Data for years after 2011 with total crime counts
summary_table_after_2011 <- crime_data %>%
  mutate(Year = as.numeric(format(as.Date(Occurred.Date, format = "%m/%d/%Y"), "%Y"))) %>%
  filter(Year >= 2011) %>%
  group_by(Year) %>%
  summarise(Overall_Crime_Count = n()) %>%
  arrange(Year)

# Print the table
print(summary_table_after_2011)
