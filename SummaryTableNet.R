# Load libraries
library(dplyr)
library(tidyr)

# Read the data
crime_data <- read.csv("crime_data.csv")

# Process data and calculate summary table
summary_table <- crime_data %>%
  mutate(Year = as.numeric(format(as.Date(Occurred.Date, "%m/%d/%Y"), "%Y"))) %>%
  count(Year, Primary.Offense.Description) %>%
  pivot_wider(names_from = Primary.Offense.Description, values_from = n, values_fill = 0) %>%
  mutate(
    # Calculate overall crime count
    Overall_Crime_Count = rowSums(across(where(is.numeric)))
  ) %>%
  

  select(Year, Overall_Crime_Count, everything()) %>%
  rename_all(~gsub("\\.", " ", .)) %>%
  select(Year, Overall_Crime_Count, everything()) %>% # Keep Year and Overall_Crime_Count columns at the beginning
  select(1:2, order(colnames(.))) # Reorganize columns alphabetically starting from the third column

# Print the summary table
print(summary_table)

