# Summary Info:
suppressPackageStartupMessages(library(dplyr))

# Read crime data from a CSV file
crime_data <- read.csv("Crime_Data.csv")

# Calculate total number of each type of crime and total crimes committed
crime_counts <- crime_data %>% count(Primary.Offense.Description)
total_crimes_committed <- sum(crime_counts$n)

# Precinct with the highest reported number of crimes
precinct_max_crime <- crime_data %>% 
  count(Precinct) %>% 
  filter(n == max(n))

# Most frequent subcategory of crime
most_frequent_subcategory <- crime_data %>% 
  count(Crime.Subcategory) %>% 
  filter(n == max(n))

# Most frequent primary offense
most_frequent_primary_offense <- crime_data %>% 
  count(Primary.Offense.Description) %>% 
  filter(n == max(n))

# Hour period with the most reports of crime
hour_with_most_reports <- crime_data %>%
  mutate(Hour = format(as.POSIXct(Occurred.Time, format="%H:%M:%S"), "%H")) %>%
  count(Hour) %>%
  filter(n == max(n)) %>%
  mutate(Hour_with_Most_Reports_of_Crime = paste(Hour, ":00-", Hour, ":59", sep=""))

# Print summary information
cat("Total Number of Crime Types: ")
cat(nrow(crime_counts), "\n\n")

cat("Total Number of Crimes Committed: ")
cat(total_crimes_committed, "\n\n")

cat("Precinct with the Highest Crime Count: ")
cat(precinct_max_crime$Precinct, "\n\n")

cat("Most Frequent Subcategory of Crime: ")
cat(most_frequent_subcategory$Crime.Subcategory, "\n\n")

cat("Most Frequent Primary Offense: ")
cat(most_frequent_primary_offense$Primary.Offense.Description, "\n\n")

cat("Hour with Most Reports of Crime: ")
cat(hour_with_most_reports$Hour_with_Most_Reports_of_Crime, "\n")
