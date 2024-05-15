# This is the .R file that calculates summary information to be included in your report

#Summary Info

crime_data <- read.csv("Crime_Data.csv")

library(dplyr)

colnames(crime_data)

#1. Total number of each type of crime + total number of crimes comitted

crime_counts <- crime_data %>%
  group_by(Primary.Offense.Description) %>%
  summarise(total_crimes = n())

total_crimes_committed <- sum(crime_counts$total_crimes)

crime_counts
total_crimes_committed


#2. The precinct that has the highest reported number of crimes

precinct_max_crime <- crime_data %>%
  group_by(Precinct) %>%
  summarise(total_crimes = n()) %>%
  arrange(desc(total_crimes)) %>%
  slice(1)

#3. Most frequent subcategory of crime

most_frequent_subcategory <- crime_data %>%
  group_by(Crime.Subcategory) %>%
  summarise(total_crimes = n()) %>%
  arrange(desc(total_crimes)) %>%
  slice(1)

#4. Most frequent primary offense

most_frequent_primary_offense <- crime_data %>%
  group_by(Primary.Offense.Description) %>%
  summarise(total_crimes = n()) %>%
  arrange(desc(total_crimes)) %>%
  slice(1)

#5. Hour period with the most reports of crime

crime_data$Occurred.Time <- as.POSIXct(crime_data$Occurred.Time, format="%H:%M:%S")
hour_with_most_reports <- crime_data %>%
  group_by(hour = format(Occurred.Time, "%H")) %>%
  summarise(total_reports = n()) %>%
  arrange(desc(total_reports)) %>%
  slice(1)

#Print out results:
cat("Total number of each type of crime:\n")
print(crime_counts)
cat("\nTotal number of crimes committed:", total_crimes_committed)

cat("\n\nPrecinct with the highest reported number of crimes:\n")
print(precinct_max_crime)

cat("\nMost frequent subcategory of crime:\n")
print(most_frequent_subcategory)

cat("\nMost frequent primary offense:\n")
print(most_frequent_primary_offense)

cat("\nHour period with the most reports of crime:\n")
print(hour_with_most_reports)



#Summary Script:
# A function that takes in a dataset and returns a list of summary information:
get_summary_info <- function(crime_data) {
  # Total number of each type of crime
  crime_counts <- crime_data %>%
    group_by(Primary.Offense.Description) %>%
    summarise(total_crimes = n())
  total_crimes_committed <- sum(crime_counts$total_crimes)
  
  # Precinct with the highest reported number of crimes
  precinct_max_crime <- crime_data %>%
    group_by(Precinct) %>%
    summarise(total_crimes = n()) %>%
    arrange(desc(total_crimes)) %>%
    slice(1)
  
  # Most frequent subcategory of crime
  most_frequent_subcategory <- crime_data %>%
    group_by(Crime.Subcategory) %>%
    summarise(total_crimes = n()) %>%
    arrange(desc(total_crimes)) %>%
    slice(1)
  
  # Most frequent primary offense
  most_frequent_primary_offense <- crime_data %>%
    group_by(Primary.Offense.Description) %>%
    summarise(total_crimes = n()) %>%
    arrange(desc(total_crimes)) %>%
    slice(1)
  
  # Hour period with the most reports of crime
  crime_data$Occurred.Time <- as.POSIXct(crime_data$Occurred.Time, format="%H:%M:%S")
  hour_with_most_reports <- crime_data %>%
    group_by(hour = format(Occurred.Time, "%H")) %>%
    summarise(total_reports = n()) %>%
    arrange(desc(total_reports)) %>%
    slice(1)
  
  
  # Constructing the summary list
  summary_info <- list(
    Total_Number_of_Crime_Types = length(unique(crime_data$Primary.Offense.Description)),
    Total_Crimes_Committed = total_crimes_committed,
    Precinct_with_Highest_Crime_Count = as.character(precinct_max_crime$Precinct),
    Most_Frequent_Subcategory_of_Crime = as.character(most_frequent_subcategory$Crime.Subcategory),
    Most_Frequent_Primary_Offense = as.character(most_frequent_primary_offense$Primary.Offense.Description),
    Hour_with_Most_Reports_of_Crime = paste(hour_with_most_reports$hour, ":00-", hour_with_most_reports$hour, ":59", sep="")
  )
  
  return(summary_info)
}

# Usage:
summary_info <- get_summary_info(crime_data)
summary_info

