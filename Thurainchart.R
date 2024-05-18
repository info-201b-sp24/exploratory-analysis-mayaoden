library(dplyr)
library(ggplot2)
library(lubridate)

# Load the Seattle crime dataset
seattle_crime_dataset <- read.csv("https://raw.githubusercontent.com/info-201b-sp24/exploratory-analysis-mayaoden/main/Crime_Data.csv")

# Convert Occurred.Date to date format
seattle_crime_dataset$Occurred.Date <- as.Date(seattle_crime_dataset$Occurred.Date, format = "%m/%d/%Y")

# Extract the year from Occurred.Date
seattle_crime_dataset$Year <- year(seattle_crime_dataset$Occurred.Date)

# Filter the data for years 2000 to 2020
seattle_crime_dataset_filtered <- seattle_crime_dataset %>%
  filter(Year >= 2000 & Year <= 2020)

# Group the data by Year and Crime.Subcategory and calculate the proportion of each subcategory
crime_subcategory_prop <- seattle_crime_dataset_filtered %>%
  group_by(Year, Crime.Subcategory) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

# Create the stacked bar chart
plot <- ggplot(crime_subcategory_prop, aes(x = factor(Year), y = prop, fill = Crime.Subcategory)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Crime Subcategories (2000-2020)",
       x = "Year",
       y = "Proportion",
       fill = "Crime Subcategory") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


print(plot)