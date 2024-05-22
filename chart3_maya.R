library(ggplot2)
library(dplyr)
library(lubridate)

seattle_crime_dataset <- read.csv("Crime_data.csv")

seattle_crime_dataset$Occurred.Date <- as.Date(seattle_crime_dataset$Occurred.Date, format = "%m/%d/%Y")
seattle_crime_dataset$Reported.Date <- as.Date(seattle_crime_dataset$Reported.Date, format = "%m/%d/%Y")

num_of_crimes_per_month <- seattle_crime_dataset %>%
  mutate(month = month(Occurred.Date, label = TRUE)) %>%
  group_by(month) %>%
  summarise(num_crimes = n()) %>%
  na.omit()

num_of_crimes_reported_per_month <- seattle_crime_dataset %>%
  mutate(month = month(Reported.Date, label = TRUE)) %>%
  group_by(month) %>%
  summarise(num_reported_crimes = n()) %>%
  na.omit()

combined_table <- left_join(num_of_crimes_per_month, num_of_crimes_reported_per_month, by = "month")

# Plotting
plot <- ggplot() +
  geom_point(combined_table, mapping = aes(x = month, y = num_crimes, group = 1, color = "Number of Committed Crimes")) +
  geom_line(combined_table, mapping = aes(x = month, y = num_crimes, group = 1, color = "Number of Committed Crimes")) + 
  geom_point(combined_table, mapping = aes(x = month, y = num_reported_crimes, group = 1, color = "Number of Reported Crimes")) +
  geom_line(combined_table, mapping = aes(x = month, y = num_reported_crimes, group = 1, color = "Number of Reported Crimes")) + 
  theme_minimal() + 
  ggtitle("Number of Committed and Reported Crimes per Month") +
  xlab("Month") +
  ylab("Number of Crimes") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1), 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = c(0.825, 0.92),
        legend.title = element_blank()) 

plot(plot)