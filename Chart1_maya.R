# Function to download and unzip a zip file
download_and_unzip <- function(url, dest_dir) {
  temp_zip <- tempfile(fileext = ".zip")
  download.file(url, destfile = temp_zip, mode = "wb")
  unzip(temp_zip, exdir = dest_dir)
  unlink(temp_zip)
}

# URLs of the zip files
urls <- c(
  "https://github.com/info-201b-sp24/exploratory-analysis-mayaoden/raw/d36e264adda57345b75808b8a3efd12fef99fd8b/pre2008.zip",
  "https://github.com/info-201b-sp24/exploratory-analysis-mayaoden/raw/d36e264adda57345b75808b8a3efd12fef99fd8b/2008-2015.zip",
  "https://github.com/info-201b-sp24/exploratory-analysis-mayaoden/raw/d36e264adda57345b75808b8a3efd12fef99fd8b/2015-2017.zip",
  "https://github.com/info-201b-sp24/exploratory-analysis-mayaoden/raw/d36e264adda57345b75808b8a3efd12fef99fd8b/post_2017.zip"
)

# Destination directory for unzipped files
destination_directory <- tempdir()

# Download and unzip each zip file
for (url in urls) {
  download_and_unzip(url, destination_directory)
}

# Load libraries
library(dplyr)
library(ggplot2)
library(sf)
library(gridExtra)
library(grid)

# Read CSV file
seattle_crime_dataset <- read.csv("https://raw.githubusercontent.com/info-201b-sp24/exploratory-analysis-mayaoden/main/Crime_Data.csv")

# Convert date column to Date format
seattle_crime_dataset$Occurred.Date <- as.Date(seattle_crime_dataset$Occurred.Date, format = "%m/%d/%Y")

# Read shapefiles for each time period
merged_data_pre_2008 <- st_read(file.path(destination_directory, "pre2008", "Beats_WM.shp"), quiet = TRUE)
merged_data_2008_2015 <- st_read(file.path(destination_directory, "2008_to_2015", "Beats_WM.shp"), quiet = TRUE)
merged_data_2015_2017 <- st_read(file.path(destination_directory, "2015_to_2017", "Beats_WM.shp"), quiet = TRUE)
merged_data_post_2017 <- st_read(file.path(destination_directory, "post_2017", "Beats_WM.shp"), quiet = TRUE)

# Group and summarize crime data by police beat
merged_data_pre_2008 <- seattle_crime_dataset %>%
  filter(Occurred.Date >= "1975-01-01" & Occurred.Date < "2008-01-01") %>%
  group_by(Beat) %>%
  summarise(total_crimes = n()) %>%
  mutate(prop_crimes = total_crimes / sum(total_crimes)) %>%
  inner_join(merged_data_pre_2008, by = c('Beat' = 'beat'))

merged_data_2008_2015 <- seattle_crime_dataset %>%
  filter(Occurred.Date >= "2008-01-01" & Occurred.Date < "2015-01-01") %>%
  group_by(Beat) %>%
  summarise(total_crimes = n()) %>%
  mutate(prop_crimes = total_crimes / sum(total_crimes)) %>%
  inner_join(merged_data_2008_2015, by = c('Beat' = 'beat'))

merged_data_2015_2017 <- seattle_crime_dataset %>%
  filter(Occurred.Date >= "2015-01-01" & Occurred.Date < "2017-01-01") %>%
  group_by(Beat) %>%
  summarise(total_crimes = n()) %>%
  mutate(prop_crimes = total_crimes / sum(total_crimes)) %>%
  inner_join(merged_data_2015_2017, by = c('Beat' = 'beat'))

merged_data_post_2017 <- seattle_crime_dataset %>%
  filter(Occurred.Date >= "2017-01-01" & Occurred.Date < "2018-01-01") %>%
  group_by(Beat) %>%
  summarise(total_crimes = n()) %>%
  mutate(prop_crimes = total_crimes / sum(total_crimes)) %>%
  inner_join(merged_data_post_2017, by = c('Beat' = 'beat'))

# Create plots
plot_pre_2008 <- ggplot() +
  geom_sf(data = merged_data_pre_2008, aes(geometry = geometry, fill = prop_crimes)) +
  scale_fill_gradient(name = "Proportion of Crimes", low = "lightblue", high = "darkblue") +  
  theme_void() + ggtitle("1975-2007") + theme(plot.title = element_text(hjust = 0.5))

plot_2008_2015 <- ggplot() +
  geom_sf(data = merged_data_2008_2015, aes(geometry = geometry, fill = prop_crimes)) +
  scale_fill_gradient(name = "Proportion of Crimes", low = "lightblue", high = "darkblue") +  
  theme_void() + ggtitle("2008-2015") + theme(plot.title = element_text(hjust = 0.5))

plot_2015_2017 <- ggplot() +
  geom_sf(data = merged_data_2015_2017, aes(geometry = geometry, fill = prop_crimes)) +
  scale_fill_gradient(name = "Proportion of Crimes", low = "lightblue", high = "darkblue") +  
  theme_void() + ggtitle("2015-2017") + theme(plot.title = element_text(hjust = 0.5))

plot_post_2017 <- ggplot() +
  geom_sf(data = merged_data_post_2017, aes(geometry = geometry, fill = prop_crimes)) +
  scale_fill_gradient(name = "Proportion of Crimes", low = "lightblue", high = "darkblue") +  
  theme_void() + ggtitle("2017-2018") + theme(plot.title = element_text(hjust = 0.5))

final_plot <- arrangeGrob(plot_pre_2008
                          