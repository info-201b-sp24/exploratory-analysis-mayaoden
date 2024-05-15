library(dplyr)
library(ggplot2)
library(sf)
library(gridExtra)
library(grid)

seattle_crime_dataset <- read.csv("~/Downloads/INFO201/seattle_crime_data.csv")
seattle_crime_dataset$Occurred.Date <- as.Date(seattle_crime_dataset$Occurred.Date, format = "%m/%d/%Y")

merged_data_pre_2008 <- seattle_crime_dataset %>%
  filter(Occurred.Date >= "1975-01-01" & Occurred.Date < "2008-01-01") %>%
  group_by(Beat) %>%
  summarise(total_crimes = n()) %>%
  mutate(prop_crimes = total_crimes / sum(total_crimes)) %>%
  inner_join(st_read("~/Downloads/pre2008/Pre2008_Beats_WM.shp", quiet = TRUE), by = c('Beat' = 'beat'))

merged_data_2008_2015 <- seattle_crime_dataset %>%
  filter(Occurred.Date >= "2008-01-01" & Occurred.Date < "2015-01-01") %>%
  group_by(Beat) %>%
  summarise(total_crimes = n()) %>%
  mutate(prop_crimes = total_crimes / sum(total_crimes)) %>%
  inner_join(st_read("~/Downloads/2008_to_2015/Beats_2008_2015_WM.shp", quiet = TRUE), by = c('Beat' = 'beat'))

merged_data_2015_2017 <- seattle_crime_dataset %>%
  filter(Occurred.Date >= "2015-01-01" & Occurred.Date < "2017-01-01") %>%
  group_by(Beat) %>%
  summarise(total_crimes = n()) %>%
  mutate(prop_crimes = total_crimes / sum(total_crimes)) %>%
  inner_join(st_read("~/Downloads/2015_to_2017/Beats_2015_2017_WM.shp", quiet = TRUE), by = c('Beat' = 'beat'))

merged_data_post_2017 <- seattle_crime_dataset %>%
  filter(Occurred.Date >= "2017-01-01" & Occurred.Date < "2018-01-01") %>%
  group_by(Beat) %>%
  summarise(total_crimes = n()) %>%
  mutate(prop_crimes = total_crimes / sum(total_crimes)) %>%
  inner_join(st_read("~/Downloads/post_2017/Beats_WM.shp", quiet = TRUE), by = c('Beat' = 'beat'))

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

final_plot <- arrangeGrob(plot_pre_2008, plot_2008_2015, plot_2015_2017, plot_post_2017, ncol = 2)
plot_title <- textGrob("Proportion of Crimes per Police Beat in Seattle", gp = gpar(fontface = "bold", fontsize = 15))
final_plot <- arrangeGrob(final_plot, top = plot_title)
plot(final_plot)
