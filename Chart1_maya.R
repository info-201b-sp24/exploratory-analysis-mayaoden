# Necessary Libraries For This Script -------------------------------------
  
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(patchwork)
  library(gridExtra)
  library(grid)

# Functions Created For This Script ---------------------------------------
  
  # Calculates crime counts and proportions
  calculate_crime_props <- function(crime_data, start_date, end_date, beats_shapefile) {
    crimes <- crime_data %>%
      filter(Occurred.Date >= start_date & Occurred.Date < end_date) %>%
      group_by(Beat) %>%
      summarise(total_crimes = n()) %>%
      mutate(prop_crimes = total_crimes / sum(total_crimes))
    
    beats <- st_read(beats_shapefile)
    
    merged_data <- inner_join(crimes, beats, by = c('Beat' = 'beat'))
    
    return(merged_data)
  }
  
  # Creates individual plot
  create_crime_plot <- function(data, title) {
    ggplot() +
      geom_sf(data = data, aes(geometry = geometry, fill = prop_crimes)) +
      scale_fill_gradient(name = "Proportion of Crimes", low = "lightblue", high = "darkblue") +  
      theme_minimal() + ggtitle(title) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5))
  }
  
# Creation of the Figure --------------------------------------------------
  
  # Reading in Seattle crime data
  seattle_crime_dataset <- read.csv("seattle_crime_data.csv")
  
  # Converting Occurred.Date to Date class
  seattle_crime_dataset$Occurred.Date <- as.Date(seattle_crime_dataset$Occurred.Date, format = "%m/%d/%Y")
  
  # Calculate crime counts and proportions for different time periods
  merged_data_pre_2008 <- calculate_crime_props(seattle_crime_dataset, "1975-01-01", "2008-01-01", "~/Downloads/pre2008/Pre2008_Beats_WM.shp")
  merged_data_2008_2015 <- calculate_crime_props(seattle_crime_dataset, "2008-01-01", "2015-01-01", "~/Downloads/2008_to_2015/Beats_2008_2015_WM.shp")
  merged_data_2015_2017 <- calculate_crime_props(seattle_crime_dataset, "2015-01-01", "2017-01-01", "~/Downloads/2015_to_2017/Beats_2015_2017_WM.shp")
  merged_data_post_2017 <- calculate_crime_props(seattle_crime_dataset, "2017-01-01", "2018-01-01", "~/Downloads/post_2017/Beats_WM.shp")
  
  # Create individual plots
  plot_pre_2008 <- create_crime_plot(merged_data_pre_2008, "1975-2007")
  plot_2008_2015 <- create_crime_plot(merged_data_2008_2015, "2008-2015")
  plot_2015_2017 <- create_crime_plot(merged_data_2015_2017, "2015-2017")
  plot_post_2017 <- create_crime_plot(merged_data_post_2017, "2017-2018")
  
  # Merge plots
  final_plot <- grid.arrange(plot_pre_2008, plot_2008_2015, plot_2015_2017, plot_post_2017, ncol = 2)
  plot_title <- textGrob("Proportion of Crimes per Police Beat in Seattle", gp = gpar(fontface = "bold", fontsize = 15))
  final_plot <- arrangeGrob(final_plot, top = plot_title)
  plot(final_plot)