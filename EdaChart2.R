# Chart 2 - Eda Gokdogan 

rm(list=ls())  
library(ggplot2)
library(dplyr)
crime_data <- read.csv("Crime_Data.csv")
# formatting the function and extracting the month in order to categorize them as seasons 
crime_data$Occurred.Date <- as.character(crime_data$Occurred.Date)
crime_data$Date <- as.Date(crime_data$Occurred.Date, tryFormats = c("%m/%d/%Y"))
crime_data$Month <- format(crime_data$Date, "%m") #extracting the month so that we can categorize it by seasons
crime_data <- na.omit(crime_data)
#creating seasons column
seasons <- function(month) {
  month <- as.numeric(month)  # Convert month to numeric inside the function
  if (month %in% c(12, 01, 02)) {
    return("Winter")
  } else if (month %in% c(03, 04, 05)) {
    return("Spring")
  } else if (month %in% c(06, 07, 08)) {
    return("Summer")
  } else {
    return("Fall")
  }
}
plot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#the graph is not working # combining all and creating one graph  
crime_data$Season <- sapply(crime_data$Month, seasons)
p <- ggplot(crime_data, aes(x = Season, fill = Crime.Subcategory, na.rm=TRUE)) +
  geom_bar(stat = "count", color= "black") +
  ggtitle(label = "Histogram of Crime Rates by Seasons and Types of Crimes") + 
  xlab("Seasons") +
  ylab("Count of Crimes") +
  theme_minimal() + 
  theme(
    legend.position = "right",
    plot.title = element_text(size = 13, face = "bold"),
    axis.title.x = element_text(size =10, face="bold"),
    axis.title.y = element_text(size = 10, face="bold"),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    strip.text.y = element_text(size = 3, face = "bold"),
    legend.title = element_text(size = 6, face = "bold"),
    legend.text = element_text(size = 5)
  )
print(p)

