library(tidyverse)
library(ggplot2)

fname <- "R_datasets/unemployment_data_us.csv"
unemployment_us <- read.csv(fname, header = TRUE, sep = ",", stringsAsFactors = FALSE)

glimpse(unemployment_us)
summary(unemployment_us)

x <- unemployment_us$Date
date <- as.Date(paste0("01-", x), format = "%d-%b-%Y")
formatted_date <- format(date, "%b-%d-%Y")

ggplot( unemployment_us, aes(x = date)) +
  geom_line( aes(y = Men, color = "Series 1" )) +
  geom_line( aes(y = Women, color = "Series 2" )) +  
  theme(
    panel.grid.major = element_line(color = "black", linetype = "dotted"),
    panel.grid.minor = element_line(color = "black", linetype = "dotted"),
    axis.line = element_line( linewidth = 1)
  ) 
  labs(x = "X-axis label", y = "Y-axis label", color = "Series") +
  scale_color_manual(values = c("Series 1" = "red", "Series 2" = "blue"))

 
  