# 
# DataLex Project Data Analysis
#
library( tidyr )
library( dplyr )
library( ggplot2 )

setwd("~/R")
path <- "DLEX-ODC UA People - All Data.csv"

# Classes for importing instead of factors
ColClass <- c("character", "character", "character", "character", "character", "character",
              "character", "character", "character", "character", "character", "character", 
              "character",  "numeric", "character", "character", "character")

# Import data from CSV file
DLX_UA_Ppl <- read.csv(path, colClasses = ColClass )

# from string to Date Conversion
DLX_UA_Ppl$StartedIn <- as.Date(DLX_UA_Ppl$StartedIn, "%m/%d/%Y")
DLX_UA_Ppl$EndDate <- as.Date(DLX_UA_Ppl$EndDate, "%m/%d/%Y")

# Active vs. Total per City
x <- DLX_UA_Ppl %>%
  filter( Active == "Yes" ) 
# Years on the Account distribution chart
par( mfrow=c(1,2))
  hist( x$YearsOnAccount, main = "Active: Years on Account Distribution", xlab = "Years on Account", col = "darkblue")
  hist( DLX_UA_Ppl$YearsOnAccount, main = "Total: Years on Account Distribution", xlab = "Years on Account", col = "darkblue")
# Years on the account distribution data  
  YOA_active <- hist( x$YearsOnAccount, plot = FALSE )
  YOA_total <- hist( DLX_UA_Ppl$YearsOnAccount, plot = FALSE )
  Years <- YOA_active$breaks[2:7]
  Active <- YOA_active$counts
  Total <- YOA_total$counts
  YOA_df <- data.frame( Years, Active, Total )
  View(YOA_df)


