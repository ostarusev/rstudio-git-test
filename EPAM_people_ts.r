# With library fpp, you can easily create time series with date format: 
# time_ser=ts(data,frequency=4,start=c(1954,2))

library(readr)
library(dplyr)
library( forecast )

# Import Data from CSV file
setwd("~/R")
EPAM_InOut <- read.csv("EPAM In_Out Data - In_Out TS.csv")

# Removing Data column
EPAM_In <- EPAM_InOut %>% 
  select( In )
EPAM_Out <- EPAM_InOut %>% select( Out )

# Convert data to TS
# frequency - data frequency, 1 - year, 12 - month, 4 - quarter
EPAM_In_ts <- ts( EPAM_In, frequency = 12, start=c(2016,1))
EPAM_Out_ts <- ts( EPAM_Out, frequency = 12, start=c(2016,1) )

tsdisplay( EPAM_In_ts )
tsdisplay( EPAM_Out_ts )

EPAM_In_diff <- diff( EPAM_In_ts )
EPAM_Out_diff <- diff( EPAM_Out_ts )
tsdisplay( EPAM_In_diff )

# Plotting the time series
plot( EPAM_In_ts, xlab = "Year", ylab = "People",
      main = "Hiring People since Jan 2016")
plot( EPAM_Out_ts, xlab = "Year", ylab = "People",
      main = "Leaving People since Jan 2016")

# Displaying TS parameters
#time(EPAM_InOut_ts)
#deltat(EPAM_InOut_ts)
#frequency(EPAM_InOut_ts)
#cycle(EPAM_InOut_ts)

x <- auto.arima( EPAM_In_ts)
summary(x)
xp <- forecast( x, h=5 )
plot(xp)

y <- auto.arima( EPAM_Out_ts )
summary(y)
yp <- forecast(y, h=5)
plot(yp)

