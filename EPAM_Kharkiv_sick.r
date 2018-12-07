library(dplyr)
library(forecast)

setwd("~/R")
path <- "EPAM sickness.csv"
EPAM_sickness <- read.csv(path)

EPAM_sikness <- EPAM_sickness %>%
  select(Kyiv, Lviv, Kharkiv, Dnipro, Vinnytsya)

### Analysis of Kharkiv data set 
# Create a TS
Kharkiv_sick <- ts(EPAM_sickness$Kharkiv, frequency = 52, start=c(2018,1))
tsdisplay(Kharkiv_sick)
#acf(Kharkiv_sick)

# Select proper ARIMA model in auto mode
Kharkiv_model = auto.arima( Kharkiv_sick)
summary(Kharkiv_model)

# Use selected mode to forecast data
Kharkiv_fc <- forecast(Kharkiv_model,h=5)
plot(Kharkiv_fc, col = "red")
grid(lwd=1,lty=1)

# time(Kharkiv_sick)
#deltat(Kharkiv_sick)
#frequency(Kharkiv_sick)
#cycle(Kharkiv_sick)
