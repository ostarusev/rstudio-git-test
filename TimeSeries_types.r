library( forecast )
library( stats )
library(ggplot2)

# White Noice
white_noise <- arima.sim(model = list(order = c(0,0,0)), n=100)
tsdisplay(white_noise)

# Random Walk
random_walk <- arima.sim(model = list(order = c(0, 1, 0)), n = 100)
tsdisplay(random_walk)

# Auto Regression
#AR <- arima.sim(model = list(order = c(1, 0, 0)), n = 100)
AR <- arima.sim(model = list(ar = -0.5), n = 100)
tsdisplay(AR)

# Meaning Average
#meaning_av <- arima.sim(model = list(order = c(0,0,1)), n=100)
MA <- arima.sim(model = list(ma = 0.5), n = 100)
tsdisplay( MA )

y <- auto.arima(AR)
summary(y)
plot(y)

# Extra investigation of 


AR1 <- arima.sim(model = list(ar = 0.5), n = 100)
AR2 <- arima.sim(model = list(ar = 0.9), n = 100)
AR3 <- arima.sim(model = list(ar = -0.5), n = 100)
AR4 <- arima.sim(model = list(ar = -0.9), n = 100)
acf(AR1)
acf(AR2)
acf(AR3)
acf(AR4)
