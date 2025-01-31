# Bike Buyers Script
# From Kaggle
# 8/7/2023

library(tidyverse)
library(ggplot2)
library(GGally)

# read data from file
bike_buyers <- read.csv( "R_datasets/bike_buyers.csv", header = TRUE, na.strings = '', stringsAsFactors = TRUE )

# Display top records
head(bike_buyers)

# Show up class of the dataset
class(bike_buyers)

# Structure, dimension and names information
str(bike_buyers)
dim(bike_buyers)
names(bike_buyers)

# display numbers of NA strings
colSums(is.na(bike_buyers))

# Stat summary
summary(bike_buyers)

# Draws paits plot
ggpairs(bike_buyers)
ggduo(bike_buyers)

# histograms
par(mfrow = c(2,2))
hist(bike_buyers$Income)
hist(bike_buyers$Children, breaks = 20)
hist(bike_buyers$Cars, breaks = 15)
hist(bike_buyers$Age)
par(mfrow = c(1,1))

# Replacing of missed values ---------------------------------------------------
# Since, the distribution of Income and Age is left-skewed. We will impute median values
## Income and Age --------------------------------------------------------------
median(na.omit((bike_buyers$Income)))
median(na.omit((bike_buyers$Age)))
bike_buyers_clean <- bike_buyers
colSums(is.na(bike_buyers_clean))

# Income replaced with Median
bike_buyers_clean$Income[is.na(bike_buyers_clean$Income)] <- 
  median(na.omit((bike_buyers$Income)))
# Age replaced with Median
bike_buyers_clean$Age[is.na(bike_buyers_clean$Age)] <- 
  median(na.omit((bike_buyers$Age)))
colSums(is.na(bike_buyers_clean))

## Status, Gender and Children ------------------------------------------------
# Using Mode to impute Marital Status, Gender, Children and Home Owners
#Marital Statuses
freq_table <- table(bike_buyers$Marital.Status)
MS_mode <- names(freq_table)[freq_table == max(freq_table)]
bike_buyers_clean$Marital.Status[is.na(bike_buyers_clean$Marital.Status)] <- 
  MS_mode
colSums(is.na(bike_buyers_clean))

# Gender
freq_table <- table(bike_buyers$Gender)
Gender_mode <- names(freq_table)[freq_table == max(freq_table)]
bike_buyers_clean$Gender[is.na(bike_buyers_clean$Gender)] <- 
  Gender_mode
colSums(is.na(bike_buyers_clean))

# Children
freq_table <- table(bike_buyers$Children)
Children_mode <- names(freq_table)[freq_table == max(freq_table)]
bike_buyers_clean$Children[is.na(bike_buyers_clean$Children)] <- 
  Children_mode
colSums(is.na(bike_buyers_clean))

# Home Owners
freq_table <- table(bike_buyers$Home.Owner)
HO_mode <- names(freq_table)[freq_table == max(freq_table)]
bike_buyers_clean$Home.Owner[is.na(bike_buyers_clean$Home.Owner)] <- 
  HO_mode
colSums(is.na(bike_buyers_clean))

## Cars -----------------------------------------------------------------------
# Will be replaced with mean
bike_buyers_clean$Cars[is.na(bike_buyers_clean$Cars)] <- 
  mean(bike_buyers$Cars, na.rm = TRUE)
colSums(is.na(bike_buyers_clean))

# Save clean dataframe --------------------------------------------------------
write.csv(bike_buyers_clean,"R_datasets/bike_buyers_clean.csv", quote = FALSE, row.names = TRUE)
bike_buyers <- bike_buyers_clean

# Outlier analysis ------------------------------------------------------------
par(mfrow = c(1,2))
OutVals = boxplot(bike_buyers$Income, main = "Raw Income")$out
print( "Outliers in Income: ")
print(OutVals)

print("Records with outliers: ")
which(bike_buyers$Income %in% OutVals)

# Excluding outliers 
x = bike_buyers$Income [!(bike_buyers$Income %in% OutVals) ]
boxplot(x, main = "Income w/o Outliers")
summary(bike_buyers$Income)
summary(x)
par(mfrow = c(1,1))

# Diagrams ---------------------------------------------------------------------
## Barplots --------------------------------------------------------------------
counts <- table(bike_buyers$Cars, bike_buyers$Gender)
barplot(counts, main = '',
        xlab="Number",
        legend = rownames(counts))
## Scatterplot
plot(bike_buyers$Income, type= "p")

## Histogram with ggplot2
ggplot(bike_buyers, aes(x = Age)) +
  geom_histogram()

## Density plot
plot(density(bike_buyers$Income), main='Income Density Spread')
ggplot( haberman, aes(x = Age, fill = Status)) +
  geom_density()

# Transform dataset into pivot table with several columns and display multiply histograms
bike_buyers_pvt <- pivot_longer( bike_buyers, c("Income", "Cars", "Age"))
ggplot(bike_buyers_pvt, aes(x = value)) +
  geom_histogram() + 
  facet_wrap(name ~ ., scales = "free")
# Display multiply boxplots on the same canvas
#ggplot(bike_buyers_pvt, aes(x = value, fill = name)) +
#  geom_boxplot()

