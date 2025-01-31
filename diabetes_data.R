library(ggplot2)
library(tidyverse)
# 9/11/2023

# Diabetes Data from Sklearn

# https://github.com/scikit-learn/scikit-learn/blob/7f9bad99d6e0a3e8ddf92a7e5561245224dab102/sklearn/datasets/data/diabetes_data_raw.csv.gz
# https://scikit-learn.org/stable/

# Custom Functions -------------------------------------------------------------
# Function to test a variable for normality
test_normality <- function(data, variable_name) {
  result <- shapiro.test(data)
  if (result$p.value > 0.05) {
    cat(variable_name, result$p.value, " Gaussian\n")
  } else {
    cat(variable_name, result$p.value, " Not Gaussian\n")
  }
}

# Variation Coefficient Calculating
VC_calc <- function( data, variable_name ) {
  result <- mean(data) / sd(data) * 100
  cat("Variation coefficient for ", variable_name, " = ", result, "\n")
  
}

# reading the data -------------------------------------------------------------
# Read the data
data_fname <- "R_datasets/Sklearn/diabetes_data.csv"
diabetes_data <- read.csv( data_fname, header = TRUE, sep = ",", stringsAsFactors = FALSE)
target_fname <- "R_datasets/Sklearn/diabetes_target.csv"
diabetes_target <- read.csv( target_fname, header = FALSE, sep = ",", stringsAsFactors = FALSE)
target <- diabetes_target$V1

# Primary data reviewing -------------------------------------------------------
# Show up 10 first records
head(diabetes_data, 10)

# Data structure
str(diabetes_data)

# Statistical summary
summary(diabetes_data)

# Transform SEX to factor ------------------------------------------------------
diabetes_data$sex <- as.factor(diabetes_data$sex)

# Interim summary about types --------------------------------------------------
# factor - sex
# numeric - age, bmi, bp, s1-s6
# no NA

# Calculate frequencies and % for SEX variable
sex_counts <- table( diabetes_data$sex)
sex_percentages <- sex_counts / sum(sex_counts) * 100

# Histograms of numeric variables ----------------------------------------------
#par(mfrow = c(2,2))
hist( diabetes_data$age )
hist( diabetes_data$bmi )
hist( diabetes_data$bp )
hist( diabetes_data$s1_tc )
hist( diabetes_data$s2_ldl )
hist( diabetes_data$s3_hdl )
hist( diabetes_data$s4_tch )
hist( diabetes_data$s5_ltg )
hist( diabetes_data$s6_glu )
#par(mfrow = c(1,1))
# All variables look as normal except AGE and S4

# Density plots of numeric variables -------------------------------------------
par(mfrow = c(2,2))
age_density <- density( diabetes_data$age )
plot(age_density, main = "Density AGE")
bmi_density <- density( diabetes_data$bmi )
plot(bmi_density, main = "Density BMI")
bp_density <- density( diabetes_data$bp )
plot(bp_density, main = "Density BP")
s1_density <- density( diabetes_data$s1_tc )
plot(s1_density, main = "Density S1")
s2_density <- density( diabetes_data$s2_ldl )
plot(s2_density, main = "Density S2")
s3_density <- density( diabetes_data$s3_hdl )
plot(s3_density, main = "Density S3")
s4_density <- density( diabetes_data$s4_tch )
plot(s4_density, main = "Density S4")
s5_density <- density( diabetes_data$s5_ltg )
plot(s5_density, main = "Density S5")
s6_density <- density( diabetes_data$s6_glu )
plot(s6_density, main = "Density S6")
par(mfrow = c(1,1))
# All variables except S4 have density plot look close to Gaussian

# QQNORM for all numeric variables ---------------------------------------------
par( mfrow = c(2,2))
qqnorm(diabetes_data$age, main = "Age"); qqline(diabetes_data$age) 
qqnorm(diabetes_data$bmi, main = "BMI");qqline(diabetes_data$bmi)
qqnorm(diabetes_data$bp, main = "BP");qqline(diabetes_data$bp)
qqnorm(diabetes_data$s1_tc, main = "S1");qqline(diabetes_data$s1_tc, col = "red")
qqnorm(diabetes_data$s2_ldl, main = "S2");qqline(diabetes_data$s2_ldl, col = "red")
qqnorm(diabetes_data$s3_hdl, main = "S3");qqline(diabetes_data$s3_hdl, col = "red")
qqnorm(diabetes_data$s4_tch, main = "S4");qqline(diabetes_data$s4_tch, col = "red")
qqnorm(diabetes_data$s5_ltg, main = "S5");qqline(diabetes_data$s5_ltg, col = "red")
qqnorm(diabetes_data$s6_glu, main = "S6");qqline(diabetes_data$s6_glu, col = "red")
par(mfrow = c(1,1))

# Test for Normality -----------------------------------------------------------
test_normality( diabetes_data$age, "Age")
test_normality( diabetes_data$bmi, "BMI")
test_normality( diabetes_data$bp, "BP")
test_normality( diabetes_data$s1_tc, "S1")
test_normality( diabetes_data$s2_ldl, "S2")
test_normality( diabetes_data$s3_hdl, "S3")
test_normality( diabetes_data$s4_tch, "S4")
test_normality( diabetes_data$s5_ltg, "S5")
test_normality( diabetes_data$s6_glu, "S6")
# All variables are not Gaussian

# Variation Coefficient as: mean / stdev * 100% --------------------------------
VC_calc(diabetes_data$age, "Age" )
VC_calc( diabetes_data$bmi, "BMI")
VC_calc( diabetes_data$bp, "BP")
VC_calc( diabetes_data$s1_tc, "S1")
VC_calc( diabetes_data$s2_ldl, "S2")
VC_calc( diabetes_data$s3_hdl, "S3")
VC_calc( diabetes_data$s4_tch, "S4")
VC_calc( diabetes_data$s5_ltg, "S5")
VC_calc( diabetes_data$s6_glu, "S6")

# Distribution of categorical variable -----------------------------------------
diabetes_data %>%
  ggplot( aes(x = sex )) +
  geom_bar()
result <- table(diabetes_data$sex)
result

# Target Based Part -----------------------------------------------------------
## Target variable diagrams ----------------------------------------------------
par(mfrow = c(2,2))
hist(target, main = "Target")
target_density <- density( target )
plot(target_density, main = "Density of TARGET")
boxplot( target, main = "Target" )
qqnorm(target, main = "Target"); qqline( target, col = "red" )
par(mfrow = c(1,1))

## Relationship between categorical and target variables ------------------------
### Comparing boxplots ----------------------------------------------------------
diabetes_data %>%
  ggplot( aes(x = target, color = sex)) +
  geom_boxplot()

### scatterplots ----------------------------------------------------------------
library(cowplot)
p1 <- diabetes_data %>%
  ggplot( aes(x = age, y = target)) +
  geom_point() +
  geom_smooth( method = "lm", color = "red")
p2 <- diabetes_data %>%
  ggplot( aes(x = bmi, y = target)) +
  geom_point() +
  geom_smooth( method = "lm", color = "red")
p3 <- diabetes_data %>%
  ggplot( aes(x = bp, y = target)) +
  geom_point() +
  geom_smooth( method = "lm", color = "red")
p4 <- diabetes_data %>%
  ggplot( aes(x = s1_tc, y = target)) +
  geom_point() +
  geom_smooth( method = "lm", color = "red")
p5 <- diabetes_data %>%
  ggplot( aes(x = s2_ldl, y = target)) +
  geom_point() +
  geom_smooth( method = "lm", color = "red")
p6 <- diabetes_data %>%
  ggplot( aes(x = s3_hdl, y = target)) +
  geom_point() +
  geom_smooth( method = "lm", color = "red")
p7 <- diabetes_data %>%
  ggplot( aes(x = s4_tch, y = target)) +
  geom_point() +
  geom_smooth( method = "lm", color = "red")
p8 <- diabetes_data %>%
  ggplot( aes(x = s5_ltg, y = target)) +
  geom_point() +
  geom_smooth( method = "lm", color = "red")
p9 <- diabetes_data %>%
  ggplot( aes(x = s6_glu, y = target)) +
  geom_point() +
  geom_smooth( method = "lm", color = "red")

plot_grid( p1, p2, p3, p4, ncol = 2)
plot_grid( p5, p6, p7, p8, ncol = 2)
plot_grid( p9, ncol = 1)

## Show correlation matrix ------------------------------------------------------
# Adding TARGET and removing SEX as categorical
dd <- diabetes_data  %>%
  select( -sex ) %>%
  mutate( target = target)
library(corrplot)
corrplot( cor(dd, method = "kendall"), method = "number", type = "upper" )
corrplot( cor(dd, method = "spearman"), method = "number", type = "upper" )
corrplot( cor(dd, method = "pearson"), method = "number", type = "upper" )


.