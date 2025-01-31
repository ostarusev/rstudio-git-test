library(tidyverse)
library(ggplot2)
library(GGally)
# Haberman Survival Data ----
# The dataset contains cases from a study that was conducted between
# 1958 and 1970 at the University of Chicago's Billings Hospital on
# the survival of patients who had undergone surgery for breast
# cancer.
# Sources:
#(a) Donor: Tjen-Sien Lim (limt@stat.wisc.edu)
#(b) Date: March 4, 1999

# Attribute Information:
# 1) (Age) Age of patient at time of operation (numerical)
# 2) (Year) Patient's year of operation (year - 1900, numerical)
# 3) (Node) Number of positive auxiliary nodes detected (numerical)
# 4) (Status) Survival status (class attribute)
#    1 = the patient survived 5 years or longer
#    2 = the patient died within 5 year


# read data from file
haberman <- read.csv( "R_datasets/haberman.csv", header = TRUE, na.strings = '', stringsAsFactors = TRUE )

# General Info ----------------------------------------------------------------
head(haberman)
dim(haberman)
names(haberman)
str(haberman)

# Stat summary
summary(haberman)

# Pairs plot
ggpairs(haberman)
ggduo(haberman)

# Missed Values and Outliers --------------------------------------------------
## Missed Values
haberman_clean <- haberman
colSums(is.na(haberman_clean)) # no NA values

## Outliers
par(mfrow = c(2,2))
boxplot(haberman$Age)$out
boxplot(haberman$Year)$out
boxplot(haberman$Node)$out
boxplot(haberman$Status)$out
par(mfrow = c(1,1))

# Node column contains outliers
par(mfrow = c(1,2))
OutVals = boxplot(haberman$Node, main = "Raw Data")$out
print( "Outliers in Node: ")
print(OutVals)

print("Records with outliers: ")
which(haberman$Node %in% OutVals)

# Excluding outliers 
x = haberman$Node [!(haberman$Node %in% OutVals) ]
boxplot(x, main = "Node w/o Outliers")
summary(haberman$Node)
summary(x)
par(mfrow = c(1,1))

# Using EXPLORE package -------------------------------------------------------
library(explore)
haberman %>% explore_all()
haberman %>% describe_all()
haberman %>% explore()

# Correlation -----------------------------------------------------------------
cor_matr <- round( cor( haberman ), 3)
cor_matr

library(ggcorrplot)
ggcorrplot(cor_matr, type = "lower", lab = TRUE)

library(corrplot)
corrplot(cor_matr, method = "pie", type = "upper", order = 'original')

# Diagrams --------------------------------------------------------------------
## histograms ----
par(mfrow = c(2,2))
hist(haberman$Age, breaks = 30)
hist(haberman$Node, breaks = 20)
hist(haberman$Year, breaks = 15)
hist(haberman$Status)
par(mfrow = c(1,1))

## Boxplots ----
par(mfrow = c(2,2))
boxplot(haberman$Age, main = "Age")
boxplot(haberman$Node, main = "Positivr Aux Num")
boxplot(haberman$Year, main = "Year of Ops")
boxplot(haberman$Status, main = "Status")
par(mfrow = c(1,1))

## Scatter plots ----
par(mfrow = c(2,2))
plot(x = haberman$Age, y = haberman$Node, col = haberman$Status, main = "Age ~ PosAuxNum")
plot(x = haberman$Age, y = haberman$Node, col = haberman$Status, main = "Age ~ YearOfOps")
plot(x = haberman$Node, y = haberman$Year, col = haberman$Status, main = "PosAuxNum ~ YearOfOps")
par(mfrow = c(1,1))

## PDF and CDF for all main parameters ----
# Age 
ggplot(haberman, aes(x = Age)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "lightblue", color = "black") +
  geom_density(color = "blue") +
  labs(title = "PDF and Histogram for Ages")
ggplot(haberman, aes(x = Age)) +
  stat_ecdf(geom = "step", color = "blue") +
  labs(title = "Cumulative Distribution Function for Age")

# PosAuxNum
ggplot(haberman, aes(x = Node)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "lightblue", color = "black") +
  geom_density(color = "blue") +
  labs(title = "Probability Density Functions and Histogram for Positive Aux Num")
ggplot(haberman, aes(x = Node)) +
  stat_ecdf(geom = "step", color = "blue") +
  labs(title = "Cumulative Distribution Function for Positive Aux Num")

# Year of Op
ggplot(haberman, aes(x = Year)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "lightblue", color = "black") +
  geom_density(color = "blue") +
  labs(title = "Probability Density Functions and Histogram for YearOfOp")
ggplot(haberman, aes(x = Year)) +
  stat_ecdf(geom = "step", color = "blue") +
  labs(title = "Cumulative Distribution Function for Year of Ops")


# For Age with Status
# PDF with splitting by Status
ggplot(haberman, aes(x = Age, fill = factor(Status))) +
  geom_density(alpha = 0.5) +
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", fill = "lightblue", alpha = 0.5) +
  labs(title = "PDF for Age split into Status") +
  scale_fill_discrete(name = "Status", labels = c("Alive", "Died"))

### CDF for Age split into Status -----
ggplot(haberman, aes(x = Age, color = factor(Status))) +
  stat_ecdf(geom = "step") +
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", fill = "lightblue", alpha = 0.5) +
  labs(title = "CDF for Age split into Status") +
  scale_color_discrete(name = "Status", labels = c("Alive", "Died"))

# Distributions ---------------------------------------------------------------
library(MASS)
library(fitdistrplus)

## Age ----
# N(52.5, 11)
descdist(haberman$Age, boot = 1000)
plotdist(haberman$Age)
qqnorm(haberman$Age)

fd_res <- fitdistr( haberman$Age, "normal")
print(fd_res)

x <- rnorm(306, 52.5, 11)
chisq_result <- chisq.test(haberman$Age, x )
chisq_result
qqplot(x, haberman$Age, conf.level = 1 - chisq_result$p.value, conf.args = list(exact = TRUE, col = "lightgrey"))

## Node ----
# Exponential( 0.25 )
descdist( haberman$Node, boot = 1000 )
plotdist(haberman$Node)
qqnorm(haberman$Node)

fd_res <- fitdistr( haberman$Node, "exponential")
print(fd_res)

x <- rexp( 306, 0.2484 )
chisq_result <- chisq.test(haberman$Node, x )
chisq_result
qqplot(x, haberman$Node, conf.level = 1 - chisq_result$p.value, conf.args = list(exact = TRUE, col = "lightgrey"))

## Year of Operations -----
# Uniform( 58, 69 )
descdist( haberman$Year, boot = 1000 )
plotdist( haberman$Year)

fd_res <- fitdist( haberman$Year, "unif")
print(fd_res)

x <- runif( 306, 58, 69 )
chisq_result <- chisq.test(haberman$Year, x )
chisq_result
qqplot(x, haberman$Year, conf.level = 1 - chisq_result$p.value, conf.args = list(exact = TRUE, col = "lightgrey"))

# Survival Analysis --------------------------
library(survival)

# Kaplan-Meier fit and survival curve diagram
km_fit <- survfit(Surv(Year, Status) ~ 1, data = haberman)
plot(km_fit, xlab = "Year", ylab = "Survival", main = "Survival Curve Kaplan-Meier")

# Cox Proportional-Hazards model
cox_fit <- coxph(Surv(Year, Status) ~ Age + Node, data = haberman)
summary(cox_fit)

# Groups Comparison ------------------------------
# Log-rank тест
logrank_test <- survdiff(Surv(Year, Status) ~ Node, data = haberman)
print(logrank_test)

# Boxplot для сравнения количества положительных узлов между группами
ggplot(haberman, aes(x = factor(Node), y = Year, fill = factor(Status))) +
  geom_boxplot() +
  labs(title = "Сравнение групп по количеству положительных узлов", x = "Количество положительных узлов", y = "Год") +
  scale_fill_discrete(name = "Status", labels = c("Alive", "Dead"))

