# https://cran.r-project.org/web/packages/explore/vignettes/explore_titanic.html

library(tidyverse)
library(dplyr)
library(explore)
 
# How to explore the Titanic dataset using the explore package.
# The explore package simplifies Exploratory Data Analysis (EDA). 
# Get faster insights with less code!
  
# The titanic dataset is available in base R. The data has 5 variables and only 
# 32 rows. Each row does NOT represent an observation. It is not tidy, instead 
# the data set contains Frequencies! And it is not a data frame, therefore we we need to convert it first.
titanic <- as_tibble(Titanic)

# Explore data set 
titanic %>% describe_tbl(n = n)

titanic %>% describe()

# Look at the data
titanic %>% head(10)

# Explore variables
titanic %>% explore(Sex, n=n)
titanic %>% explore(Class, n=n)
titanic %>% describe(Class)

# To explore all variables, we can simply use explore_all(). You automatically 
# fit the height of the plot using 
# fig.height=total_fig_height(titanic, var_name_n = "n") in the code chunk header.
titanic %>% explore_all(n = n)

# To get a better feeling of the relationship between Class and Survived, we 
# switch to percentage and split the target into sperate bars. We can do that 
# by using split = TRUE (which is default).
titanic %>% explore(Class, target = Survived, n = n, split = FALSE)

# To get a better feeling of the relationship between Class and Survived, we 
# switch to percentage and split the target into sperate bars. We can do that 
# by using split = TRUE (which is default).
titanic %>% explore(Class, target = Survived, n = n, split = TRUE)

# Now we get a plot, where each color sum to 100%. So a big difference in bar 
# length indicates an important relationship between the two variables. In this 
# case, passengers of 1st Class had the highest probability to survive.
titanic %>% explore(Sex, target = Survived, n = n)

# Female are much more likely to survive!

titanic %>% explore(Age, target = Survived, n = n)
# Child had an advantage to survive.

# Now we can create a simple decision tree. As we have count-data we need to 
# pass parameter n. Building a simple decision tree
titanic %>% explain_tree(target = Survived, n = n)
# We see that Sex and Class can give a good explanation who are more likely to survive.
# - Sex = Male: 21% survived (79% of all observations)
# - Sex = Female & Class = 3rd: 46% survived (9% of all observations)
# - Sex = Female & Class <> 3rd: 93% survived (12% of all observations)

# Other correlations
titanic %>% explore(Age, target = Class, n = n)
# Child are unlikely in the 1st class! And all Crew members are adult as expected.

titanic %>% explore(Sex, target = Class, n = n)
# Almost no female Crew members! Female tend to have better Class!

explore(titanic)
