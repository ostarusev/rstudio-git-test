library(dplyr)
library(explore)

mtcars %>% explore_tbl()
mtcars %>% head()

mtcars %>% describe()

mtcars %>% explore_all()

mtcars %>% explore( gear )
mtcars %>% 
  select(gear, mpg, hp, cyl, am) %>% 
  explore_all(target = gear)

data <- mtcars %>% 
  mutate(highmpg = if_else(mpg > 25, 1, 0, 0)) %>% 
  select(-mpg)
data %>% explore(highmpg)

data %>% 
  select(highmpg, cyl, disp, hp) %>% 
  explore_all(target = highmpg)
