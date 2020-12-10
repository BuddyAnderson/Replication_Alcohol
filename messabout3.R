## Let's try and make Table 3 as best we can... not very hopeful here but we'll give it a go
library(stargazer)
library(tidyverse)
library(foreign)
library(haven)
dt <- read_dta("Replication Data/Final_alcohol_main_data.dta")
## filter
dt <- subset(dt, day4 == 1)
dt <- subset(dt, dt$day_in_study > 1 & dt$day_in_study < 20)
## Now let's try and make those tables using stargazer's nice formating
## 2 OLS models
linear.1 <- lm(rating ~ complaints + privileges + learning + raises + critical,
               data=attitude)
linear.2 <- lm(rating ~ complaints + privileges + learning, data=attitude)
## create an indicator dependent variable, and run a probit model
attitude$high.rating <- (attitude$rating > 70)
probit.model <- glm(high.rating ~ learning + critical + advance, data=attitude,
                    family = binomial(link = "probit"))
stargazer(linear.1, linear.2, probit.model, title="Results", align=TRUE)






