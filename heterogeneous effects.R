## This is our regression for the extension in which we examine heterogeneous effects of heavy drinking and 
## other types

library(stargazer)
library(tidyverse)
library(haven)

dt <- read_dta('Replication Data/Final_alcohol_main_data.dta')
dt <- subset(dt, day4==1 & day_in_study > 1 & day_in_study < 20)

## Let's just get rid of everything that we're not interested in

## This is our reduced data table, now we need a determining factor of "heavy drinking"
## Use 6 drinks or more
dthd <- subset(dt, Std_drinks_overall_BL > 6)
dtr <- subset(dt, Std_drinks_overall_BL <= 6)

## Now look at simple regressions of...
## Treat_group, Choice_group, Control_group on...
## sober_dummy (sober at the office that day), BAC_result (used for payment), and std_drinks_today
## We'll look, without controls, at these models in a straight comparison

lm1 <- lm(sober_dummy ~ Treat_group + Choice_group + Control_group, data = dthd)
lm2 <- lm(BAC_result ~ Treat_group + Choice_group + Control_group, data = dthd)
lm3 <- lm(std_drinks_today ~ Treat_group + Choice_group + Control_group, data = dthd)
##
lm4 <- lm(sober_dummy ~ Treat_group + Choice_group + Control_group, data = dtr)
lm5 <- lm(BAC_result ~ Treat_group + Choice_group + Control_group, data = dtr)
lm6 <- lm(std_drinks_today ~ Treat_group + Choice_group + Control_group, data = dtr)

