## This is our regression for the extension in which we examine heterogeneous effects of heavy drinking and 
## other types

library(stargazer)
library(tidyverse)

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

lm1 <- lm(Treat_group ~ sober_dummy + BAC_result + std_drinks_today, data = dthd)
coefficients(lm1)

lm2 <- lm(Choice_group ~ sober_dummy + BAC_result + std_drinks_today, data = dthd)
coefficients(lm2)

lm3 <- lm(Control_group ~ sober_dummy + BAC_result + std_drinks_today, data = dthd)
coefficients(lm3)

lm4 <- lm(Treat_group ~ sober_dummy + BAC_result + std_drinks_today, data = dtr)
coefficients(lm4)

lm5 <- lm(Choice_group ~ sober_dummy + BAC_result + std_drinks_today, data = dtr)
coefficients(lm5)

lm6 <- lm(Control_group ~ sober_dummy + BAC_result + std_drinks_today, data = dtr)
coefficients(lm6)

# lm2 <- lm(Treat_group ~ BAC_result, data = dthd)
# lm3 <- lm(Treat_group ~ std_drinks_today, data = dthd)
# 
# lm4 <- lm(Choice_group ~ sober_dummy, data = dthd)
# lm5 <- lm(Choice_group ~ BAC_result, data = dthd)
# lm6 <- lm(Choice_group ~ std_drinks_today, data = dthd)
# 
# lm7 <- lm(Control_group ~ sober_dummy, data = dthd)
# lm8 <- lm(Control_group ~ BAC_result, data = dthd)
# lm9 <- lm(Control_group ~ std_drinks_today, data = dthd)
# 
# ## now for non-heavy drinkers
# 
# lm10 <- lm(Treat_group ~ sober_dummy, data = dtr)
# lm11 <- lm(Treat_group ~ BAC_result, data = dtr)
# lm12 <- lm(Treat_group ~ std_drinks_today, data = dtr)
# 
# lm13 <- lm(Choice_group ~ sober_dummy, data = dtr)
# lm14 <- lm(Choice_group ~ BAC_result, data = dtr)
# lm15 <- lm(Choice_group ~ std_drinks_today, data = dtr)
# 
# lm16 <- lm(Control_group ~ sober_dummy, data = dtr)
# lm17 <- lm(Control_group ~ BAC_result, data = dtr)
# lm18 <- lm(Control_group ~ std_drinks_today, data = dtr)

# stargazer(lm1, title="Results", align=TRUE)