## Multiple regression analysis for alcohol consumption

## daily drinks = b0 + b1marriage + b2number of kids + b3yearsworking  ??????


library(tidyverse)
library(foreign)
library(haven)
final_dt <- read_dta("Replication Data/Final_alcohol_main_data.dta")
# colnames(final_dt)
# which( colnames(final_dt)=="sober_dummy" )

## We want to look at the impact of marriage on the amount of alcohol consumed
## This could be done at any point in time for our weak analysis, so let's just do one of the first days
## AKA, day_in_study == 1

dt <- subset(final_dt, day_in_study == 1) ## all subjects day 1 of study
## Round number of drinks to whole numbers and do something like 10+ for last bar
dt$Rounded_Drinks <-round(dt$Std_drinks_overall_BL)
invisible(length(unique(dt[["married"]]))) ## this was 3, so let's get only married or unmarried
mdt <- subset(dt, married == 1) ## married subjects day 1 of study
nmdt <- subset(dt, married == 0) ## married subjects day 1 of study

##########
dt$Cmarried <- "Yes"
dt$Cmarried[dt$married == 0] <- "No"

p<-ggplot(dt, aes(x=Std_drinks_overall_BL, fill=Cmarried, color=Cmarried)) +
  geom_histogram(position="identity", alpha = 0.5, bins = 50, binwidth = 1)


## Now let's run a regression of marriage on standard drinks
# Compute the model
model <- lm(Rounded_Drinks ~ married, data = dt)
summary(model)$coef

model2 <- lm(Rounded_Drinks ~ married + work_years + num_children, data = dt)
summary(model2)$coef

## Now let's predict some more with the number of children a driver has, number of years worked as driver, age,
## education years, wife and he live together, effect of owning rickshaw
## Note, only men were in the study from what the paper said


