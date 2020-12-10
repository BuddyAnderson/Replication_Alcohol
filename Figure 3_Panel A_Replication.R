## Let's try to legitimately replicate Figure 3, left hand panel.
## This will involve trying to write code that does the same thing as the STATA code
## The first steps are straight forward

# Problem solving graphing issue
# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)


## Step 1: Use participant data for those that were still in the study at day_in_study == 4
## but we still want all observations for those individuals
## Note: we'll need some libraries

library(tidyverse)
library(foreign)
library(haven)
library(doBy)

## keep if day4 == 1 (made it through lead in period)
dt <- read_dta('Replication Data/Final_alcohol_main_data.dta')

## STATA chunks
# * Consider only individuals who made it at least to day 4 (i.e. who were assigned a treatment):
#   keep if day4 == 1
dt <- subset(dt, day4 == 1)
# 
# *Consider only individuals between days 1 and 20: 
#   keep if day_in_study < 20 & day_in_study > 1
dt <- subset(dt, dt$day_in_study > 1 & dt$day_in_study < 20)
# 
## Not really sure what to do here; do I need to sort the data?
# *Check whether data are complete:
#   bys tx_group: tab day_in_study, m

## Skipping the bys..... portion of the STATA script, let's move on to the graphing portion
## preserve
#                               VARIABLE         
# collapse (mean) mean_sober = sober_dummy (sd) sd_sober = sober_dummy (sum) number, by(day_in_study tx_group)
# replace mean_sober = 100 * mean_sober
# 
## Collapse data in R
## Note, we are doing away with ID's here and just looking in the aggregate per day in study and treatments
collapse1 <- summaryBy(sober_dummy + number ~ day_in_study + tx_group, FUN=c(mean,sd, sum), data=dt)
collapse1 <- select(collapse1, -c(sober_dummy.sum, number.mean, number.sd))

# Rename column where names is ....
names(collapse1)[names(collapse1) == "sober_dummy.mean"] <- "mean_sober"
names(collapse1)[names(collapse1) == "sober_dummy.sd"] <- "sd_sober"



collapse1$mean_sober<-collapse1$mean_sober*100


## We now have our data, supposedly in the same manner as the original authors
## Let's figure out how to properly graph this shit
## getting "continuous" error so maybe rounding will help

collapseround <-round(collapse1[,],3) #the "-1" excludes column 1
collapseround

collapseround$tx_group2 <- "Incentives"
collapseround$tx_group2[collapseround$tx_group == 2] <- "Choice"
collapseround$tx_group2[collapseround$tx_group == 3] <- "Control"

## Reorder labels
collapseround$tx_group2 <- factor(collapseround$tx_group2, levels = c("Incentives", "Choice", "Control"))

# Plot
 p <-  ggplot(collapseround, aes(x=day_in_study, y=mean_sober, group=tx_group2, color=tx_group2)) +
  geom_line() +
  # scale_color_viridis(discrete = TRUE) +
  ggtitle("Panel A: Sobriety at the Study Office") +
  ylab("Fraction Sober (%)") +
   xlab("Day in Study") +
   guides(color=guide_legend(title="Treatments")) + 
   geom_vline(xintercept = 4.5) + 
   scale_color_manual(values=c("green1", "cyan1", "royalblue"))
 
 ndt <- subset(dt, day_in_study == 2)

## The only thing we are missing now is the intervention line, but I'm not going to worry about that
## seeing as I've spent a very long time on this one figure already


# twoway 	(scatter mean_sober day_in_study if tx_group == 1, sort mcolor(gs1) lcolor(gs1) connect(l) ///
#         msize(medium) msymbol(circle) lwidth(medthick) xline(4.5, lcolor(navy))) ///
#         (scatter mean_sober day_in_study if tx_group == 2, sort mcolor(forest_green) lcolor(forest_green) connect(l) ///
#         msize(medium) msymbol(square_hollow)) ///
#         (scatter mean_sober day_in_study if tx_group == 3, sort mcolor(maroon) lcolor(maroon) connect(l) ///
#         msize(medium) msymbol(diamond_hollow) lwidth(medthick) lpattern(shortdash_dot) ///
#         text(31 8.6 "{&larr} Sobriety incentives assigned", color(navy))) , ///
#         ysc(r(30 60)) ymtick(30(10)60) ylabel(30(10)60) ///
#         xsc(r(1.5 19.5)) xmtick(2 5 10 15 19) xlabel(2 5 10 15 19) ///
#         graphregion(color(white)) bgcolor(white) ///
#         xtitle("Day in Study") ytitle("Fraction Sober (%)") title("Sobriety at the Study Office") ///
#         legend(label(1 "Incentives") label(2 "Choice") label(3 "Control") rows(1))
# 
# *Save graph
# graph export "$figures/3a_Sobriety_figure_FINAL.eps", replace 
# 
# restore



