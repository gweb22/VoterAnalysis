rm(list = ls())
setwd("~/Desktop/VSCode/R/POLI 281/CSV_Files")
voter_wt <- read.csv("cces18.csv", stringsAsFactors = T)



#P2
#table section
table(voter_wt$wait)
prop.table(table(voter_wt$wait))
mean(voter_wt$wait, na.rm = T)
median(voter_wt$wait, na.rm = T)

#long wait
library(dplyr)
voter_wt <- voter_wt %>%
  mutate(longwait = case_when(
    (wait == 4 | wait == 5) ~ T,
    (wait <= 3) ~ F,
    (wait == 6) ~ NA
  ))



#P3
#creates df for long wait percentages by state
st_wt <- voter_wt %>%
  group_by(state) %>%
  summarize(lng_wt_pct = mean(longwait, na.rm = T))

#graphs results
library(ggplot2)
wait_plot <- ggplot(st_wt, aes(reorder(state, -lng_wt_pct), lng_wt_pct)) + 
  geom_bar(stat = "identity", fill = "Blue") +
  coord_flip() +
  ylab("Long Wait Time Percentage") +
  xlab("State")
wait_plot



#P4
#creates duplicate of above df, including regions
rgn_wt <- voter_wt %>%
  group_by(state, region) %>%
  summarize(lng_wt_pct = mean(longwait, na.rm = T))

#graphs results
wait_plot2 <- ggplot(rgn_wt, aes(reorder(state, -lng_wt_pct), lng_wt_pct, fill = region)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Long Wait Time Percentage") +
  xlab("State")
wait_plot2



#P5
#creates conserv_vote variable based on 2016 voting
voter_wt <- voter_wt %>%
  mutate(conserv_vote = case_when(
    (vote2016 == 1) ~ "Conservative",
    (vote2016 == 2) ~ "Liberal",
    (vote2016 != 1 & vote2016 != 2) ~ NA_character_
  ))

#creates wait time and ideology df
conserv_wt <- voter_wt %>%
  select(conserv_vote, longwait) %>%
  filter(!is.na(conserv_vote))

#prop table comparing ideology with wait times
prop.table(table(conserv_wt$longwait, conserv_wt$conserv_vote), margin = 2)



#P6
#creates condensed race variable
voter_wt <- voter_wt %>%
  mutate(race_5 = case_when(
    (race == 1) ~ "White",
    (race == 2) ~ "Black",
    (race == 3) ~ "Hispanic",
    (race == 4) ~ "Asian",
    (race >= 5) ~ "Other"
  ))

#reorders race_5 for graphing purposes
voter_wt$race_5 <- factor(voter_wt$race_5, levels = c("White", "Black", "Hispanic", "Asian", "Other"))

#displays long wait by race
race_wt <- voter_wt %>%
  group_by(race_5) %>%
  summarize(prop_lngwt = mean(longwait, na.rm = T))

#graphs long wait by race
race_plot <- ggplot(race_wt, aes(race_5, prop_lngwt)) + 
  geom_bar(stat = "identity", fill = "Blue") +
  xlab("Race") +
  ylab("Proportion of Long Wait Times")
race_plot



#P7
#creates new income variable divided by 4 categories
voter_wt <- voter_wt %>%
  mutate(faminc_4 = case_when(
    (faminc <= 5) ~ "Low", # less than 50,000
    (faminc > 5 & faminc <= 10) ~ "Mid_Low", #50k - 120k
    (faminc > 10 & faminc <= 13) ~ "Mid_High", #120k - 250k
    (faminc > 13 & faminc <= 16) ~ "High", #250k+
    (faminc == 97) ~ NA_character_
  ))

#displays the proportion of voters in each income category
prop.table(table(voter_wt$faminc_4))



#P8
#reorders levels of faminc_4 for graphing purposes
voter_wt$faminc_4 <- factor(voter_wt$faminc_4, levels = c("High", "Mid_High", "Mid_Low", "Low"))

#df including race, income, and avg long wait
race_income <- voter_wt %>%
  group_by(faminc_4, race_5) %>%
  summarize(wait_times = mean(longwait, na.rm = T))

#graph of income and wait times
inc_wt_plot <- ggplot(race_income, aes(faminc_4, wait_times)) + 
  geom_bar(stat = "identity", fill = "DarkGreen") +
  xlab("Income Level") +
  ylab("Proportion of Long Wait Times")
inc_wt_plot

#graph of race, wait time, and income
race_inc_plot <- ggplot(race_income, aes(reorder(race_5, -wait_times), wait_times, fill = faminc_4)) + geom_bar(stat = "identity") +
  xlab("Race") +
  ylab("Proportion of Long Wait Times") +
  guides(fill = guide_legend(title = "Income Level"))
race_inc_plot



#P9
#Low Income df
Low_df <- race_income %>%
  filter(faminc_4 == "Low")

#Low Income graph
Low_plot <- ggplot(Low_df, aes(race_5, wait_times)) +
  geom_bar(stat = "identity", fill = "#C77CFF")
Low_plot


#Mid_Low Income df
Mid_Low_df <- race_income %>%
  filter(faminc_4 == "Mid_Low")

#Mid_Low Income graph
Mid_Low_plot <- ggplot(Mid_Low_df, aes(race_5, wait_times)) +
  geom_bar(stat = "identity", fill = "#00BFC4")
Mid_Low_plot


#Mid_High Income df
Mid_High_df <- race_income %>%
  filter(faminc_4 == "Mid_High")

#Mid_High Income graph
Mid_High_plot <- ggplot(Mid_High_df, aes(race_5, wait_times)) +
  geom_bar(stat = "identity", fill = "#7CAE00")
Mid_High_plot


#High Income df
High_df <- race_income %>%
  filter(faminc_4 == "High")

#High Income graph
High_plot <- ggplot(High_df, aes(race_5, wait_times)) +
  geom_bar(stat = "identity", fill = "#F8766D")
High_plot



#P10
#histogram of average county income
ggplot(voter_wt, aes(income_county)) + geom_histogram()

#outlier removal
#filters county income to only include +/- 2.5 standard deviations from the mean
voter_wt <- voter_wt %>%
  filter(between(income_county, (mean(voter_wt$income_county, na.rm = T) - (2.5 * sd(voter_wt$income_county, na.rm = T))), (mean(voter_wt$income_county, na.rm = T) + (2.5 * sd(voter_wt$income_county, na.rm = T)))))

#creates density variable
voter_wt <- voter_wt %>%
  mutate(voter_wt, density = (county_pop/1000) / land_area)

#views density histogram
ggplot(voter_wt, aes(density)) + geom_histogram()

#adjusts density variable to only include +/- 2.5 standard deviations from the mean
voter_wt <- voter_wt %>%
  filter(between(density, mean(voter_wt$density, na.rm = T) - (2.5 * sd(voter_wt$density, na.rm = T)), mean(voter_wt$density, na.rm = T) + (2.5 * sd(voter_wt$density, na.rm = T))))


#regression race variable updates
voter_wt <- voter_wt %>%
  mutate(voter_wt, 
         black = case_when(
           race_5 == "Black" ~ 1, 
           race_5 != "Black" ~ 0),
         hispanic = case_when(
           race_5 == "Hispanic" ~ 1, 
           race_5 != "Hispanic" ~ 0),
         asian = case_when(
           race_5 == "Asian" ~ 1, 
           race_5 != "Asian" ~ 0),
         other = case_when(
           race_5 == "Other" ~ 1, 
           race_5 != "Other" ~ 0),
         wait_reg = wait,
         faminc_reg = faminc
  )

#modifies wait_reg and faminc_reg variable
voter_wt$wait_reg[voter_wt$wait == 6] <- NA
voter_wt$faminc_reg[voter_wt$faminc == 97] <- NA



#P11
#Regression of waiting times on Black, Hispanic, Asian, and Other
fit1 <- lm(voter_wt$wait_reg ~ voter_wt$black + voter_wt$hispanic + voter_wt$asian + voter_wt$other)
summary(fit1)
nobs(fit1)

#Regression of waiting times on Black, Hispanic, Asian, Other, personal income, county socioeconomic status, and population density
fit2 <- lm(voter_wt$wait_reg ~ voter_wt$black + voter_wt$hispanic + voter_wt$asian + voter_wt$other + voter_wt$faminc_reg + voter_wt$density + voter_wt$income_county)
summary(fit2)
nobs(fit2)
