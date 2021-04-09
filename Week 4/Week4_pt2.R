# Assingment 4 Part 2
# Name: Tabbalat, Abed
# Class: DSC520
# Date: 04-09-2021

#We interact with a few datasets in this course, one you are already familiar with, the 2014 American Community Survey and the second is a Housing dataset, 
#that provides real estate transactions recorded from 1964 to 2016.  
# For this exercise, you need to start practicing some data transformation steps – 
# which will carry into next week, as you learn some additional methods.  
# For this week, using either dataset (or one of your own – although I will let you know ahead of time that the Housing dataset is used for a later assignment, 
# so not a bad idea for you to get more comfortable with now!), perform the following data transformations:
library(tidyr)
library(pastecs)
library(plyr)
library(dplyr)
library(ggplot2)

setwd("C:/Users/abedt/OneDrive/Desktop/PyCharm Projects/DSC 520/Week 4")
df_2 <- read.csv("acs-14.csv")

  # Use the apply function on a variable in your dataset
  apply(select(df_2, c(6:8)), 2, sum)
  # Use the aggregate function on a variable in your dataset
  aggregate(df_2$HSDegree ~ df_2$POPGROUP.display.label, df_2, sum)
  # Use the plyr function on a variable in your dataset – more specifically, 
  # I want to see you split some data, perform a modification to the data, and then bring it back together
  
  state_summary <-
    df_2 %>% 
    separate(Geography, sep = ', ', into = c('County', 'State')) %>%
    group_by(State) %>% 
    mutate(`County Count` = 1) %>% 
    summarise(
      `average pop with HS Degree` = mean(HSDegree),
      `State Total RacesReported` = mean(RacesReported),
      `Number of Counties` = sum(`County Count`))
      
  
  stat.desc(state_summary)
  
  # Check distributions of the data
  state_summary %>%
  ggplot(aes(x = `average pop with HS Degree`)) + geom_density(fill = 'navy') +
    stat_function(fun = dnorm, args = list(
      mean = mean(state_summary$`average pop with HS Degree`),
      sd = sd(state_summary$`average pop with HS Degree`)
    )) + ggtitle("Distribution check to HS Degree")
  
  # -> distribution seems to be a normal distribution
  
  # Identify if there are any outliers
  boxplot(state_summary$`average pop with HS Degree`)
  boxplot.stats(state_summary$`average pop with HS Degree`)$out

  # -> Outliers are defined in the boxplot.stats function that determines 82 is an outlier
  # Create at least 2 new variables
  state_summary_addVars <- state_summary %>% 
    mutate(`RacesReported per County` = round(`State Total RacesReported` / `Number of Counties`, 2),
           `State Total RacesReported with HS Degree` = round(`State Total RacesReported` * (`average pop with HS Degree`/100),0))
  