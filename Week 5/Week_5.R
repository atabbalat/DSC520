# Assingment 5
# Name: Tabbalat, Abed
# Class: DSC520
# Date: 04-15-2021


# Using either the same dataset(s) you used in the previous weeks’ exercise or a brand-new dataset of your choosing, 
# perform the following transformations 
# (Remember, anything you learn about the Housing dataset in these two weeks can be used for a later exercise!)
library(tidyr)
library(pastecs)
library(plyr)
library(dplyr)
library(ggplot2)
library(purrr)

setwd("C:/Users/abedt/OneDrive/Desktop/PyCharm Projects/DSC 520/Week 5")
df_1 <- read.csv("acs-14.csv")

  # Using the dplyr package, use the 6 different operations to analyze/transform the data - GroupBy, Summarize, Mutate, Filter, Select, and Arrange 
  #– Remember this isn’t just modifying data, you are learning about your data also – so play around and start to understand your dataset in more detail
state_summary <- df_1 %>% 
  separate(Geography, sep = ', ', into = c('County', 'State')) %>%
  group_by(State) %>% 
  mutate(`County Count` = 1) %>% 
  summarise(
    `average pop with HS Degree` = mean(HSDegree),
    `State Total RacesReported` = mean(RacesReported),
    `Number of Counties` = sum(`County Count`),
    PopGroupID = max(PopGroupID)) %>% 
  filter(`Number of Counties` > 1)

state_summary_dplyr <- state_summary %>% 
  select(-PopGroupID) %>% 
  arrange(desc(`average pop with HS Degree`))
    

  #Using the purrr package – perform 2 functions on your dataset.  You could use zip_n, keep, discard, compact, etc.
timesFive <- function(.x) {
  return(.x * 5)
}
map(.x = state_summary$PopGroupID,
    .f = timesFive)
timeTwo <- function(.x) {return(.x * 2)}

modify_if(.x = state_summary_dplyr$`Number of Counties`, .p = function(x) x > 10, .f = timeTwo)

  # Use the cbind and rbind function on your dataset
df_2 <- read.csv("state.csv") %>% 
  separate(Geography, sep = ', ', into = c('County', 'State')) %>%
  group_by(State) %>% 
  mutate(`County Count` = 1) %>% 
  summarise(
    `average pop with HS Degree` = mean(HSDegree),
    `State Total RacesReported` = mean(RacesReported),
    `Number of Counties` = sum(`County Count`),
    PopGroupID = max(PopGroupID),
    `above 80 HS Degree` = sum(Flag)) %>% 
  filter(`Number of Counties` > 1)

mod_state_summary <- 
  select(df_2, -c(`above 80 HS Degree`, `PopGroupID`)) %>% 
  rbind(state_summary_dplyr) %>% 
  unique.data.frame() %>% 
  cbind(select(df_2, c(`above 80 HS Degree`)))

  # Split a string, then concatenate the results back together
df_3 <- read.csv("state.csv") %>% 
  separate(Geography, sep = ', ', into = c('County', 'State')) %>% 
  mutate(concState = paste(County, State, sep = '-'))

