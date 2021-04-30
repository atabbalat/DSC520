# Assignment: ASSIGNMENT 5
# Name: Tabbalat, Abed
# Date: 2021-04-29

## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/abedt/OneDrive/Desktop/PyCharm Projects/DSC 520/Week 7")

## Load the `data/r4ds/heights.csv` to
heights_df <- read.csv("heights.csv")

## Using `cor()` compute correclation coefficients for
## height vs. earn
cor(x = heights_df$height, y = heights_df$earn)
### age vs. earn
cor(x = heights_df$age, y = heights_df$earn)
### ed vs. earn
cor(x = heights_df$ed, y = heights_df$earn)

## Spurious correlation
## The following is data on US spending on science, space, and technology in millions of today's dollars
## and Suicides by hanging strangulation and suffocation for the years 1999 to 2009
## Compute the correlation between these variables
tech_spending <- c(18079, 18594, 19753, 20734, 20831, 23029, 23597, 23584, 25525, 27731, 29449)
suicides <- c(5427, 5688, 6198, 6462, 6635, 7336, 7248, 7491, 8161, 8578, 9000)
cor(x = tech_spending, y = suicides)
