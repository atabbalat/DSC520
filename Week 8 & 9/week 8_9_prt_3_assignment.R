# Assignment: ASSIGNMENT Part III
# Name: Tabbalat, Abed
# Date: 2021-05-06


# Work individually on this assignment. You are encouraged to collaborate on ideas and strategies pertinent to this assignment. 
# Data for this assignment is focused on real estate transactions recorded from 1964 to 2016 and can be found in Housing.xlsx. 
# Using your skills in statistical correlation, multiple regression, and R programming, 
# you are interested in the following variables: Sale Price and several other possible predictors.

# If you worked with the Housing dataset in previous week - you are in luck, you likely have already found any issues in the dataset and made the necessary transformations. 
# If not, you will want to take some time looking at the data with all your new skills and identifying if you have any clean up that needs to happen.
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(outliers)
library(lmtest)
library(car)

seeLevels <- function(column){
  levels(as.factor(column))
}

setwd("C:/Users/abedt/OneDrive/Desktop/PyCharm Projects/DSC 520/Week 8 & 9")

house_df_raw <- read_excel("housing.xlsx")
house_df <- house_df_raw %>%
  separate(`Sale Date`, into = c('Sale Year', 'Sale Month', 'Sale Day')) %>%
  mutate(
    `Sales Price per Sq Foot Lot` = round(`Sale Price` / sq_ft_lot, 0),
    `Sales Price per Sq Foot Living` = round(`Sale Price` / square_feet_total_living, 0),
    `Sale Date Mod` = ymd(paste(
      ifelse(`Sale Year` < year_built, year_built, `Sale Year`),
      `Sale Month`,
      `Sale Day`,
      sep = '-'
    )),
    `Age at Sale Mod` = year(`Sale Date Mod`) - year_built, 
    `Remodel Flag` = ifelse(year_renovated > 0, 1, 0)
  )


# Explain any transformations or modifications you made to the dataset
  # -> Separated date into 3 columns
  # -> Added additional variables to show the price of per square foot lot and price per square foot living
  # -> Added age of sale relative to the house being built
  # -> Added flag to identify if the house has been renovated or not

# Create two variables; one that will contain the variables Sale Price and Square Foot of Lot 
# (same variables used from previous assignment on simple regression) and one that will contain Sale Price 
# and several additional predictors of your choice. Explain the basis for your additional predictor selections.

housing_single_lm <- lm(`Sale Price` ~ sq_ft_lot, data = house_df)
housing_multiple_lm <-
  lm(
    `Sale Price` ~ sq_ft_lot + sale_reason + building_grade + square_feet_total_living + bedrooms + bath_full_count + bath_half_count + current_zoning + present_use + `Sales Price per Sq Foot Lot` + `Sales Price per Sq Foot Living` + `Age at Sale Mod`,
    data = house_df
  )

# Execute a summary() function on two variables defined in the previous step to compare the model results. 
# What are the R2 and Adjusted R2 statistics? Explain what these results tell you about the overall model. 
# Did the inclusion of the additional predictors help explain any large variations found in Sale Price?
summary(housing_single_lm)
summary(housing_multiple_lm)

  # -> Single: R2 = 0.01435  Adjusted R2 = 0.01428
  # -> Multiple: R2 = 0.8398  Adjusted R2 = 0.8394
  # -> The closer R2 and Adjusted R2 are to 1, the more accurate which tells me that the single regression is far from being accurate while multiple is fairly accurate.


# Considering the parameters of the multiple regression model you have created. 
# What are the standardized betas for each parameter and what do the values indicate?
housing_lm_betas <- data.frame(housing_multiple_lm$coefficients)
  
  # -> The values indicate that each variable impacts the regression model with the degree of error it contains. y = mx + b and beta is b that determines how are we going to solve for why given that m is slope and x is the axis.

# Calculate the confidence intervals for the parameters in your model and explain what the results indicate.
conf_interval <- confint(housing_multiple_lm, level = 0.95)
  
  # -> the confidence interval shows us what the model is with the variable errors at 95% confidence, meaning we are 95% confident that our model parameters will fall within the given range.

# Assess the improvement of the new model compared to your original model (simple regression model) 
# by testing whether this change is significant by performing an analysis of variance.
anova(housing_single_lm, housing_multiple_lm)
  
  # -> The P value is significantly low which means adding additional variables in from single to multiple improved the accuracy to the model.

# Perform casewise diagnostics to identify outliers and/or influential cases, 
# storing each function's output in a dataframe assigned to a unique variable name.

housing2 <- house_df

housing2$residuals <- resid(housing_multiple_lm)
housing2$standardized.residuals <- rstandard(housing_multiple_lm)
housing2$studentized.residulas <- rstudent(housing_multiple_lm)

# Calculate the standardized residuals using the appropriate command, 
# specifying those that are +-2, storing the results of large residuals in a variable you create.
housing2$standard_resid <- housing2$standardized.residuals >2 | housing2$standardized.residuals < -2

# Use the appropriate function to show the sum of large residuals.
large_resid <- housing2 %>% 
  group_by(standard_resid) %>% 
  summarise(standardized.residuals = sum(standardized.residuals)) %>% 
  filter(standard_resid == T)

# Which specific variables have large residuals (only cases that evaluate as TRUE)?
large_resid_var <- housing2 %>% 
  group_by(standard_resid) %>% 
  filter(standard_resid == T)

# Investigate further by calculating the leverage, cooks distance, and covariance rations. Comment on all cases that are problematics.
housing2$cooks.distance <- cooks.distance(housing_multiple_lm)
housing2$dfbeta <- dfbeta(housing_multiple_lm)
housing2$dffit <- dffits(housing_multiple_lm)
housing2$leverage <- hatvalues(housing_multiple_lm)
housing2$covaraince.ratios <- covratio(housing_multiple_lm)

# Perform the necessary calculations to assess the assumption of independence and state if the condition is met or not.
assump_independence <- dwt(housing_multiple_lm)

  # -> Condition is met as autocorrelation is greater than 0

# Perform the necessary calculations to assess the assumption of no multicollinearity and state if the condition is met or not.
multi <- vif(housing_multiple_lm)
  
  # -> Conditions are met as there are no values greater than 10

# Visually check the assumptions related to the residuals using the plot() and hist() functions. 
# Summarize what each graph is informing you of and if any anomalies are present.
plot(housing_multiple_lm)

ggplot(housing2, aes(x = housing2$dffit, y = housing2$standardized.residuals)) + geom_point() + xlab("Fitted Values") + ylab("Standardized Residuals") + ggtitle("Fitted vs Standardized Residuals")

ggplot(housing2, aes(x = housing2$standardized.residuals)) + geom_histogram(binwidth = 0.25, color = "Black", fill = "Steelblue") + xlab("Standardized Residuals") + ggtitle("Standardized Residuals Histogram")
# Overall, is this regression model unbiased? If an unbiased regression model, 
# what does this tell us about the sample vs. the entire population model?
  # -> The model seems to be unbiased as it met all the conditions that are present in the model.