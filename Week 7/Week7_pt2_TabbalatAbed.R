# Assignment: Week 7 Part 2
# Name: Tabbalat, Abed
# Date: 2021-04-29

library(ggplot2)
library(GGally)
library(ggm)

setwd("C:/Users/abedt/OneDrive/Desktop/PyCharm Projects/DSC 520/Week 7")

# As a data science intern with newly learned knowledge in skills in statistical correlation and R programming, 
# you will analyze the results of a survey recently given to college students. 
# You learn that the research question being investigated is: "Is there a significant relationship between the amount of time spent reading and the time spent watching television?" 
# You are also interested if there are other significant relationships that can be discovered? The survey data is located in this StudentSurvey.csv file.
student_df <- read.csv("student-survey.csv")
# Use R to calculate the covariance of the Survey variables and provide an explanation of why you would use this calculation and what the results indicate.
cov_student_p <- cov(student_df, use = "everything", method = 'pearson')
GGally::ggpairs(as.data.frame(cov_student_p))
#-> Using the pearson method would be the answer here as the data seem to be at a normal scale for each variable, the results indicate the following:
  #TimeReading: TimeTV -> negative covariance, Happiness -> negative covariance, Gender -> negative covariance (minimal, close to zero)
  #TimeTV: TimeReading -> negative covariance, Happiness -> positive covariance, Gender -> positive covariance (minimal, close to zero)
  #Happiness: TimeReading -> negative covariance, TimeTV -> positive covariance, Gender -> positive covariance
  #Gender: TimeReading -> negative covariance (minimal, close to zero), TimeTV -> positive covariance (minimal, close to zero), Happiness -> positive covariance

# Examine the Survey data variables. What measurement is being used for the variables?
# Explain what effect changing the measurement being used for the variables would have on the covariance calculation. 
# Would this be a problem? Explain and provide a better alternative if needed.
    # Time is the measurement used for reading and watching TV, Happiness seems to be a percentage metric or a range from 0-100, gender is a boolean of 1 and 0 (doesnt determine which is which).
    # If we add an additional variable that determines the type of books and TV shows, this will impact the covariance calculation, as TV shows could be done for leisure while books could be class materials instead of entertainment books.


# Choose the type of correlation test to perform, explain why you chose this test, and make a prediction if the test yields a positive or negative correlation?
    # Using pearson method will determine if we have positive or negative correlation between the variables, I believe reading vs tv will have a negative correlation.
# Perform a correlation analysis of:

  # All variables
cor_all <- cor(student_df, use = 'everything', method = 'pearson')
  # A single correlation between two a pair of the variables
cor_two_paid <- cor(student_df$Happiness, student_df$TimeTV, method = "pearson")
  # Repeat your correlation test in step 2 but set the confidence interval at 99%
cor_two_99 <- cor.test(student_df$Happiness, student_df$TimeTV, method = "pearson", conf.level = 0.99)
  # Describe what the calculations in the correlation matrix suggest about the relationship between the variables. Be specific with your explanation.
GGally::ggpairs(as.data.frame(cor_all))
  # Time Reading: It is determined that TimeReading has a negative correlation with TimeTV and Happiness, Gender has a very small negative correlation that can be determined as no correlation. Meaning, the more students read the less happy they are, and watching less tv regardless of gender.
  # Time TV: It is determined that TimeTV has a negative correlation with reading, a positive correlation with happiness, and a very small negative correlation that can be determined as no correlation. Meaning, the more tv students watch the happier they are, and less time to read regardless of the gender.
  # Happiness: It is determined that the more time spent on reading their happiness drops, and the more time they spend watching tv the happier they get regardless of gender.
# Calculate the correlation coefficient and the coefficient of determination, describe what you conclude about the results.
corr_coef <- cor(student_df, use = "everything", method = "spearman")
GGally::ggpairs(as.data.frame(corr_coef))
corr_det <- cor(student_df, use = "everything", method = "pearson")^2
GGally::ggpairs(as.data.frame(corr_det))
  # Correlation coefficient: Applying the cooficient results in showing the correlation without a confidence interval, the highest result shows TimeReading vs TimeTV at -0.9.
  # Correlation of determination: Applying the correlation of determination results in determining the variability of the correlation, the highest is between TimeReading and TimeTV at 0.7789 (77.89% of students are impacted by the correlation)
# Based on your analysis can you say that watching more TV caused students to read less? Explain.
    # Yes, there is a clear negative correlation between watching tv and reading, watching more tv will result in less reading.

# Pick three variables and perform a partial correlation, documenting which variable you are "controlling". 
# Explain how this changes your interpretation and explanation of the results.
student_df2 <- student_df[, c("TimeReading", "TimeTV", "Happiness")]
par_cor <- pcor(c("TimeReading", "TimeTV", "Happiness"), var(student_df2))
    # In the above analysis on watching tv vs reading correlation, happiness is a factor as it determines students are doing what makes them happy. Placing happiness as the controlled variable will remain a negative correlation between watching tv and reading, as there are so many hours during the day and it is working as a substitute.
