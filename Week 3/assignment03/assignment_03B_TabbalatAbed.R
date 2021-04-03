# Assingment 3.2 Part 2
# Name: Tabbalat, Abed
# Class: DSC520
# Date: 04-01-2021

library(ggplot2)
library(pastecs)
library(psych)
options(scipen = 999)

df <- read.csv("data_3B.csv")
# What are the elements in your data (including the categories and data types)?
str(df)
summary(df)

# Please provide the output from the following functions: str(); nrow(); ncol()
str(df)
nrow(df)
ncol(df)

# Create a Histogram of the HSDegree variable using the ggplot2 package.
# Set a bin size for the Histogram.
# Include a Title and appropriate X/Y axis labels on your Histogram Plot.

ggplot(data = df, aes (x = HSDegree)) + geom_histogram(bins = 50, color = "black", fill = "Navy", alpha = 0.7) + 
  ggtitle("Histogram of Total Poulation with a highschool degree") +
  xlab ("% of Population for highschool degree") +
  ylab ("Count")

# Answer the following questions based on the Histogram produced:
# Based on what you see in this histogram, is the data distribution unimodal? 
    # -> Yes, histogram is unimodal since there is only one peak.
# Is it approximately symmetrical? 
    # -> No, it looks more negatively skewed.
# Is it approximately bell-shaped? 
    # -> No, bell shaped would be more symmetrical.
# Is it approximately normal? 
    # -> No, normal distribution would be more bell shaped rather than skewed.
# If not normal, is the distribution skewed? If so, in which direction? 
    # -> it is negatively skewed
# Include a normal curve to the Histogram that you plotted.
ggplot(data = df, aes (x = HSDegree)) + geom_histogram(bins = 50, color = "black", fill = "Navy", alpha = 0.7) +
  ggtitle("Histogram of Total Poulation with a highschool degree") +
  xlab ("% of Population for highschool degree") +
  ylab ("Count") +
  geom_density(aes(x = HSDegree), fill = "green")


ggplot(data = df, aes(x = HSDegree)) + geom_histogram(aes(y = ..density..), bins = 50, colour = "black", fill = "Navy", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(df$HSDegree), sd = sd(df$HSDegree)), color = "red") + 
  ggtitle("Histogram of Total Poulation with a highschool degree") +
  xlab ("% of Population for highschool degree") +
  ylab ("Count")

# Explain whether a normal distribution can accurately be used as a model for this data. 
    # -> This a clear negative skewed distribution, so a normal wouldn't work.

# Create a Probability Plot of the HSDegree variable.

ggplot(data = df, aes(x = HSDegree)) + geom_density(fill = "Navy") +
  ggtitle("Probability Distribution of Total Poulation with a highschool degree") +
  xlab ("% of Population for highschool degree") +
  ylab ("Density")

# Answer the following questions based on the Probability Plot:
# Based on what you see in this probability plot, is the distribution approximately normal? Explain how you know. 
    # -> No, this is not a normal distribution since the graph is not symmetrical, the first half is less steep than the second half.
# If not normal, is the distribution skewed? If so, in which direction? Explain how you know. 
    # -> This is negatively skewed since the beginning half is less steep than the last half of the graph.

# Now that you have looked at this data visually for normality, you will now quantify normality with numbers using the stat.desc() function. Include a screen capture of the results produced.
stats <- stat.desc(df$HSDegree, basic = FALSE, norm = TRUE)
stats

# ->    median               mean            SE.mean       CI.mean.0.95                var            std.dev 
# 88.699999999999989 87.632352941176464  0.438859785193231  0.867929607967526 26.193315904139435  5.117940592087743 
#     coef.var           skewness           skew.2SE           kurtosis           kurt.2SE         normtest.W 
# 0.058402409844264 -1.674766610458690 -4.030253997803690  4.352856462349686  5.273885336447716  0.877363543603847 
#     normtest.p 
# 0.000000003193634 

# In several sentences provide an explanation of the result produced for skew, kurtosis, and z-scores. In addition, explain how a change in the sample size may change your explanation?
  # -> Since the skewness is a negative value (-1.67) it is clear that the pile up values are on the right side of the curve which is also visible on the graph
  # -> Since the kurtosis is a positive value (4.35) it is determined that it is a pointy and heavy-tailed distribution as it is shown in the graph
  # -> Changing the sample size means changing the denominator of things that will cause impact on most calculations that are done by the stat.desc() function