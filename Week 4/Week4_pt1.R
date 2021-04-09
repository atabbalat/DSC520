
# Test Scores

# A professor has recently taught two sections of the same course with only one difference between the sections. 
# In one section, he used only examples taken from sports applications, and in the other section, he used examples taken from a variety of application areas. 
# The sports themed section was advertised as such; so students knew which type of section they were enrolling in. 
# The professor has asked you to compare student performance in the two sections using course grades and total points earned in the course. 
# You will need to import the Scores.csv dataset that has been provided for you.
# Use the appropriate R functions to answer the following questions:

library(ggplot2)
library(plyr)
library(dplyr)
library(extrafont); loadfonts(device = "win")
library(pastecs)
library(car)

themeAbed <- function(axis_angle=0, vjust_value=0){
  #make sure ot load extra fonts -> library(extrafont);loadfonts(device = "win")
  theme(
    # color plot background
    plot.background = element_rect(fill = '#F0F8FF'),
    # add border
    panel.border = element_rect(colour = 'navy', fill = NA, linetype = 1),
    # color panel background
    panel.background = element_rect(fill = '#778899'),
    # plot font type
    text = element_text(family = 'Century Schoolbook'),
    # modify grid
    panel.grid.major.x = element_line(colour = 'grey42', linetype = 1, size = 0.25, lineend = 'butt'),
    panel.grid.minor.x = element_line(colour = 'grey42', linetype = 1, size = 0.10, lineend = 'butt'),
    panel.grid.major.y = element_line(colour = "grey42", linetype = 1, size = 0.25, lineend = 'butt'),
    panel.grid.minor.y = element_line(colour = 'grey42', linetype = 1, size = 0.10, lineend = 'butt'),
    # Modify axis
    axis.text = element_text(colour = "navy", face = "bold"),
    axis.text.x = element_text(angle = axis_angle, vjust = vjust_value),
    axis.title = element_text(colour = "navy"),
    axis.ticks = element_line(colour = "grey20"),
    axis.title.x = element_text(face = 'bold'),
    axis.title.y = element_text(face = 'bold'),
    # Modify Legend
    legend.position = "bottom",
    legend.text = element_text(colour = 'navy', face = 'bold'),
    legend.title = element_text(colour = 'navy', face = 'bold'),
    legend.background = element_blank(),
    # Plot title & subtitle
    plot.title = element_text(colour = 'navy', face = 'bold'),
    plot.subtitle = element_text(colour = 'navy', face = 'bold'),
    # Facets
    strip.background = element_rect(colour = "navy", fill = 'navy'),
    strip.text.x = element_text(face = "bold", color = 'white'),
  )
}



setwd("C:/Users/abedt/OneDrive/Desktop/PyCharm Projects/DSC 520/Week 4")

df <- read.csv("scores.csv")


# What are the observational units in this study?
  # -> Count, Score, Section
str(df)

# Identify the variables mentioned in the narrative paragraph and determine which are categorical and quantitative?
  # -> Count: quantitative
  # -> Score: quantitative
  # -> Section: categorical

# Create one variable to hold a subset of your data set that contains only the Regular Section and one variable for the Sports Section.
  regular <- df %>%
    filter(Section == "Regular") %>%
    group_by(Score) %>%
    summarise(Count = sum(Count))

  sports <- df %>%
    filter(Section == "Sports") %>%
    group_by(Score) %>%
    summarise(Count = sum(Count))
  
# Use the Plot function to plot each Sections scores and the number of students achieving that score. 
# Use additional Plot Arguments to label the graph and give each axis an appropriate label. Once you have produced your Plots answer the following questions:
  # -> using plot function for regular
  plot(
    x = regular$Score,
    y = regular$Count,
    type = 'b',
    xlab = 'Score',
    ylab = 'Student Count',
    main = 'Regular Section - Student Score count')
  
  # -> using plot function for sports
  plot(
    x = sports$Score,
    y = sports$Count,
    type = 'b',
    xlab = 'Score',
    ylab = 'Student Count',
    main = 'Sports Section - Student Score count')
  
  # -> using both charts side by side for comparison
  df %>%
    group_by(Section, Score) %>%
    summarise(Count = sum(Count)) %>%
    ggplot(aes(x = Score, y = Count)) +
    geom_bar(
      stat = 'identity',
      size = 1,
      color = 'black',
      fill = 'navy',
      alpha = .75
    ) +
    facet_wrap(.~ Section) +
    themeAbed() +
    ggtitle('Student Score Count by Course Section')
  
  # -> this is the chart that will be used for the following questions that shows the distributions.
  df %>%
    group_by(Section, Score) %>%
    summarise(Count = sum(Count)) %>% ggplot(aes(x = Score)) +
    geom_density(fill = "Navy") +
    facet_wrap(. ~ Section) +
    stat_function(fun = dnorm, args = list(mean = mean(df$Score), sd = sd(df$Score))) +
    themeAbed()

  # Comparing and contrasting the point distributions between the two section, looking at both tendency and consistency: 
  # Can you say that one section tended to score more points than the other? Justify and explain your answer.
    # -> Yes, it is clear that the regular course produced higher scores than the sports, we can tell the concentration of the chart in the regular
    #   sits in the higher scores while the sports is more scattered and contains lower scores than the regular

  # Did every student in one section score more points than every student in the other section? If not, explain what a statistical tendency means in this context.
    stat.desc(regular)
    stat.desc(sports)
    # -> No, looking at the mean and median will indicate that some students in each classes did better than other students.
    
    
  # What could be one additional variable that was not mentioned in the narrative that could be influencing the point distributions between the two sections?
    # -> Time of class could be a variable that can influence the scores, hypothetically, if the sports class was at night vs regular class was in the morning
    #   students could be focusing more in the morning rather at night.