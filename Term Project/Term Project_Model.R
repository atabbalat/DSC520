


library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(outliers)
library(lmtest)
library(car)

setwd("C:/Users/atabbala/OneDrive - Bankers Financial Corporation/Desktop/Temp/DSC 520/Term Project")

df_claim_raw <- read_excel("Set 1_Claim Details.xlsx")
df_exposure_raw <- read.csv("Set 2_Exposure File.csv")
df_score_raw <- read_excel("Set 3_CAT Score.xlsx")

# Step 1: Data cleanup (summarize all data by product state)

# consistent policy labels, join needed columns, add premiums to total column, add additional variables, group by then summarize, calculate LR, then perform a glm, and a covariance function. 


df_claim <- df_claim_raw %>%
  separate(
    PolicyLabel,
    into = c(
      "PolicyState",
      "PolicyNumber",
      "PolicyDigit",
      "PolicyOccurance"
    ),
    sep = "-"
  ) %>%
  mutate(
    PolicyOccurance = as.numeric(PolicyOccurance),
    WPolicyNum = paste(PolicyState, PolicyNumber, PolicyDigit, PolicyOccurance, sep = "-")
  ) %>% 
  group_by(WPolicyNum) %>% 
  summarise(`Total Incurred` = sum(`Total Incurred`), )

df_score <- df_score_raw %>% 
  group_by(WPolicyNum) %>% 
  summarise(`Total Ceded Premium` = sum(`Total Ceded Premium`))

df_exposure <- df_exposure_raw %>%
  left_join(df_claim) %>%
  left_join(df_score) %>%
  mutate(
    `Total Incurred` = ifelse(is.na(`Total Incurred`), 0, `Total Incurred`),
    `Total Ceded Premium` = ifelse(is.na(`Total Ceded Premium`), 0, `Total Ceded Premium`),
    `Total Premium` = Written.Premium + Hurricane.Premium + Earthquake.Prem + Liability.Premium)

fixed_expense <- 0.3

#Regression Models

multiple_reg_exposure_incurred <-
  lm(
    `Total Incurred` ~ `Total Premium` + `Total Ceded Premium` + Product + Property.State + Year.Built + Total.Square.Feet + Building.Exposure + Other.Structures.Exposure + Contents.Exposure + Loss.of.Use.Exposure + Roof.Age + Customer_Segment,
    data = df_exposure
  )


multiple_reg_cat <- lm(
  `Total Ceded Premium` ~ `Total Premium` + Product + Property.State + Year.Built + Total.Square.Feet + Building.Exposure + Other.Structures.Exposure + Contents.Exposure + Loss.of.Use.Exposure,
  data = df_exposure
)

summary(multiple_reg_exposure_incurred)
summary(multiple_reg_cat)

# Summaries: By product state, construction, and customer segment
df_summary_product <- df_exposure %>% 
  group_by(Product, Property.State) %>% 
  summarise(`Total Premium` = sum(`Total Premium`), `Total Incurred` = sum(`Total Incurred`), `Total Ceded Premium` = sum(`Total Ceded Premium`)) %>% 
  mutate(LossRatio = `Total Incurred` / `Total Premium`, CATRatio = `Total Ceded Premium` / `Total Premium`, CombinedRatio = LossRatio + CATRatio + fixed_expense)

df_summary_product %>%
  ggplot(aes(x = Property.State, y = CombinedRatio)) + geom_bar(aes(group = Product, fill = Product),
                                                                stat = "identity",
                                                                position = "dodge")

df_summary_product %>%
  ggplot(aes(x = Property.State, y = LossRatio)) + geom_bar(aes(group = Product, fill = Product),
                                                                stat = "identity",
                                                                position = "dodge")

df_summary_product %>%
  ggplot(aes(x = Property.State, y = CATRatio)) + geom_bar(aes(group = Product, fill = Product),
                                                                stat = "identity",
                                                                position = "dodge")
df_summary_construction <- df_exposure %>% 
  group_by(Construction.Desc) %>% 
  summarise(`Total Premium` = sum(`Total Premium`), `Total Incurred` = sum(`Total Incurred`), `Total Ceded Premium` = sum(`Total Ceded Premium`)) %>% 
  mutate(LossRatio = `Total Incurred` / `Total Premium`, CATRatio = `Total Ceded Premium` / `Total Premium`, CombinedRatio = LossRatio + CATRatio + fixed_expense)

df_summary_construction %>%
  ggplot(aes(x = Construction.Desc, y = CombinedRatio)) + geom_bar(
                                                                stat = "identity") + theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1))

df_summary_construction %>%
  ggplot(aes(x = Construction.Desc, y = LossRatio)) + geom_bar(
    stat = "identity") + theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1))

df_summary_construction %>%
  ggplot(aes(x = Construction.Desc, y = CATRatio)) + geom_bar(
    stat = "identity") + theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1))

df_summary_segmentation <- df_exposure %>% 
  group_by(Customer_Segment) %>% 
  summarise(`Total Premium` = sum(`Total Premium`), `Total Incurred` = sum(`Total Incurred`), `Total Ceded Premium` = sum(`Total Ceded Premium`)) %>% 
  mutate(LossRatio = `Total Incurred` / `Total Premium`, CATRatio = `Total Ceded Premium` / `Total Premium`, CombinedRatio = LossRatio + CATRatio + fixed_expense)

df_summary_segmentation %>%
  ggplot(aes(x = Customer_Segment, y = CombinedRatio)) + geom_bar(
    stat = "identity") + scale_y_continuous(labels = scales::percent) + theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1))

df_summary_segmentation %>%
  ggplot(aes(x = Customer_Segment, y = LossRatio)) + geom_bar(
    stat = "identity") + scale_y_continuous(labels = scales::percent) + theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1))

df_summary_segmentation %>%
  ggplot(aes(x = Customer_Segment, y = CATRatio)) + geom_bar(
    stat = "identity") + scale_y_continuous(labels = scales::percent) + theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1))
