########## Drew Wilimitis ##########
########## June 29th, 2018 ##########
########## EDUCAUSE Assignment 3 Submission ##########

# Importing Dependencies 
library(readr);library(lubridate);library(dplyr);library(stringr);library(ggplot2);library(plotly)

# Setting up workspace
rm(list = ls())
gc()

# Loading data
cds_data <- read_csv("C:/Users/Drew/Desktop/Career Search/EDUCAUSE Interview/CDSdata.csv")

# Exploring the data
head(cds_data)
str(cds_data)
glimpse(cds_data)

# Data frames used to clean CDS data
total_spend <- cds_data[, c(1, 7, 16, 25, 35)]
spend_2017 <- cds_data[, c(1, 7:15)]

#__ IMPUTING VALUES FOR MISSING IT EXPENDITURE DATA __#
##  1.) Create new total by summing the spend categories
##  2.) Use the value from Question 12, 2016 IT expenditure total (The survey states these values should be identical)
##  3.) If reasonable, use the mean within the Carnegie groups

#__ CLEANING SPEND 2017 DATA FRAME __#
spend_2017[is.na(spend_2017)] <- 0
spend_2017[spend_2017 == '-'] <- 0

# Need to delete commas, -, $, spaces, etc.
# Only keep values like 0-9 using Reg. Expressions
spend_2017 = as.data.frame(sapply(spend_2017, function(x) gsub('[^0-9|.]', '', x)), stringsAsFactors = FALSE)

# Read out 11 significant digits
options(digits = 11)

# Coerce to numeric type
spend_2017 <- mutate_all(spend_2017, function(x) as.numeric(as.character(x)))

# Summing category spend
spend_2017 <- spend_2017 %>%
  mutate(sum_spend = rowSums(spend_2017[, 3:10]))

#__ CLEANING TOTAL SPEND DATA FRAME __#
total_spend[is.na(total_spend)] <- 0
total_spend[total_spend == '-'] <- 0

# Make column names easier
colnames(total_spend)[2] <- "total_2017"
colnames(total_spend)[4] <- "total_2016"

total_spend <- as.data.frame(sapply(total_spend, function(x) gsub('[^0-9|.]', '', x)), stringsAsFactors = FALSE)

total_spend <- mutate_all(total_spend, function(x) as.numeric(as.character(x)))

# Replace missing values with item from Question 12
total_spend[total_spend$total_2017 == 0, "total_2017"] <- total_spend[total_spend$total_2017 == 0, "total_2016"] 

#__ CLEANING CDS DATA FRAME __#
spend_clean <- spend_2017 %>%
  inner_join(total_spend, by = c("PublicID" = "PublicID"))

colnames(spend_clean)[2] <- "orig_spend"

# Impute 
spend_clean[spend_clean$orig_spend == 0, "orig_spend"] <- spend_clean[spend_clean$orig_spend == 0, "sum_spend"]
spend_clean[spend_clean$orig_spend == 0, "orig_spend"] <- spend_clean[spend_clean$orig_spend == 0, "total_2016"]

cds_clean <- cds_data
cds_clean$`2017 ITOSFQ10: Expenditures - Total` <- spend_clean$orig_spend
cds_clean$`2017 ITOSFQ10: Expenditures - Total`[cds_clean$`2017 ITOSFQ10: Expenditures - Total` <= 1] <- 0
cds_clean$`2016 Institutional Expenditure`[cds_clean$`2016 Institutional Expenditure` == '-'] <- 0
cds_clean$`2016 Institutional Expenditure` <- as.numeric(cds_clean$`2016 Institutional Expenditure`)

cds_clean$IT_spend_per_studentFTE_updated <- cds_clean$`2017 ITOSFQ10: Expenditures - Total` / cds_clean$`2016 Student FTE`
cds_clean$percent_IT_spend <- (cds_clean$`2017 ITOSFQ10: Expenditures - Total` / cds_clean$`2016 Institutional Expenditure`) * 100

# Comparing medians to new computed values
clean_median_per_student <- median(cds_clean$IT_spend_per_studentFTE_updated[cds_clean$IT_spend_per_studentFTE_updated != 0])
clean_median_percent_IT <- median(cds_clean$percent_IT_spend[cds_clean$percent_IT_spend != 0])

cds_clean$`2017 CM: Total central IT spending per Student FTE`[cds_clean$`2017 CM: Total central IT spending per Student FTE` == '-'] <- 0
cds_clean$`2017 CM: Total central IT spending as a percentage of institutional expenses`[cds_clean$`2017 CM: Total central IT spending as a percentage of institutional expenses` == '-'] <- 0

cds_clean$`2017 CM: Total central IT spending per Student FTE` <- 
  as.numeric(cds_clean$`2017 CM: Total central IT spending per Student FTE`)

cds_clean$`2017 CM: Total central IT spending as a percentage of institutional expenses` <- 
  as.numeric(cds_clean$`2017 CM: Total central IT spending as a percentage of institutional expenses`)

median_per_student <- 
  median(cds_clean$`2017 CM: Total central IT spending per Student FTE`[cds_clean$`2017 CM: Total central IT spending per Student FTE` != 0])

median_percent_IT <- 
  median(cds_clean$`2017 CM: Total central IT spending as a percentage of institutional expenses`[cds_clean$`2017 CM: Total central IT spending as a percentage of institutional expenses` != 0])

# Compare clean vs. unclean medians within the Carnegie groups
group_median <- cds_clean[, c(1:7, 46, 47)]
group_median[group_median == 0] <- NA
group_median <- group_median %>%
  group_by(`Carnegie Classification`) %>%
  mutate(group_per_student = median(`2017 CM: Total central IT spending per Student FTE`, na.rm = TRUE),
         group_percent_IT = median(`2017 CM: Total central IT spending as a percentage of institutional expenses`, na.rm = TRUE),
         clean_group_per_student = median(IT_spend_per_studentFTE_updated, na.rm = TRUE),
         clean_group_percent_IT = median(percent_IT_spend, na.rm = TRUE)) %>%
  select(`Carnegie Classification`, group_per_student, group_percent_IT, clean_group_per_student, clean_group_percent_IT) %>%
  distinct() 

group_median <- group_median[, c(1, 10, 11, 12, 13)]
cds_clean <- cds_clean %>%
  left_join(group_median, by = c('PublicID' = 'PublicID'))

#__ RESULTS/COMMENTS __#

# Medians from original, uncleaned data: 
# The median Total central IT spending per Student FTE = 1287.875
# The median Total central IT spending as a percentage of institutional expenses = 4.64

# Medians from clean data calculation:
# The median Total central IT spending per Student FTE = 1137.2478386
# The median Total central IT spending as a percentage of institutional expenses = 4.2232454387


# The difference in these median calculations seems to be a reasonably sized difference, given the
# fact that I imputed only a small fraction of the given data points used in the calculations for the
# clean data. It's not immediately obvious why the clean data provided smaller median calculations. 
# Perhaps the cleaned data was somehow biased to be an underestimate for missing values, or the
# uncleaned data was just more affected by larger outliers since there were fewer data points in the uncleaned
# data. 

# The difference between the cleaned and uncleaned data in the medians at the group level 
# was the largest for the group "Other U.S" and it was the smallest for the group "B.A Pub". 
# This makes sense as there were only a few observations in the "Other U.S" group, so therefore
# this median calculation would be sensitive to changes in the input values. The "B.A Pub" group
# would be public universities that offer bachelor's degrees but very few or no master's degrees, 
# so it makes sense that this is a larger group and less sensitive to change.


# ADDITIONAL DATA SOURCES

# In order to improve this data cleaning methodology and compute more accurate values for columns E and F, it would
# be beneficial to have access to additional data on these specific educational institutions. Ideally, we would be
# able to fill in all the NA values in a statistically accurate way. For example, if we had data that could be used to
# break the education institutes into smaller groups, we would likely have more accurate mean-imputation values. Some examples of
# potentially useful data would be specific student demographics like the number of undergrads vs. graduate students at 
# these schools, the geographical locations, or more financial data like revenue or endowment size. This would open
# up several possibilities for better detecting outliers among the data and even using statistical modeling/prediction 
# to determine the most accurate values to impute.


