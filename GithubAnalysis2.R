# Analysis for Exam 1 Stat 121 Data
setwd("/Users/alexiacw11/Desktop/Fall_24/ExamAnalysis/Exam2Analysis")

# Load libraries and data
library(tidyverse)
library(kableExtra)
data <- vroom::vroom("filename")

# Clean up text issues in data 
cleaned_data <- data %>%
  mutate(across(everything(), ~ str_remove(., ":dropped*"))) |> 
  rename(FirstV = "Exam 2 - V1", 
         SecondV = "Exam 2 - V2",
         Net.ID = "Net ID") 

# Goal is to find people that took both versions, 160 in total
both_taken <- cleaned_data |> 
  filter(!SecondV == 0) |> 
  filter(!FirstV == 0)

final <- both_taken |> 
  group_by(Net.ID) |> 
  mutate(FirstV = as.numeric(FirstV),
         SecondV = as.numeric(SecondV), 
         better_or_same = sum(SecondV >= FirstV))

# Get total count for those results
final_summary <- final |> 
  ungroup() |> 
  summarize(count_better = sum(better_or_same == 1),
            count_worse = sum(better_or_same == 0),
            mean_better = mean(better_or_same), 
            average_of_differences = mean(SecondV-FirstV),
            mean_SecondV = mean(SecondV), 
            mean_FirstV = mean(FirstV))

# Table of above
final_summary |> 
  kbl() |> 
  kable_minimal()

# Paired T-Test to see if there is a real difference, there is at the 0.05 level
t.test(x = final$FirstV, y=final$SecondV, alternative = "two.sided", mu = 0, 
       paired = TRUE, conf.level = 0.95)

# Statistically important, but not practically!