# Analysis for Exam 1 Stat 121 Data

# Load libraries and data
library(tidyverse)
library(kableExtra)
data <- vroom::vroom("filename.csv")

# Clean up text issues in data 
cleaned_data <- data %>%
  mutate(across(everything(), ~ str_remove(., ":dropped*"))) |> 
  rename(FirstV = "Exam 1, V1", 
         SecondV = "Exam 1, V2",
         Net.ID = "ID") 

# Goal is to find people that took both versions - Step 1. Filter rows where the 2nd version 
# does not have 0. Step 2. Filter those that did not take the 1st exam
both_taken <- cleaned_data |> 
  filter(!SecondV == 0) |> 
  filter(!FirstV == 0)

# 174 took both exams, how many people did better on the second version vs. the first
View(both_taken)

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

# Paired T-Test to see if there is a real difference, there is not at 0.05 level, maybe slight if anything 
t.test(x = final$FirstV, y=final$SecondV, alternative = "two.sided", mu = 0, 
       paired = TRUE, conf.level = 0.95)


# Interestingly, the average of the differences suggests, there is a noticeable average
# difference indicating that SecondV might be harder. The paired-t test on the 
# other hand,  does not have enough statistical evidence to reject the null hypothesis.  
# The p-value suggests that the observed difference could be due to random chance rather than a true effect.
#-------------------------------------------------------------------------------
# Load more data
energy_exam <- vroom::vroom(delim =",",  "Student Answers for Exam 1 - V1")
alz_exam <- vroom::vroom(delim =",","Student Answers for Exam 1 - V2")

# Select needed columns and filter out nas 
V1_exam_data <- V1_exam |> 
  dplyr::select("ID", Score) |> 
  rename(FirstV = Score, Net.ID = "ID") |> 
  na.omit()

# Do same to other exam
V2_exam_data <- V2_exam |> 
  dplyr::select("ID", Score) |> 
  rename(SecondV = Score, Net.ID = "ID") |> 
  na.omit()

# Join both datasets by id, want to keep everything in there for now
joined <- full_join(x = V1_exam_data, y = V2_exam_data)

# Goal is to find people that took both versions - Step 1. Filter rows where the 2nd version 
# does not have 0. Step 2. Filter those that did not take the 1st exam
both_taken <- joined |> 
  filter(!SecondV == "NA") |> 
  filter(!SecondV == 0) |> 
  filter(!FirstV == 0) |> 
  filter(!FirstV == "NA")

# 686 people who took both

final <- both_taken |> 
  group_by(Net.ID) |> 
  mutate(FirstV = as.numeric(FirstV),
         SecondV = as.numeric(SecondV), 
         better_or_same = sum(SecondV >= FirstV),
         exact_same = sum(SecondV == FirstV))

# Get total count for those results
final_summary <- final |> 
  ungroup() |> 
  summarize(count_better = sum(better_or_same == 1),
            count_worse = sum(better_or_same == 0),
            count_same = sum(exact_same == 1),
            mean_better = mean(better_or_same), 
            average_of_differences = mean(SecondV-FirstV))

# Table of above
final_summary |> 
  kbl() |> 
  kable_minimal()


# Paired T-Test to see if there is a real difference, there is not at 0.05 level, maybe slight if anything 
t.test(x = final$FirstV, y=final$SecondV, alternative = "two.sided", mu = 0, 
       paired = TRUE, conf.level = 0.95)
#-------------------------------------------------------------------------------

# How much worse did the students do on the second exam? 
# We know that anybody who did worse is 0 in better_second
energy_exam_data <- final |> 
  filter(better_second == 0) |> 
  mutate(Difference = FirstV-SecondV)
