require(tidyverse)


#For Lasso regression 

#Read in AYCET gameplay data and DCCS data
AYCET_gameplay_aggregated <- read_csv("data/AYCET_gameplay_aggregated.csv")
ALL_DCCS_data <- read_csv("data/ALL_DCCS_data.csv")


#Merge by userID so each row represents one participant
AYCET_DCCS <- left_join(AYCET_gameplay_aggregated, ALL_DCCS_data, by = c("userID"))

table(AYCET_DCCS$userID)
#check that each row is a unique participant
count <- AYCET_DCCS %>%
  group_by(userID) %>%
  summarize(n = n()) %>%
  filter(n > 1)


#Define & Add outcomes for DCCS
  #Already calculated - improvement (based on change in DCCS NIH Score), DCCS$ImproverScore
  #New: Above the median, Accuracy of 5 vs below five
  #New: Above the median, RT Score of 2.7 vs below 2.7

#test and train data using 5 fold cross validation
#precision and accuracy graphs
  
#Lasso 1: Which variables are associated with improvement in NIH Score?

#Lasso 2: Which variables are associated with high accuracy at the end?

#Lasso 3: Which variables are associated with low reaction times at the end (fast, correct responses)?
