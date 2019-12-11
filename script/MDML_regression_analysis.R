require(tidyverse)
library(readr)
require(ROCR)
require(ranger)
require(glmnet)
require(dplyr)


#For Lasso regression 
#Read in AYCET gameplay data and DCCS data
AYCET_gameplay_aggregated <- read_csv("data/AYCET_gameplay_aggregated.csv") 
ALL_DCCS_data <- read_csv("data/ALL_DCCS_data.csv")

#Merge by userID so each row represents one participant
AYCET_DCCS <- left_join(AYCET_gameplay_aggregated, ALL_DCCS_data, by = c("userID"))

#check that each row is a unique participant
count <- AYCET_DCCS %>%
  group_by(userID) %>%
  summarize(n = n()) %>%
  filter(n > 1)

#remove variables with many NAs; change variable types
AYCET_DCCS <- AYCET_DCCS %>%
  select(-c(grep("_sess_6",colnames(AYCET_DCCS))))%>%
  select(-c("accessCode", "userID")) %>%
  mutate(highestLevel_user = factor(highestLevel_user, levels = c( "SpaceCakesLevel 0-0-0", "SpaceCakesLevel 0-0-1", "SpaceCakesLevel 0-0-2",  "SpaceCakesLevel 0-0-3",  "SpaceCakesLevel 0-0-4", 
                                                           "SpaceCakesLevel 0-0-5",  "SpaceCakesLevel 0-0-6",  "SpaceCakesLevel 0-0-7",  "SpaceCakesLevel 0-0-8",  "SpaceCakesLevel 0-0-9", 
                                                           "SpaceCakesLevel 0-1-0",  "SpaceCakesLevel 0-1-1",  "SpaceCakesLevel 0-1-2",  "SpaceCakesLevel 0-1-3",  "SpaceCakesLevel 0-1-4", 
                                                           "SpaceCakesLevel 0-1-5",  "SpaceCakesLevel 0-1-6",  "SpaceCakesLevel 0-1-7",  "SpaceCakesLevel 0-2-0",  "SpaceCakesLevel 0-2-1", 
                                                           "SpaceCakesLevel 0-2-2",  "SpaceCakesLevel 0-2-3",  "SpaceCakesLevel 0-2-4",  "SpaceCakesLevel 0-2-5",  "SpaceCakesLevel 0-2-6", 
                                                           "SpaceCakesLevel 0-2-7",  "SpaceCakesLevel 0-2-8",  "SpaceCakesLevel 0-3-0",  "SpaceCakesLevel 0-3-1",  "SpaceCakesLevel 0-3-2", 
                                                           "SpaceCakesLevel 0-3-3",  "SpaceCakesLevel 0-3-4",  "SpaceCakesLevel 0-3-5",  "SpaceCakesLevel 0-3-6",  "SpaceCakesLevel 0-3-7", 
                                                           "SpaceCakesLevel 0-3-8",  "SpaceCakesLevel 0-4-0",  "SpaceCakesLevel 0-4-1", "SpaceCakesLevel 0-4-2",  "SpaceCakesLevel 0-4-3",
                                                           "SpaceCakesLevel 0-4-4",  "SpaceCakesLevel 0-4-5",  "SpaceCakesLevel 0-4-6", "SpaceCakesLevel 0-4-7",  "SpaceCakesLevel 0-4-8",
                                                           "SpaceCakesLevel 0-4-9", "SpaceCakesLevel 0-4-10", "SpaceCakesLevel 0-4-11", "SpaceCakesLevel 1-0-0", "SpaceCakesLevel 1-0-1",
                                                           "SpaceCakesLevel 1-0-2",  "SpaceCakesLevel 1-0-3",  "SpaceCakesLevel 1-0-4",  "SpaceCakesLevel 1-0-5", "SpaceCakesLevel 1-0-6",
                                                           "SpaceCakesLevel 1-0-7",  "SpaceCakesLevel 1-0-8",  "SpaceCakesLevel 1-0-9",  "SpaceCakesLevel 1-0-10", "SpaceCakesLevel 1-1-0",
                                                           "SpaceCakesLevel 1-1-1",  "SpaceCakesLevel 1-1-2",  "SpaceCakesLevel 1-1-3",  "SpaceCakesLevel 1-1-4",  "SpaceCakesLevel 1-2-0", 
                                                           "SpaceCakesLevel 1-2-1",  "SpaceCakesLevel 1-2-2",  "SpaceCakesLevel 1-2-3",  "SpaceCakesLevel 1-2-4",  "SpaceCakesLevel 1-2-5", 
                                                           "SpaceCakesLevel 1-2-6",  "SpaceCakesLevel 1-2-7", "SpaceCakesLevel 1-2-8",  "SpaceCakesLevel 1-3-0",  "SpaceCakesLevel 1-3-1", 
                                                           "SpaceCakesLevel 1-3-2",  "SpaceCakesLevel 1-3-3",  "SpaceCakesLevel 1-3-4",  "SpaceCakesLevel 1-3-5",  "SpaceCakesLevel 1-3-6", 
                                                           "SpaceCakesLevel 1-3-7",  "SpaceCakesLevel 1-3-8",  "SpaceCakesLevel 1-3-9",  "SpaceCakesLevel 1-4-0",  "SpaceCakesLevel 1-4-1", 
                                                           "SpaceCakesLevel 1-4-2",  "SpaceCakesLevel 1-4-3",  "SpaceCakesLevel 1-4-4",  "SpaceCakesLevel 1-4-5",  "SpaceCakesLevel 2-0-0", 
                                                           "SpaceCakesLevel 2-0-1",  "SpaceCakesLevel 2-0-2",  "SpaceCakesLevel 2-0-3",  "SpaceCakesLevel 2-0-4",  "SpaceCakesLevel 2-0-5", 
                                                           "SpaceCakesLevel 2-0-6",  "SpaceCakesLevel 2-0-7",  "SpaceCakesLevel 2-0-8",  "SpaceCakesLevel 2-0-9",  "SpaceCakesLevel 2-0-10",  
                                                           "SpaceCakesLevel 2-0-11", "SpaceCakesLevel 2-0-12", "SpaceCakesLevel 2-0-13", "SpaceCakesLevel 2-0-14",  "SpaceCakesLevel 2-1-0", 
                                                           "SpaceCakesLevel 2-1-1",  "SpaceCakesLevel 2-1-2",  "SpaceCakesLevel 2-1-3",  "SpaceCakesLevel 2-1-4",  "SpaceCakesLevel 2-1-5", 
                                                           "SpaceCakesLevel 2-1-6",  "SpaceCakesLevel 2-1-7",  "SpaceCakesLevel 2-1-8",  "SpaceCakesLevel 2-1-9",  "SpaceCakesLevel 2-1-10", 
                                                           "SpaceCakesLevel 2-1-11", "SpaceCakesLevel 2-1-12", "SpaceCakesLevel 2-1-13", "SpaceCakesLevel 2-1-14",  "SpaceCakesLevel 2-2-0", 
                                                           "SpaceCakesLevel 2-2-1",  "SpaceCakesLevel 2-2-2",  "SpaceCakesLevel 2-2-3",  "SpaceCakesLevel 2-2-4",  "SpaceCakesLevel 2-2-5", 
                                                           "SpaceCakesLevel 2-2-6",  "SpaceCakesLevel 2-3-0",  "SpaceCakesLevel 2-3-1",  "SpaceCakesLevel 2-3-2",  "SpaceCakesLevel 2-3-3", 
                                                           "SpaceCakesLevel 2-3-4",  "SpaceCakesLevel 2-4-0",  "SpaceCakesLevel 2-4-1",  "SpaceCakesLevel 2-4-2",  "SpaceCakesLevel 2-4-3", 
                                                           "SpaceCakesLevel 2-4-4",  "SpaceCakesLevel 3-0-0",  "SpaceCakesLevel 3-0-1",  "SpaceCakesLevel 3-0-2",  "SpaceCakesLevel 3-0-3", 
                                                           "SpaceCakesLevel 3-0-4",  "SpaceCakesLevel 3-0-5",  "SpaceCakesLevel 3-0-6",  "SpaceCakesLevel 3-0-7",  "SpaceCakesLevel 3-1-0", 
                                                           "SpaceCakesLevel 3-1-1",  "SpaceCakesLevel 3-1-2",  "SpaceCakesLevel 3-1-3",  "SpaceCakesLevel 3-2-0",  "SpaceCakesLevel 3-2-1", 
                                                           "SpaceCakesLevel 3-2-2",  "SpaceCakesLevel 3-3-0",  "SpaceCakesLevel 3-3-1",  "SpaceCakesLevel 3-3-2",  "SpaceCakesLevel 3-3-3", 
                                                           "SpaceCakesLevel 3-3-4",  "SpaceCakesLevel 3-3-5",  "SpaceCakesLevel 3-4-0",  "SpaceCakesLevel 3-4-1",  "SpaceCakesLevel 3-4-2", 
                                                           "SpaceCakesLevel 4-0-0",  "SpaceCakesLevel 4-0-1",  "SpaceCakesLevel 4-0-2",  "SpaceCakesLevel 4-0-3",  "SpaceCakesLevel 4-0-4", 
                                                           "SpaceCakesLevel 4-0-5",  "SpaceCakesLevel 4-0-6",  "SpaceCakesLevel 4-0-7",  "SpaceCakesLevel 4-1-0",  "SpaceCakesLevel 4-1-1", 
                                                           "SpaceCakesLevel 4-1-2",  "SpaceCakesLevel 4-1-3",  "SpaceCakesLevel 4-1-4",  "SpaceCakesLevel 4-1-5",  "SpaceCakesLevel 4-2-0", 
                                                           "SpaceCakesLevel 4-2-1",  "SpaceCakesLevel 4-2-2",  "SpaceCakesLevel 4-3-0",  "SpaceCakesLevel 4-3-1",  "SpaceCakesLevel 4-4-0", 
                                                           "SpaceCakesLevel 4-4-1",  "SpaceCakesLevel 4-4-2",  "SpaceCakesLevel 4-4-3",  "SpaceCakesLevel 5-0-0",  "SpaceCakesLevel 5-0-1", 
                                                           "SpaceCakesLevel 5-1-0",  "SpaceCakesLevel 5-2-0",  "SpaceCakesLevel 5-3-0",  "SpaceCakesLevel 5-4-0",  "SpaceCakesLevel 6-0-0", 
                                                           "SpaceCakesLevel 6-1-0",  "SpaceCakesLevel 6-2-0"),
                                    ordered = TRUE), #need to give the order because numbers read as characters are wrong,
         highestLevel_diff_Easy = factor(highestLevel_diff_Easy, levels = c( "SpaceCakesLevel 0-0-0", "SpaceCakesLevel 0-0-1", "SpaceCakesLevel 0-0-2",  "SpaceCakesLevel 0-0-3",  "SpaceCakesLevel 0-0-4", 
                                                                  "SpaceCakesLevel 0-0-5",  "SpaceCakesLevel 0-0-6",  "SpaceCakesLevel 0-0-7",  "SpaceCakesLevel 0-0-8",  "SpaceCakesLevel 0-0-9", 
                                                                  "SpaceCakesLevel 0-1-0",  "SpaceCakesLevel 0-1-1",  "SpaceCakesLevel 0-1-2",  "SpaceCakesLevel 0-1-3",  "SpaceCakesLevel 0-1-4", 
                                                                  "SpaceCakesLevel 0-1-5",  "SpaceCakesLevel 0-1-6",  "SpaceCakesLevel 0-1-7",  "SpaceCakesLevel 0-2-0",  "SpaceCakesLevel 0-2-1", 
                                                                  "SpaceCakesLevel 0-2-2",  "SpaceCakesLevel 0-2-3",  "SpaceCakesLevel 0-2-4",  "SpaceCakesLevel 0-2-5",  "SpaceCakesLevel 0-2-6", 
                                                                  "SpaceCakesLevel 0-2-7",  "SpaceCakesLevel 0-2-8",  "SpaceCakesLevel 0-3-0",  "SpaceCakesLevel 0-3-1",  "SpaceCakesLevel 0-3-2", 
                                                                  "SpaceCakesLevel 0-3-3",  "SpaceCakesLevel 0-3-4",  "SpaceCakesLevel 0-3-5",  "SpaceCakesLevel 0-3-6",  "SpaceCakesLevel 0-3-7", 
                                                                  "SpaceCakesLevel 0-3-8",  "SpaceCakesLevel 0-4-0",  "SpaceCakesLevel 0-4-1", "SpaceCakesLevel 0-4-2",  "SpaceCakesLevel 0-4-3",
                                                                  "SpaceCakesLevel 0-4-4",  "SpaceCakesLevel 0-4-5",  "SpaceCakesLevel 0-4-6", "SpaceCakesLevel 0-4-7",  "SpaceCakesLevel 0-4-8",
                                                                  "SpaceCakesLevel 0-4-9", "SpaceCakesLevel 0-4-10", "SpaceCakesLevel 0-4-11", "SpaceCakesLevel 1-0-0", "SpaceCakesLevel 1-0-1",
                                                                  "SpaceCakesLevel 1-0-2",  "SpaceCakesLevel 1-0-3",  "SpaceCakesLevel 1-0-4",  "SpaceCakesLevel 1-0-5", "SpaceCakesLevel 1-0-6",
                                                                  "SpaceCakesLevel 1-0-7",  "SpaceCakesLevel 1-0-8",  "SpaceCakesLevel 1-0-9",  "SpaceCakesLevel 1-0-10", "SpaceCakesLevel 1-1-0",
                                                                  "SpaceCakesLevel 1-1-1",  "SpaceCakesLevel 1-1-2",  "SpaceCakesLevel 1-1-3",  "SpaceCakesLevel 1-1-4",  "SpaceCakesLevel 1-2-0", 
                                                                  "SpaceCakesLevel 1-2-1",  "SpaceCakesLevel 1-2-2",  "SpaceCakesLevel 1-2-3",  "SpaceCakesLevel 1-2-4",  "SpaceCakesLevel 1-2-5", 
                                                                  "SpaceCakesLevel 1-2-6",  "SpaceCakesLevel 1-2-7", "SpaceCakesLevel 1-2-8",  "SpaceCakesLevel 1-3-0",  "SpaceCakesLevel 1-3-1", 
                                                                  "SpaceCakesLevel 1-3-2",  "SpaceCakesLevel 1-3-3",  "SpaceCakesLevel 1-3-4",  "SpaceCakesLevel 1-3-5",  "SpaceCakesLevel 1-3-6", 
                                                                  "SpaceCakesLevel 1-3-7",  "SpaceCakesLevel 1-3-8",  "SpaceCakesLevel 1-3-9",  "SpaceCakesLevel 1-4-0",  "SpaceCakesLevel 1-4-1", 
                                                                  "SpaceCakesLevel 1-4-2",  "SpaceCakesLevel 1-4-3",  "SpaceCakesLevel 1-4-4",  "SpaceCakesLevel 1-4-5",  "SpaceCakesLevel 2-0-0", 
                                                                  "SpaceCakesLevel 2-0-1",  "SpaceCakesLevel 2-0-2",  "SpaceCakesLevel 2-0-3",  "SpaceCakesLevel 2-0-4",  "SpaceCakesLevel 2-0-5", 
                                                                  "SpaceCakesLevel 2-0-6",  "SpaceCakesLevel 2-0-7",  "SpaceCakesLevel 2-0-8",  "SpaceCakesLevel 2-0-9",  "SpaceCakesLevel 2-0-10",  
                                                                  "SpaceCakesLevel 2-0-11", "SpaceCakesLevel 2-0-12", "SpaceCakesLevel 2-0-13", "SpaceCakesLevel 2-0-14",  "SpaceCakesLevel 2-1-0", 
                                                                  "SpaceCakesLevel 2-1-1",  "SpaceCakesLevel 2-1-2",  "SpaceCakesLevel 2-1-3",  "SpaceCakesLevel 2-1-4",  "SpaceCakesLevel 2-1-5", 
                                                                  "SpaceCakesLevel 2-1-6",  "SpaceCakesLevel 2-1-7",  "SpaceCakesLevel 2-1-8",  "SpaceCakesLevel 2-1-9",  "SpaceCakesLevel 2-1-10", 
                                                                  "SpaceCakesLevel 2-1-11", "SpaceCakesLevel 2-1-12", "SpaceCakesLevel 2-1-13", "SpaceCakesLevel 2-1-14",  "SpaceCakesLevel 2-2-0", 
                                                                  "SpaceCakesLevel 2-2-1",  "SpaceCakesLevel 2-2-2",  "SpaceCakesLevel 2-2-3",  "SpaceCakesLevel 2-2-4",  "SpaceCakesLevel 2-2-5", 
                                                                  "SpaceCakesLevel 2-2-6",  "SpaceCakesLevel 2-3-0",  "SpaceCakesLevel 2-3-1",  "SpaceCakesLevel 2-3-2",  "SpaceCakesLevel 2-3-3", 
                                                                  "SpaceCakesLevel 2-3-4",  "SpaceCakesLevel 2-4-0",  "SpaceCakesLevel 2-4-1",  "SpaceCakesLevel 2-4-2",  "SpaceCakesLevel 2-4-3", 
                                                                  "SpaceCakesLevel 2-4-4",  "SpaceCakesLevel 3-0-0",  "SpaceCakesLevel 3-0-1",  "SpaceCakesLevel 3-0-2",  "SpaceCakesLevel 3-0-3", 
                                                                  "SpaceCakesLevel 3-0-4",  "SpaceCakesLevel 3-0-5",  "SpaceCakesLevel 3-0-6",  "SpaceCakesLevel 3-0-7",  "SpaceCakesLevel 3-1-0", 
                                                                  "SpaceCakesLevel 3-1-1",  "SpaceCakesLevel 3-1-2",  "SpaceCakesLevel 3-1-3",  "SpaceCakesLevel 3-2-0",  "SpaceCakesLevel 3-2-1", 
                                                                  "SpaceCakesLevel 3-2-2",  "SpaceCakesLevel 3-3-0",  "SpaceCakesLevel 3-3-1",  "SpaceCakesLevel 3-3-2",  "SpaceCakesLevel 3-3-3", 
                                                                  "SpaceCakesLevel 3-3-4",  "SpaceCakesLevel 3-3-5",  "SpaceCakesLevel 3-4-0",  "SpaceCakesLevel 3-4-1",  "SpaceCakesLevel 3-4-2", 
                                                                  "SpaceCakesLevel 4-0-0",  "SpaceCakesLevel 4-0-1",  "SpaceCakesLevel 4-0-2",  "SpaceCakesLevel 4-0-3",  "SpaceCakesLevel 4-0-4", 
                                                                  "SpaceCakesLevel 4-0-5",  "SpaceCakesLevel 4-0-6",  "SpaceCakesLevel 4-0-7",  "SpaceCakesLevel 4-1-0",  "SpaceCakesLevel 4-1-1", 
                                                                  "SpaceCakesLevel 4-1-2",  "SpaceCakesLevel 4-1-3",  "SpaceCakesLevel 4-1-4",  "SpaceCakesLevel 4-1-5",  "SpaceCakesLevel 4-2-0", 
                                                                  "SpaceCakesLevel 4-2-1",  "SpaceCakesLevel 4-2-2",  "SpaceCakesLevel 4-3-0",  "SpaceCakesLevel 4-3-1",  "SpaceCakesLevel 4-4-0", 
                                                                  "SpaceCakesLevel 4-4-1",  "SpaceCakesLevel 4-4-2",  "SpaceCakesLevel 4-4-3",  "SpaceCakesLevel 5-0-0",  "SpaceCakesLevel 5-0-1", 
                                                                  "SpaceCakesLevel 5-1-0",  "SpaceCakesLevel 5-2-0",  "SpaceCakesLevel 5-3-0",  "SpaceCakesLevel 5-4-0",  "SpaceCakesLevel 6-0-0", 
                                                                  "SpaceCakesLevel 6-1-0",  "SpaceCakesLevel 6-2-0"),
                                           ordered = TRUE), #need to give the order because numbers read as characters are wrong,
         highestLevel_diff_Medium = factor(highestLevel_diff_Medium, levels = c( "SpaceCakesLevel 0-0-0", "SpaceCakesLevel 0-0-1", "SpaceCakesLevel 0-0-2",  "SpaceCakesLevel 0-0-3",  "SpaceCakesLevel 0-0-4", 
                                                                  "SpaceCakesLevel 0-0-5",  "SpaceCakesLevel 0-0-6",  "SpaceCakesLevel 0-0-7",  "SpaceCakesLevel 0-0-8",  "SpaceCakesLevel 0-0-9", 
                                                                  "SpaceCakesLevel 0-1-0",  "SpaceCakesLevel 0-1-1",  "SpaceCakesLevel 0-1-2",  "SpaceCakesLevel 0-1-3",  "SpaceCakesLevel 0-1-4", 
                                                                  "SpaceCakesLevel 0-1-5",  "SpaceCakesLevel 0-1-6",  "SpaceCakesLevel 0-1-7",  "SpaceCakesLevel 0-2-0",  "SpaceCakesLevel 0-2-1", 
                                                                  "SpaceCakesLevel 0-2-2",  "SpaceCakesLevel 0-2-3",  "SpaceCakesLevel 0-2-4",  "SpaceCakesLevel 0-2-5",  "SpaceCakesLevel 0-2-6", 
                                                                  "SpaceCakesLevel 0-2-7",  "SpaceCakesLevel 0-2-8",  "SpaceCakesLevel 0-3-0",  "SpaceCakesLevel 0-3-1",  "SpaceCakesLevel 0-3-2", 
                                                                  "SpaceCakesLevel 0-3-3",  "SpaceCakesLevel 0-3-4",  "SpaceCakesLevel 0-3-5",  "SpaceCakesLevel 0-3-6",  "SpaceCakesLevel 0-3-7", 
                                                                  "SpaceCakesLevel 0-3-8",  "SpaceCakesLevel 0-4-0",  "SpaceCakesLevel 0-4-1", "SpaceCakesLevel 0-4-2",  "SpaceCakesLevel 0-4-3",
                                                                  "SpaceCakesLevel 0-4-4",  "SpaceCakesLevel 0-4-5",  "SpaceCakesLevel 0-4-6", "SpaceCakesLevel 0-4-7",  "SpaceCakesLevel 0-4-8",
                                                                  "SpaceCakesLevel 0-4-9", "SpaceCakesLevel 0-4-10", "SpaceCakesLevel 0-4-11", "SpaceCakesLevel 1-0-0", "SpaceCakesLevel 1-0-1",
                                                                  "SpaceCakesLevel 1-0-2",  "SpaceCakesLevel 1-0-3",  "SpaceCakesLevel 1-0-4",  "SpaceCakesLevel 1-0-5", "SpaceCakesLevel 1-0-6",
                                                                  "SpaceCakesLevel 1-0-7",  "SpaceCakesLevel 1-0-8",  "SpaceCakesLevel 1-0-9",  "SpaceCakesLevel 1-0-10", "SpaceCakesLevel 1-1-0",
                                                                  "SpaceCakesLevel 1-1-1",  "SpaceCakesLevel 1-1-2",  "SpaceCakesLevel 1-1-3",  "SpaceCakesLevel 1-1-4",  "SpaceCakesLevel 1-2-0", 
                                                                  "SpaceCakesLevel 1-2-1",  "SpaceCakesLevel 1-2-2",  "SpaceCakesLevel 1-2-3",  "SpaceCakesLevel 1-2-4",  "SpaceCakesLevel 1-2-5", 
                                                                  "SpaceCakesLevel 1-2-6",  "SpaceCakesLevel 1-2-7", "SpaceCakesLevel 1-2-8",  "SpaceCakesLevel 1-3-0",  "SpaceCakesLevel 1-3-1", 
                                                                  "SpaceCakesLevel 1-3-2",  "SpaceCakesLevel 1-3-3",  "SpaceCakesLevel 1-3-4",  "SpaceCakesLevel 1-3-5",  "SpaceCakesLevel 1-3-6", 
                                                                  "SpaceCakesLevel 1-3-7",  "SpaceCakesLevel 1-3-8",  "SpaceCakesLevel 1-3-9",  "SpaceCakesLevel 1-4-0",  "SpaceCakesLevel 1-4-1", 
                                                                  "SpaceCakesLevel 1-4-2",  "SpaceCakesLevel 1-4-3",  "SpaceCakesLevel 1-4-4",  "SpaceCakesLevel 1-4-5",  "SpaceCakesLevel 2-0-0", 
                                                                  "SpaceCakesLevel 2-0-1",  "SpaceCakesLevel 2-0-2",  "SpaceCakesLevel 2-0-3",  "SpaceCakesLevel 2-0-4",  "SpaceCakesLevel 2-0-5", 
                                                                  "SpaceCakesLevel 2-0-6",  "SpaceCakesLevel 2-0-7",  "SpaceCakesLevel 2-0-8",  "SpaceCakesLevel 2-0-9",  "SpaceCakesLevel 2-0-10",  
                                                                  "SpaceCakesLevel 2-0-11", "SpaceCakesLevel 2-0-12", "SpaceCakesLevel 2-0-13", "SpaceCakesLevel 2-0-14",  "SpaceCakesLevel 2-1-0", 
                                                                  "SpaceCakesLevel 2-1-1",  "SpaceCakesLevel 2-1-2",  "SpaceCakesLevel 2-1-3",  "SpaceCakesLevel 2-1-4",  "SpaceCakesLevel 2-1-5", 
                                                                  "SpaceCakesLevel 2-1-6",  "SpaceCakesLevel 2-1-7",  "SpaceCakesLevel 2-1-8",  "SpaceCakesLevel 2-1-9",  "SpaceCakesLevel 2-1-10", 
                                                                  "SpaceCakesLevel 2-1-11", "SpaceCakesLevel 2-1-12", "SpaceCakesLevel 2-1-13", "SpaceCakesLevel 2-1-14",  "SpaceCakesLevel 2-2-0", 
                                                                  "SpaceCakesLevel 2-2-1",  "SpaceCakesLevel 2-2-2",  "SpaceCakesLevel 2-2-3",  "SpaceCakesLevel 2-2-4",  "SpaceCakesLevel 2-2-5", 
                                                                  "SpaceCakesLevel 2-2-6",  "SpaceCakesLevel 2-3-0",  "SpaceCakesLevel 2-3-1",  "SpaceCakesLevel 2-3-2",  "SpaceCakesLevel 2-3-3", 
                                                                  "SpaceCakesLevel 2-3-4",  "SpaceCakesLevel 2-4-0",  "SpaceCakesLevel 2-4-1",  "SpaceCakesLevel 2-4-2",  "SpaceCakesLevel 2-4-3", 
                                                                  "SpaceCakesLevel 2-4-4",  "SpaceCakesLevel 3-0-0",  "SpaceCakesLevel 3-0-1",  "SpaceCakesLevel 3-0-2",  "SpaceCakesLevel 3-0-3", 
                                                                  "SpaceCakesLevel 3-0-4",  "SpaceCakesLevel 3-0-5",  "SpaceCakesLevel 3-0-6",  "SpaceCakesLevel 3-0-7",  "SpaceCakesLevel 3-1-0", 
                                                                  "SpaceCakesLevel 3-1-1",  "SpaceCakesLevel 3-1-2",  "SpaceCakesLevel 3-1-3",  "SpaceCakesLevel 3-2-0",  "SpaceCakesLevel 3-2-1", 
                                                                  "SpaceCakesLevel 3-2-2",  "SpaceCakesLevel 3-3-0",  "SpaceCakesLevel 3-3-1",  "SpaceCakesLevel 3-3-2",  "SpaceCakesLevel 3-3-3", 
                                                                  "SpaceCakesLevel 3-3-4",  "SpaceCakesLevel 3-3-5",  "SpaceCakesLevel 3-4-0",  "SpaceCakesLevel 3-4-1",  "SpaceCakesLevel 3-4-2", 
                                                                  "SpaceCakesLevel 4-0-0",  "SpaceCakesLevel 4-0-1",  "SpaceCakesLevel 4-0-2",  "SpaceCakesLevel 4-0-3",  "SpaceCakesLevel 4-0-4", 
                                                                  "SpaceCakesLevel 4-0-5",  "SpaceCakesLevel 4-0-6",  "SpaceCakesLevel 4-0-7",  "SpaceCakesLevel 4-1-0",  "SpaceCakesLevel 4-1-1", 
                                                                  "SpaceCakesLevel 4-1-2",  "SpaceCakesLevel 4-1-3",  "SpaceCakesLevel 4-1-4",  "SpaceCakesLevel 4-1-5",  "SpaceCakesLevel 4-2-0", 
                                                                  "SpaceCakesLevel 4-2-1",  "SpaceCakesLevel 4-2-2",  "SpaceCakesLevel 4-3-0",  "SpaceCakesLevel 4-3-1",  "SpaceCakesLevel 4-4-0", 
                                                                  "SpaceCakesLevel 4-4-1",  "SpaceCakesLevel 4-4-2",  "SpaceCakesLevel 4-4-3",  "SpaceCakesLevel 5-0-0",  "SpaceCakesLevel 5-0-1", 
                                                                  "SpaceCakesLevel 5-1-0",  "SpaceCakesLevel 5-2-0",  "SpaceCakesLevel 5-3-0",  "SpaceCakesLevel 5-4-0",  "SpaceCakesLevel 6-0-0", 
                                                                  "SpaceCakesLevel 6-1-0",  "SpaceCakesLevel 6-2-0"),
                                           ordered = TRUE), #need to give the order because numbers read as characters are wrong, 
         highestLevel_diff_Difficult = factor(highestLevel_diff_Difficult, levels = c( "SpaceCakesLevel 0-0-0", "SpaceCakesLevel 0-0-1", "SpaceCakesLevel 0-0-2",  "SpaceCakesLevel 0-0-3",  "SpaceCakesLevel 0-0-4", 
                                                                     "SpaceCakesLevel 0-0-5",  "SpaceCakesLevel 0-0-6",  "SpaceCakesLevel 0-0-7",  "SpaceCakesLevel 0-0-8",  "SpaceCakesLevel 0-0-9", 
                                                                     "SpaceCakesLevel 0-1-0",  "SpaceCakesLevel 0-1-1",  "SpaceCakesLevel 0-1-2",  "SpaceCakesLevel 0-1-3",  "SpaceCakesLevel 0-1-4", 
                                                                     "SpaceCakesLevel 0-1-5",  "SpaceCakesLevel 0-1-6",  "SpaceCakesLevel 0-1-7",  "SpaceCakesLevel 0-2-0",  "SpaceCakesLevel 0-2-1", 
                                                                     "SpaceCakesLevel 0-2-2",  "SpaceCakesLevel 0-2-3",  "SpaceCakesLevel 0-2-4",  "SpaceCakesLevel 0-2-5",  "SpaceCakesLevel 0-2-6", 
                                                                     "SpaceCakesLevel 0-2-7",  "SpaceCakesLevel 0-2-8",  "SpaceCakesLevel 0-3-0",  "SpaceCakesLevel 0-3-1",  "SpaceCakesLevel 0-3-2", 
                                                                     "SpaceCakesLevel 0-3-3",  "SpaceCakesLevel 0-3-4",  "SpaceCakesLevel 0-3-5",  "SpaceCakesLevel 0-3-6",  "SpaceCakesLevel 0-3-7", 
                                                                     "SpaceCakesLevel 0-3-8",  "SpaceCakesLevel 0-4-0",  "SpaceCakesLevel 0-4-1", "SpaceCakesLevel 0-4-2",  "SpaceCakesLevel 0-4-3",
                                                                     "SpaceCakesLevel 0-4-4",  "SpaceCakesLevel 0-4-5",  "SpaceCakesLevel 0-4-6", "SpaceCakesLevel 0-4-7",  "SpaceCakesLevel 0-4-8",
                                                                     "SpaceCakesLevel 0-4-9", "SpaceCakesLevel 0-4-10", "SpaceCakesLevel 0-4-11", "SpaceCakesLevel 1-0-0", "SpaceCakesLevel 1-0-1",
                                                                     "SpaceCakesLevel 1-0-2",  "SpaceCakesLevel 1-0-3",  "SpaceCakesLevel 1-0-4",  "SpaceCakesLevel 1-0-5", "SpaceCakesLevel 1-0-6",
                                                                     "SpaceCakesLevel 1-0-7",  "SpaceCakesLevel 1-0-8",  "SpaceCakesLevel 1-0-9",  "SpaceCakesLevel 1-0-10", "SpaceCakesLevel 1-1-0",
                                                                     "SpaceCakesLevel 1-1-1",  "SpaceCakesLevel 1-1-2",  "SpaceCakesLevel 1-1-3",  "SpaceCakesLevel 1-1-4",  "SpaceCakesLevel 1-2-0", 
                                                                     "SpaceCakesLevel 1-2-1",  "SpaceCakesLevel 1-2-2",  "SpaceCakesLevel 1-2-3",  "SpaceCakesLevel 1-2-4",  "SpaceCakesLevel 1-2-5", 
                                                                     "SpaceCakesLevel 1-2-6",  "SpaceCakesLevel 1-2-7", "SpaceCakesLevel 1-2-8",  "SpaceCakesLevel 1-3-0",  "SpaceCakesLevel 1-3-1", 
                                                                     "SpaceCakesLevel 1-3-2",  "SpaceCakesLevel 1-3-3",  "SpaceCakesLevel 1-3-4",  "SpaceCakesLevel 1-3-5",  "SpaceCakesLevel 1-3-6", 
                                                                     "SpaceCakesLevel 1-3-7",  "SpaceCakesLevel 1-3-8",  "SpaceCakesLevel 1-3-9",  "SpaceCakesLevel 1-4-0",  "SpaceCakesLevel 1-4-1", 
                                                                     "SpaceCakesLevel 1-4-2",  "SpaceCakesLevel 1-4-3",  "SpaceCakesLevel 1-4-4",  "SpaceCakesLevel 1-4-5",  "SpaceCakesLevel 2-0-0", 
                                                                     "SpaceCakesLevel 2-0-1",  "SpaceCakesLevel 2-0-2",  "SpaceCakesLevel 2-0-3",  "SpaceCakesLevel 2-0-4",  "SpaceCakesLevel 2-0-5", 
                                                                     "SpaceCakesLevel 2-0-6",  "SpaceCakesLevel 2-0-7",  "SpaceCakesLevel 2-0-8",  "SpaceCakesLevel 2-0-9",  "SpaceCakesLevel 2-0-10",  
                                                                     "SpaceCakesLevel 2-0-11", "SpaceCakesLevel 2-0-12", "SpaceCakesLevel 2-0-13", "SpaceCakesLevel 2-0-14",  "SpaceCakesLevel 2-1-0", 
                                                                     "SpaceCakesLevel 2-1-1",  "SpaceCakesLevel 2-1-2",  "SpaceCakesLevel 2-1-3",  "SpaceCakesLevel 2-1-4",  "SpaceCakesLevel 2-1-5", 
                                                                     "SpaceCakesLevel 2-1-6",  "SpaceCakesLevel 2-1-7",  "SpaceCakesLevel 2-1-8",  "SpaceCakesLevel 2-1-9",  "SpaceCakesLevel 2-1-10", 
                                                                     "SpaceCakesLevel 2-1-11", "SpaceCakesLevel 2-1-12", "SpaceCakesLevel 2-1-13", "SpaceCakesLevel 2-1-14",  "SpaceCakesLevel 2-2-0", 
                                                                     "SpaceCakesLevel 2-2-1",  "SpaceCakesLevel 2-2-2",  "SpaceCakesLevel 2-2-3",  "SpaceCakesLevel 2-2-4",  "SpaceCakesLevel 2-2-5", 
                                                                     "SpaceCakesLevel 2-2-6",  "SpaceCakesLevel 2-3-0",  "SpaceCakesLevel 2-3-1",  "SpaceCakesLevel 2-3-2",  "SpaceCakesLevel 2-3-3", 
                                                                     "SpaceCakesLevel 2-3-4",  "SpaceCakesLevel 2-4-0",  "SpaceCakesLevel 2-4-1",  "SpaceCakesLevel 2-4-2",  "SpaceCakesLevel 2-4-3", 
                                                                     "SpaceCakesLevel 2-4-4",  "SpaceCakesLevel 3-0-0",  "SpaceCakesLevel 3-0-1",  "SpaceCakesLevel 3-0-2",  "SpaceCakesLevel 3-0-3", 
                                                                     "SpaceCakesLevel 3-0-4",  "SpaceCakesLevel 3-0-5",  "SpaceCakesLevel 3-0-6",  "SpaceCakesLevel 3-0-7",  "SpaceCakesLevel 3-1-0", 
                                                                     "SpaceCakesLevel 3-1-1",  "SpaceCakesLevel 3-1-2",  "SpaceCakesLevel 3-1-3",  "SpaceCakesLevel 3-2-0",  "SpaceCakesLevel 3-2-1", 
                                                                     "SpaceCakesLevel 3-2-2",  "SpaceCakesLevel 3-3-0",  "SpaceCakesLevel 3-3-1",  "SpaceCakesLevel 3-3-2",  "SpaceCakesLevel 3-3-3", 
                                                                     "SpaceCakesLevel 3-3-4",  "SpaceCakesLevel 3-3-5",  "SpaceCakesLevel 3-4-0",  "SpaceCakesLevel 3-4-1",  "SpaceCakesLevel 3-4-2", 
                                                                     "SpaceCakesLevel 4-0-0",  "SpaceCakesLevel 4-0-1",  "SpaceCakesLevel 4-0-2",  "SpaceCakesLevel 4-0-3",  "SpaceCakesLevel 4-0-4", 
                                                                     "SpaceCakesLevel 4-0-5",  "SpaceCakesLevel 4-0-6",  "SpaceCakesLevel 4-0-7",  "SpaceCakesLevel 4-1-0",  "SpaceCakesLevel 4-1-1", 
                                                                     "SpaceCakesLevel 4-1-2",  "SpaceCakesLevel 4-1-3",  "SpaceCakesLevel 4-1-4",  "SpaceCakesLevel 4-1-5",  "SpaceCakesLevel 4-2-0", 
                                                                     "SpaceCakesLevel 4-2-1",  "SpaceCakesLevel 4-2-2",  "SpaceCakesLevel 4-3-0",  "SpaceCakesLevel 4-3-1",  "SpaceCakesLevel 4-4-0", 
                                                                     "SpaceCakesLevel 4-4-1",  "SpaceCakesLevel 4-4-2",  "SpaceCakesLevel 4-4-3",  "SpaceCakesLevel 5-0-0",  "SpaceCakesLevel 5-0-1", 
                                                                     "SpaceCakesLevel 5-1-0",  "SpaceCakesLevel 5-2-0",  "SpaceCakesLevel 5-3-0",  "SpaceCakesLevel 5-4-0",  "SpaceCakesLevel 6-0-0", 
                                                                     "SpaceCakesLevel 6-1-0",  "SpaceCakesLevel 6-2-0"),
                                              ordered = TRUE), #need to give the order because numbers read as characters are wrong,
         highestLevel_sess_1 = factor(highestLevel_sess_1, levels = c( "SpaceCakesLevel 0-0-0", "SpaceCakesLevel 0-0-1", "SpaceCakesLevel 0-0-2",  "SpaceCakesLevel 0-0-3",  "SpaceCakesLevel 0-0-4", 
                                                             "SpaceCakesLevel 0-0-5",  "SpaceCakesLevel 0-0-6",  "SpaceCakesLevel 0-0-7",  "SpaceCakesLevel 0-0-8",  "SpaceCakesLevel 0-0-9", 
                                                             "SpaceCakesLevel 0-1-0",  "SpaceCakesLevel 0-1-1",  "SpaceCakesLevel 0-1-2",  "SpaceCakesLevel 0-1-3",  "SpaceCakesLevel 0-1-4", 
                                                             "SpaceCakesLevel 0-1-5",  "SpaceCakesLevel 0-1-6",  "SpaceCakesLevel 0-1-7",  "SpaceCakesLevel 0-2-0",  "SpaceCakesLevel 0-2-1", 
                                                             "SpaceCakesLevel 0-2-2",  "SpaceCakesLevel 0-2-3",  "SpaceCakesLevel 0-2-4",  "SpaceCakesLevel 0-2-5",  "SpaceCakesLevel 0-2-6", 
                                                             "SpaceCakesLevel 0-2-7",  "SpaceCakesLevel 0-2-8",  "SpaceCakesLevel 0-3-0",  "SpaceCakesLevel 0-3-1",  "SpaceCakesLevel 0-3-2", 
                                                             "SpaceCakesLevel 0-3-3",  "SpaceCakesLevel 0-3-4",  "SpaceCakesLevel 0-3-5",  "SpaceCakesLevel 0-3-6",  "SpaceCakesLevel 0-3-7", 
                                                             "SpaceCakesLevel 0-3-8",  "SpaceCakesLevel 0-4-0",  "SpaceCakesLevel 0-4-1", "SpaceCakesLevel 0-4-2",  "SpaceCakesLevel 0-4-3",
                                                             "SpaceCakesLevel 0-4-4",  "SpaceCakesLevel 0-4-5",  "SpaceCakesLevel 0-4-6", "SpaceCakesLevel 0-4-7",  "SpaceCakesLevel 0-4-8",
                                                             "SpaceCakesLevel 0-4-9", "SpaceCakesLevel 0-4-10", "SpaceCakesLevel 0-4-11", "SpaceCakesLevel 1-0-0", "SpaceCakesLevel 1-0-1",
                                                             "SpaceCakesLevel 1-0-2",  "SpaceCakesLevel 1-0-3",  "SpaceCakesLevel 1-0-4",  "SpaceCakesLevel 1-0-5", "SpaceCakesLevel 1-0-6",
                                                             "SpaceCakesLevel 1-0-7",  "SpaceCakesLevel 1-0-8",  "SpaceCakesLevel 1-0-9",  "SpaceCakesLevel 1-0-10", "SpaceCakesLevel 1-1-0",
                                                             "SpaceCakesLevel 1-1-1",  "SpaceCakesLevel 1-1-2",  "SpaceCakesLevel 1-1-3",  "SpaceCakesLevel 1-1-4",  "SpaceCakesLevel 1-2-0", 
                                                             "SpaceCakesLevel 1-2-1",  "SpaceCakesLevel 1-2-2",  "SpaceCakesLevel 1-2-3",  "SpaceCakesLevel 1-2-4",  "SpaceCakesLevel 1-2-5", 
                                                             "SpaceCakesLevel 1-2-6",  "SpaceCakesLevel 1-2-7", "SpaceCakesLevel 1-2-8",  "SpaceCakesLevel 1-3-0",  "SpaceCakesLevel 1-3-1", 
                                                             "SpaceCakesLevel 1-3-2",  "SpaceCakesLevel 1-3-3",  "SpaceCakesLevel 1-3-4",  "SpaceCakesLevel 1-3-5",  "SpaceCakesLevel 1-3-6", 
                                                             "SpaceCakesLevel 1-3-7",  "SpaceCakesLevel 1-3-8",  "SpaceCakesLevel 1-3-9",  "SpaceCakesLevel 1-4-0",  "SpaceCakesLevel 1-4-1", 
                                                             "SpaceCakesLevel 1-4-2",  "SpaceCakesLevel 1-4-3",  "SpaceCakesLevel 1-4-4",  "SpaceCakesLevel 1-4-5",  "SpaceCakesLevel 2-0-0", 
                                                             "SpaceCakesLevel 2-0-1",  "SpaceCakesLevel 2-0-2",  "SpaceCakesLevel 2-0-3",  "SpaceCakesLevel 2-0-4",  "SpaceCakesLevel 2-0-5", 
                                                             "SpaceCakesLevel 2-0-6",  "SpaceCakesLevel 2-0-7",  "SpaceCakesLevel 2-0-8",  "SpaceCakesLevel 2-0-9",  "SpaceCakesLevel 2-0-10",  
                                                             "SpaceCakesLevel 2-0-11", "SpaceCakesLevel 2-0-12", "SpaceCakesLevel 2-0-13", "SpaceCakesLevel 2-0-14",  "SpaceCakesLevel 2-1-0", 
                                                             "SpaceCakesLevel 2-1-1",  "SpaceCakesLevel 2-1-2",  "SpaceCakesLevel 2-1-3",  "SpaceCakesLevel 2-1-4",  "SpaceCakesLevel 2-1-5", 
                                                             "SpaceCakesLevel 2-1-6",  "SpaceCakesLevel 2-1-7",  "SpaceCakesLevel 2-1-8",  "SpaceCakesLevel 2-1-9",  "SpaceCakesLevel 2-1-10", 
                                                             "SpaceCakesLevel 2-1-11", "SpaceCakesLevel 2-1-12", "SpaceCakesLevel 2-1-13", "SpaceCakesLevel 2-1-14",  "SpaceCakesLevel 2-2-0", 
                                                             "SpaceCakesLevel 2-2-1",  "SpaceCakesLevel 2-2-2",  "SpaceCakesLevel 2-2-3",  "SpaceCakesLevel 2-2-4",  "SpaceCakesLevel 2-2-5", 
                                                             "SpaceCakesLevel 2-2-6",  "SpaceCakesLevel 2-3-0",  "SpaceCakesLevel 2-3-1",  "SpaceCakesLevel 2-3-2",  "SpaceCakesLevel 2-3-3", 
                                                             "SpaceCakesLevel 2-3-4",  "SpaceCakesLevel 2-4-0",  "SpaceCakesLevel 2-4-1",  "SpaceCakesLevel 2-4-2",  "SpaceCakesLevel 2-4-3", 
                                                             "SpaceCakesLevel 2-4-4",  "SpaceCakesLevel 3-0-0",  "SpaceCakesLevel 3-0-1",  "SpaceCakesLevel 3-0-2",  "SpaceCakesLevel 3-0-3", 
                                                             "SpaceCakesLevel 3-0-4",  "SpaceCakesLevel 3-0-5",  "SpaceCakesLevel 3-0-6",  "SpaceCakesLevel 3-0-7",  "SpaceCakesLevel 3-1-0", 
                                                             "SpaceCakesLevel 3-1-1",  "SpaceCakesLevel 3-1-2",  "SpaceCakesLevel 3-1-3",  "SpaceCakesLevel 3-2-0",  "SpaceCakesLevel 3-2-1", 
                                                             "SpaceCakesLevel 3-2-2",  "SpaceCakesLevel 3-3-0",  "SpaceCakesLevel 3-3-1",  "SpaceCakesLevel 3-3-2",  "SpaceCakesLevel 3-3-3", 
                                                             "SpaceCakesLevel 3-3-4",  "SpaceCakesLevel 3-3-5",  "SpaceCakesLevel 3-4-0",  "SpaceCakesLevel 3-4-1",  "SpaceCakesLevel 3-4-2", 
                                                             "SpaceCakesLevel 4-0-0",  "SpaceCakesLevel 4-0-1",  "SpaceCakesLevel 4-0-2",  "SpaceCakesLevel 4-0-3",  "SpaceCakesLevel 4-0-4", 
                                                             "SpaceCakesLevel 4-0-5",  "SpaceCakesLevel 4-0-6",  "SpaceCakesLevel 4-0-7",  "SpaceCakesLevel 4-1-0",  "SpaceCakesLevel 4-1-1", 
                                                             "SpaceCakesLevel 4-1-2",  "SpaceCakesLevel 4-1-3",  "SpaceCakesLevel 4-1-4",  "SpaceCakesLevel 4-1-5",  "SpaceCakesLevel 4-2-0", 
                                                             "SpaceCakesLevel 4-2-1",  "SpaceCakesLevel 4-2-2",  "SpaceCakesLevel 4-3-0",  "SpaceCakesLevel 4-3-1",  "SpaceCakesLevel 4-4-0", 
                                                             "SpaceCakesLevel 4-4-1",  "SpaceCakesLevel 4-4-2",  "SpaceCakesLevel 4-4-3",  "SpaceCakesLevel 5-0-0",  "SpaceCakesLevel 5-0-1", 
                                                             "SpaceCakesLevel 5-1-0",  "SpaceCakesLevel 5-2-0",  "SpaceCakesLevel 5-3-0",  "SpaceCakesLevel 5-4-0",  "SpaceCakesLevel 6-0-0", 
                                                             "SpaceCakesLevel 6-1-0",  "SpaceCakesLevel 6-2-0"),
                                      ordered = TRUE), #need to give the order because numbers read as characters are wrong,
         highestLevel_sess_2 = factor(highestLevel_sess_2, levels = c( "SpaceCakesLevel 0-0-0", "SpaceCakesLevel 0-0-1", "SpaceCakesLevel 0-0-2",  "SpaceCakesLevel 0-0-3",  "SpaceCakesLevel 0-0-4", 
                                                             "SpaceCakesLevel 0-0-5",  "SpaceCakesLevel 0-0-6",  "SpaceCakesLevel 0-0-7",  "SpaceCakesLevel 0-0-8",  "SpaceCakesLevel 0-0-9", 
                                                             "SpaceCakesLevel 0-1-0",  "SpaceCakesLevel 0-1-1",  "SpaceCakesLevel 0-1-2",  "SpaceCakesLevel 0-1-3",  "SpaceCakesLevel 0-1-4", 
                                                             "SpaceCakesLevel 0-1-5",  "SpaceCakesLevel 0-1-6",  "SpaceCakesLevel 0-1-7",  "SpaceCakesLevel 0-2-0",  "SpaceCakesLevel 0-2-1", 
                                                             "SpaceCakesLevel 0-2-2",  "SpaceCakesLevel 0-2-3",  "SpaceCakesLevel 0-2-4",  "SpaceCakesLevel 0-2-5",  "SpaceCakesLevel 0-2-6", 
                                                             "SpaceCakesLevel 0-2-7",  "SpaceCakesLevel 0-2-8",  "SpaceCakesLevel 0-3-0",  "SpaceCakesLevel 0-3-1",  "SpaceCakesLevel 0-3-2", 
                                                             "SpaceCakesLevel 0-3-3",  "SpaceCakesLevel 0-3-4",  "SpaceCakesLevel 0-3-5",  "SpaceCakesLevel 0-3-6",  "SpaceCakesLevel 0-3-7", 
                                                             "SpaceCakesLevel 0-3-8",  "SpaceCakesLevel 0-4-0",  "SpaceCakesLevel 0-4-1", "SpaceCakesLevel 0-4-2",  "SpaceCakesLevel 0-4-3",
                                                             "SpaceCakesLevel 0-4-4",  "SpaceCakesLevel 0-4-5",  "SpaceCakesLevel 0-4-6", "SpaceCakesLevel 0-4-7",  "SpaceCakesLevel 0-4-8",
                                                             "SpaceCakesLevel 0-4-9", "SpaceCakesLevel 0-4-10", "SpaceCakesLevel 0-4-11", "SpaceCakesLevel 1-0-0", "SpaceCakesLevel 1-0-1",
                                                             "SpaceCakesLevel 1-0-2",  "SpaceCakesLevel 1-0-3",  "SpaceCakesLevel 1-0-4",  "SpaceCakesLevel 1-0-5", "SpaceCakesLevel 1-0-6",
                                                             "SpaceCakesLevel 1-0-7",  "SpaceCakesLevel 1-0-8",  "SpaceCakesLevel 1-0-9",  "SpaceCakesLevel 1-0-10", "SpaceCakesLevel 1-1-0",
                                                             "SpaceCakesLevel 1-1-1",  "SpaceCakesLevel 1-1-2",  "SpaceCakesLevel 1-1-3",  "SpaceCakesLevel 1-1-4",  "SpaceCakesLevel 1-2-0", 
                                                             "SpaceCakesLevel 1-2-1",  "SpaceCakesLevel 1-2-2",  "SpaceCakesLevel 1-2-3",  "SpaceCakesLevel 1-2-4",  "SpaceCakesLevel 1-2-5", 
                                                             "SpaceCakesLevel 1-2-6",  "SpaceCakesLevel 1-2-7", "SpaceCakesLevel 1-2-8",  "SpaceCakesLevel 1-3-0",  "SpaceCakesLevel 1-3-1", 
                                                             "SpaceCakesLevel 1-3-2",  "SpaceCakesLevel 1-3-3",  "SpaceCakesLevel 1-3-4",  "SpaceCakesLevel 1-3-5",  "SpaceCakesLevel 1-3-6", 
                                                             "SpaceCakesLevel 1-3-7",  "SpaceCakesLevel 1-3-8",  "SpaceCakesLevel 1-3-9",  "SpaceCakesLevel 1-4-0",  "SpaceCakesLevel 1-4-1", 
                                                             "SpaceCakesLevel 1-4-2",  "SpaceCakesLevel 1-4-3",  "SpaceCakesLevel 1-4-4",  "SpaceCakesLevel 1-4-5",  "SpaceCakesLevel 2-0-0", 
                                                             "SpaceCakesLevel 2-0-1",  "SpaceCakesLevel 2-0-2",  "SpaceCakesLevel 2-0-3",  "SpaceCakesLevel 2-0-4",  "SpaceCakesLevel 2-0-5", 
                                                             "SpaceCakesLevel 2-0-6",  "SpaceCakesLevel 2-0-7",  "SpaceCakesLevel 2-0-8",  "SpaceCakesLevel 2-0-9",  "SpaceCakesLevel 2-0-10",  
                                                             "SpaceCakesLevel 2-0-11", "SpaceCakesLevel 2-0-12", "SpaceCakesLevel 2-0-13", "SpaceCakesLevel 2-0-14",  "SpaceCakesLevel 2-1-0", 
                                                             "SpaceCakesLevel 2-1-1",  "SpaceCakesLevel 2-1-2",  "SpaceCakesLevel 2-1-3",  "SpaceCakesLevel 2-1-4",  "SpaceCakesLevel 2-1-5", 
                                                             "SpaceCakesLevel 2-1-6",  "SpaceCakesLevel 2-1-7",  "SpaceCakesLevel 2-1-8",  "SpaceCakesLevel 2-1-9",  "SpaceCakesLevel 2-1-10", 
                                                             "SpaceCakesLevel 2-1-11", "SpaceCakesLevel 2-1-12", "SpaceCakesLevel 2-1-13", "SpaceCakesLevel 2-1-14",  "SpaceCakesLevel 2-2-0", 
                                                             "SpaceCakesLevel 2-2-1",  "SpaceCakesLevel 2-2-2",  "SpaceCakesLevel 2-2-3",  "SpaceCakesLevel 2-2-4",  "SpaceCakesLevel 2-2-5", 
                                                             "SpaceCakesLevel 2-2-6",  "SpaceCakesLevel 2-3-0",  "SpaceCakesLevel 2-3-1",  "SpaceCakesLevel 2-3-2",  "SpaceCakesLevel 2-3-3", 
                                                             "SpaceCakesLevel 2-3-4",  "SpaceCakesLevel 2-4-0",  "SpaceCakesLevel 2-4-1",  "SpaceCakesLevel 2-4-2",  "SpaceCakesLevel 2-4-3", 
                                                             "SpaceCakesLevel 2-4-4",  "SpaceCakesLevel 3-0-0",  "SpaceCakesLevel 3-0-1",  "SpaceCakesLevel 3-0-2",  "SpaceCakesLevel 3-0-3", 
                                                             "SpaceCakesLevel 3-0-4",  "SpaceCakesLevel 3-0-5",  "SpaceCakesLevel 3-0-6",  "SpaceCakesLevel 3-0-7",  "SpaceCakesLevel 3-1-0", 
                                                             "SpaceCakesLevel 3-1-1",  "SpaceCakesLevel 3-1-2",  "SpaceCakesLevel 3-1-3",  "SpaceCakesLevel 3-2-0",  "SpaceCakesLevel 3-2-1", 
                                                             "SpaceCakesLevel 3-2-2",  "SpaceCakesLevel 3-3-0",  "SpaceCakesLevel 3-3-1",  "SpaceCakesLevel 3-3-2",  "SpaceCakesLevel 3-3-3", 
                                                             "SpaceCakesLevel 3-3-4",  "SpaceCakesLevel 3-3-5",  "SpaceCakesLevel 3-4-0",  "SpaceCakesLevel 3-4-1",  "SpaceCakesLevel 3-4-2", 
                                                             "SpaceCakesLevel 4-0-0",  "SpaceCakesLevel 4-0-1",  "SpaceCakesLevel 4-0-2",  "SpaceCakesLevel 4-0-3",  "SpaceCakesLevel 4-0-4", 
                                                             "SpaceCakesLevel 4-0-5",  "SpaceCakesLevel 4-0-6",  "SpaceCakesLevel 4-0-7",  "SpaceCakesLevel 4-1-0",  "SpaceCakesLevel 4-1-1", 
                                                             "SpaceCakesLevel 4-1-2",  "SpaceCakesLevel 4-1-3",  "SpaceCakesLevel 4-1-4",  "SpaceCakesLevel 4-1-5",  "SpaceCakesLevel 4-2-0", 
                                                             "SpaceCakesLevel 4-2-1",  "SpaceCakesLevel 4-2-2",  "SpaceCakesLevel 4-3-0",  "SpaceCakesLevel 4-3-1",  "SpaceCakesLevel 4-4-0", 
                                                             "SpaceCakesLevel 4-4-1",  "SpaceCakesLevel 4-4-2",  "SpaceCakesLevel 4-4-3",  "SpaceCakesLevel 5-0-0",  "SpaceCakesLevel 5-0-1", 
                                                             "SpaceCakesLevel 5-1-0",  "SpaceCakesLevel 5-2-0",  "SpaceCakesLevel 5-3-0",  "SpaceCakesLevel 5-4-0",  "SpaceCakesLevel 6-0-0", 
                                                             "SpaceCakesLevel 6-1-0",  "SpaceCakesLevel 6-2-0"),
                                      ordered = TRUE), #need to give the order because numbers read as characters are wrong,
         highestLevel_sess_3 = factor(highestLevel_sess_3, levels = c( "SpaceCakesLevel 0-0-0", "SpaceCakesLevel 0-0-1", "SpaceCakesLevel 0-0-2",  "SpaceCakesLevel 0-0-3",  "SpaceCakesLevel 0-0-4", 
                                                             "SpaceCakesLevel 0-0-5",  "SpaceCakesLevel 0-0-6",  "SpaceCakesLevel 0-0-7",  "SpaceCakesLevel 0-0-8",  "SpaceCakesLevel 0-0-9", 
                                                             "SpaceCakesLevel 0-1-0",  "SpaceCakesLevel 0-1-1",  "SpaceCakesLevel 0-1-2",  "SpaceCakesLevel 0-1-3",  "SpaceCakesLevel 0-1-4", 
                                                             "SpaceCakesLevel 0-1-5",  "SpaceCakesLevel 0-1-6",  "SpaceCakesLevel 0-1-7",  "SpaceCakesLevel 0-2-0",  "SpaceCakesLevel 0-2-1", 
                                                             "SpaceCakesLevel 0-2-2",  "SpaceCakesLevel 0-2-3",  "SpaceCakesLevel 0-2-4",  "SpaceCakesLevel 0-2-5",  "SpaceCakesLevel 0-2-6", 
                                                             "SpaceCakesLevel 0-2-7",  "SpaceCakesLevel 0-2-8",  "SpaceCakesLevel 0-3-0",  "SpaceCakesLevel 0-3-1",  "SpaceCakesLevel 0-3-2", 
                                                             "SpaceCakesLevel 0-3-3",  "SpaceCakesLevel 0-3-4",  "SpaceCakesLevel 0-3-5",  "SpaceCakesLevel 0-3-6",  "SpaceCakesLevel 0-3-7", 
                                                             "SpaceCakesLevel 0-3-8",  "SpaceCakesLevel 0-4-0",  "SpaceCakesLevel 0-4-1", "SpaceCakesLevel 0-4-2",  "SpaceCakesLevel 0-4-3",
                                                             "SpaceCakesLevel 0-4-4",  "SpaceCakesLevel 0-4-5",  "SpaceCakesLevel 0-4-6", "SpaceCakesLevel 0-4-7",  "SpaceCakesLevel 0-4-8",
                                                             "SpaceCakesLevel 0-4-9", "SpaceCakesLevel 0-4-10", "SpaceCakesLevel 0-4-11", "SpaceCakesLevel 1-0-0", "SpaceCakesLevel 1-0-1",
                                                             "SpaceCakesLevel 1-0-2",  "SpaceCakesLevel 1-0-3",  "SpaceCakesLevel 1-0-4",  "SpaceCakesLevel 1-0-5", "SpaceCakesLevel 1-0-6",
                                                             "SpaceCakesLevel 1-0-7",  "SpaceCakesLevel 1-0-8",  "SpaceCakesLevel 1-0-9",  "SpaceCakesLevel 1-0-10", "SpaceCakesLevel 1-1-0",
                                                             "SpaceCakesLevel 1-1-1",  "SpaceCakesLevel 1-1-2",  "SpaceCakesLevel 1-1-3",  "SpaceCakesLevel 1-1-4",  "SpaceCakesLevel 1-2-0", 
                                                             "SpaceCakesLevel 1-2-1",  "SpaceCakesLevel 1-2-2",  "SpaceCakesLevel 1-2-3",  "SpaceCakesLevel 1-2-4",  "SpaceCakesLevel 1-2-5", 
                                                             "SpaceCakesLevel 1-2-6",  "SpaceCakesLevel 1-2-7", "SpaceCakesLevel 1-2-8",  "SpaceCakesLevel 1-3-0",  "SpaceCakesLevel 1-3-1", 
                                                             "SpaceCakesLevel 1-3-2",  "SpaceCakesLevel 1-3-3",  "SpaceCakesLevel 1-3-4",  "SpaceCakesLevel 1-3-5",  "SpaceCakesLevel 1-3-6", 
                                                             "SpaceCakesLevel 1-3-7",  "SpaceCakesLevel 1-3-8",  "SpaceCakesLevel 1-3-9",  "SpaceCakesLevel 1-4-0",  "SpaceCakesLevel 1-4-1", 
                                                             "SpaceCakesLevel 1-4-2",  "SpaceCakesLevel 1-4-3",  "SpaceCakesLevel 1-4-4",  "SpaceCakesLevel 1-4-5",  "SpaceCakesLevel 2-0-0", 
                                                             "SpaceCakesLevel 2-0-1",  "SpaceCakesLevel 2-0-2",  "SpaceCakesLevel 2-0-3",  "SpaceCakesLevel 2-0-4",  "SpaceCakesLevel 2-0-5", 
                                                             "SpaceCakesLevel 2-0-6",  "SpaceCakesLevel 2-0-7",  "SpaceCakesLevel 2-0-8",  "SpaceCakesLevel 2-0-9",  "SpaceCakesLevel 2-0-10",  
                                                             "SpaceCakesLevel 2-0-11", "SpaceCakesLevel 2-0-12", "SpaceCakesLevel 2-0-13", "SpaceCakesLevel 2-0-14",  "SpaceCakesLevel 2-1-0", 
                                                             "SpaceCakesLevel 2-1-1",  "SpaceCakesLevel 2-1-2",  "SpaceCakesLevel 2-1-3",  "SpaceCakesLevel 2-1-4",  "SpaceCakesLevel 2-1-5", 
                                                             "SpaceCakesLevel 2-1-6",  "SpaceCakesLevel 2-1-7",  "SpaceCakesLevel 2-1-8",  "SpaceCakesLevel 2-1-9",  "SpaceCakesLevel 2-1-10", 
                                                             "SpaceCakesLevel 2-1-11", "SpaceCakesLevel 2-1-12", "SpaceCakesLevel 2-1-13", "SpaceCakesLevel 2-1-14",  "SpaceCakesLevel 2-2-0", 
                                                             "SpaceCakesLevel 2-2-1",  "SpaceCakesLevel 2-2-2",  "SpaceCakesLevel 2-2-3",  "SpaceCakesLevel 2-2-4",  "SpaceCakesLevel 2-2-5", 
                                                             "SpaceCakesLevel 2-2-6",  "SpaceCakesLevel 2-3-0",  "SpaceCakesLevel 2-3-1",  "SpaceCakesLevel 2-3-2",  "SpaceCakesLevel 2-3-3", 
                                                             "SpaceCakesLevel 2-3-4",  "SpaceCakesLevel 2-4-0",  "SpaceCakesLevel 2-4-1",  "SpaceCakesLevel 2-4-2",  "SpaceCakesLevel 2-4-3", 
                                                             "SpaceCakesLevel 2-4-4",  "SpaceCakesLevel 3-0-0",  "SpaceCakesLevel 3-0-1",  "SpaceCakesLevel 3-0-2",  "SpaceCakesLevel 3-0-3", 
                                                             "SpaceCakesLevel 3-0-4",  "SpaceCakesLevel 3-0-5",  "SpaceCakesLevel 3-0-6",  "SpaceCakesLevel 3-0-7",  "SpaceCakesLevel 3-1-0", 
                                                             "SpaceCakesLevel 3-1-1",  "SpaceCakesLevel 3-1-2",  "SpaceCakesLevel 3-1-3",  "SpaceCakesLevel 3-2-0",  "SpaceCakesLevel 3-2-1", 
                                                             "SpaceCakesLevel 3-2-2",  "SpaceCakesLevel 3-3-0",  "SpaceCakesLevel 3-3-1",  "SpaceCakesLevel 3-3-2",  "SpaceCakesLevel 3-3-3", 
                                                             "SpaceCakesLevel 3-3-4",  "SpaceCakesLevel 3-3-5",  "SpaceCakesLevel 3-4-0",  "SpaceCakesLevel 3-4-1",  "SpaceCakesLevel 3-4-2", 
                                                             "SpaceCakesLevel 4-0-0",  "SpaceCakesLevel 4-0-1",  "SpaceCakesLevel 4-0-2",  "SpaceCakesLevel 4-0-3",  "SpaceCakesLevel 4-0-4", 
                                                             "SpaceCakesLevel 4-0-5",  "SpaceCakesLevel 4-0-6",  "SpaceCakesLevel 4-0-7",  "SpaceCakesLevel 4-1-0",  "SpaceCakesLevel 4-1-1", 
                                                             "SpaceCakesLevel 4-1-2",  "SpaceCakesLevel 4-1-3",  "SpaceCakesLevel 4-1-4",  "SpaceCakesLevel 4-1-5",  "SpaceCakesLevel 4-2-0", 
                                                             "SpaceCakesLevel 4-2-1",  "SpaceCakesLevel 4-2-2",  "SpaceCakesLevel 4-3-0",  "SpaceCakesLevel 4-3-1",  "SpaceCakesLevel 4-4-0", 
                                                             "SpaceCakesLevel 4-4-1",  "SpaceCakesLevel 4-4-2",  "SpaceCakesLevel 4-4-3",  "SpaceCakesLevel 5-0-0",  "SpaceCakesLevel 5-0-1", 
                                                             "SpaceCakesLevel 5-1-0",  "SpaceCakesLevel 5-2-0",  "SpaceCakesLevel 5-3-0",  "SpaceCakesLevel 5-4-0",  "SpaceCakesLevel 6-0-0", 
                                                             "SpaceCakesLevel 6-1-0",  "SpaceCakesLevel 6-2-0"),
                                      ordered = TRUE), #need to give the order because numbers read as characters are wrong,
         highestLevel_sess_4 = factor(highestLevel_sess_4, levels = c( "SpaceCakesLevel 0-0-0", "SpaceCakesLevel 0-0-1", "SpaceCakesLevel 0-0-2",  "SpaceCakesLevel 0-0-3",  "SpaceCakesLevel 0-0-4", 
                                                             "SpaceCakesLevel 0-0-5",  "SpaceCakesLevel 0-0-6",  "SpaceCakesLevel 0-0-7",  "SpaceCakesLevel 0-0-8",  "SpaceCakesLevel 0-0-9", 
                                                             "SpaceCakesLevel 0-1-0",  "SpaceCakesLevel 0-1-1",  "SpaceCakesLevel 0-1-2",  "SpaceCakesLevel 0-1-3",  "SpaceCakesLevel 0-1-4", 
                                                             "SpaceCakesLevel 0-1-5",  "SpaceCakesLevel 0-1-6",  "SpaceCakesLevel 0-1-7",  "SpaceCakesLevel 0-2-0",  "SpaceCakesLevel 0-2-1", 
                                                             "SpaceCakesLevel 0-2-2",  "SpaceCakesLevel 0-2-3",  "SpaceCakesLevel 0-2-4",  "SpaceCakesLevel 0-2-5",  "SpaceCakesLevel 0-2-6", 
                                                             "SpaceCakesLevel 0-2-7",  "SpaceCakesLevel 0-2-8",  "SpaceCakesLevel 0-3-0",  "SpaceCakesLevel 0-3-1",  "SpaceCakesLevel 0-3-2", 
                                                             "SpaceCakesLevel 0-3-3",  "SpaceCakesLevel 0-3-4",  "SpaceCakesLevel 0-3-5",  "SpaceCakesLevel 0-3-6",  "SpaceCakesLevel 0-3-7", 
                                                             "SpaceCakesLevel 0-3-8",  "SpaceCakesLevel 0-4-0",  "SpaceCakesLevel 0-4-1", "SpaceCakesLevel 0-4-2",  "SpaceCakesLevel 0-4-3",
                                                             "SpaceCakesLevel 0-4-4",  "SpaceCakesLevel 0-4-5",  "SpaceCakesLevel 0-4-6", "SpaceCakesLevel 0-4-7",  "SpaceCakesLevel 0-4-8",
                                                             "SpaceCakesLevel 0-4-9", "SpaceCakesLevel 0-4-10", "SpaceCakesLevel 0-4-11", "SpaceCakesLevel 1-0-0", "SpaceCakesLevel 1-0-1",
                                                             "SpaceCakesLevel 1-0-2",  "SpaceCakesLevel 1-0-3",  "SpaceCakesLevel 1-0-4",  "SpaceCakesLevel 1-0-5", "SpaceCakesLevel 1-0-6",
                                                             "SpaceCakesLevel 1-0-7",  "SpaceCakesLevel 1-0-8",  "SpaceCakesLevel 1-0-9",  "SpaceCakesLevel 1-0-10", "SpaceCakesLevel 1-1-0",
                                                             "SpaceCakesLevel 1-1-1",  "SpaceCakesLevel 1-1-2",  "SpaceCakesLevel 1-1-3",  "SpaceCakesLevel 1-1-4",  "SpaceCakesLevel 1-2-0", 
                                                             "SpaceCakesLevel 1-2-1",  "SpaceCakesLevel 1-2-2",  "SpaceCakesLevel 1-2-3",  "SpaceCakesLevel 1-2-4",  "SpaceCakesLevel 1-2-5", 
                                                             "SpaceCakesLevel 1-2-6",  "SpaceCakesLevel 1-2-7", "SpaceCakesLevel 1-2-8",  "SpaceCakesLevel 1-3-0",  "SpaceCakesLevel 1-3-1", 
                                                             "SpaceCakesLevel 1-3-2",  "SpaceCakesLevel 1-3-3",  "SpaceCakesLevel 1-3-4",  "SpaceCakesLevel 1-3-5",  "SpaceCakesLevel 1-3-6", 
                                                             "SpaceCakesLevel 1-3-7",  "SpaceCakesLevel 1-3-8",  "SpaceCakesLevel 1-3-9",  "SpaceCakesLevel 1-4-0",  "SpaceCakesLevel 1-4-1", 
                                                             "SpaceCakesLevel 1-4-2",  "SpaceCakesLevel 1-4-3",  "SpaceCakesLevel 1-4-4",  "SpaceCakesLevel 1-4-5",  "SpaceCakesLevel 2-0-0", 
                                                             "SpaceCakesLevel 2-0-1",  "SpaceCakesLevel 2-0-2",  "SpaceCakesLevel 2-0-3",  "SpaceCakesLevel 2-0-4",  "SpaceCakesLevel 2-0-5", 
                                                             "SpaceCakesLevel 2-0-6",  "SpaceCakesLevel 2-0-7",  "SpaceCakesLevel 2-0-8",  "SpaceCakesLevel 2-0-9",  "SpaceCakesLevel 2-0-10",  
                                                             "SpaceCakesLevel 2-0-11", "SpaceCakesLevel 2-0-12", "SpaceCakesLevel 2-0-13", "SpaceCakesLevel 2-0-14",  "SpaceCakesLevel 2-1-0", 
                                                             "SpaceCakesLevel 2-1-1",  "SpaceCakesLevel 2-1-2",  "SpaceCakesLevel 2-1-3",  "SpaceCakesLevel 2-1-4",  "SpaceCakesLevel 2-1-5", 
                                                             "SpaceCakesLevel 2-1-6",  "SpaceCakesLevel 2-1-7",  "SpaceCakesLevel 2-1-8",  "SpaceCakesLevel 2-1-9",  "SpaceCakesLevel 2-1-10", 
                                                             "SpaceCakesLevel 2-1-11", "SpaceCakesLevel 2-1-12", "SpaceCakesLevel 2-1-13", "SpaceCakesLevel 2-1-14",  "SpaceCakesLevel 2-2-0", 
                                                             "SpaceCakesLevel 2-2-1",  "SpaceCakesLevel 2-2-2",  "SpaceCakesLevel 2-2-3",  "SpaceCakesLevel 2-2-4",  "SpaceCakesLevel 2-2-5", 
                                                             "SpaceCakesLevel 2-2-6",  "SpaceCakesLevel 2-3-0",  "SpaceCakesLevel 2-3-1",  "SpaceCakesLevel 2-3-2",  "SpaceCakesLevel 2-3-3", 
                                                             "SpaceCakesLevel 2-3-4",  "SpaceCakesLevel 2-4-0",  "SpaceCakesLevel 2-4-1",  "SpaceCakesLevel 2-4-2",  "SpaceCakesLevel 2-4-3", 
                                                             "SpaceCakesLevel 2-4-4",  "SpaceCakesLevel 3-0-0",  "SpaceCakesLevel 3-0-1",  "SpaceCakesLevel 3-0-2",  "SpaceCakesLevel 3-0-3", 
                                                             "SpaceCakesLevel 3-0-4",  "SpaceCakesLevel 3-0-5",  "SpaceCakesLevel 3-0-6",  "SpaceCakesLevel 3-0-7",  "SpaceCakesLevel 3-1-0", 
                                                             "SpaceCakesLevel 3-1-1",  "SpaceCakesLevel 3-1-2",  "SpaceCakesLevel 3-1-3",  "SpaceCakesLevel 3-2-0",  "SpaceCakesLevel 3-2-1", 
                                                             "SpaceCakesLevel 3-2-2",  "SpaceCakesLevel 3-3-0",  "SpaceCakesLevel 3-3-1",  "SpaceCakesLevel 3-3-2",  "SpaceCakesLevel 3-3-3", 
                                                             "SpaceCakesLevel 3-3-4",  "SpaceCakesLevel 3-3-5",  "SpaceCakesLevel 3-4-0",  "SpaceCakesLevel 3-4-1",  "SpaceCakesLevel 3-4-2", 
                                                             "SpaceCakesLevel 4-0-0",  "SpaceCakesLevel 4-0-1",  "SpaceCakesLevel 4-0-2",  "SpaceCakesLevel 4-0-3",  "SpaceCakesLevel 4-0-4", 
                                                             "SpaceCakesLevel 4-0-5",  "SpaceCakesLevel 4-0-6",  "SpaceCakesLevel 4-0-7",  "SpaceCakesLevel 4-1-0",  "SpaceCakesLevel 4-1-1", 
                                                             "SpaceCakesLevel 4-1-2",  "SpaceCakesLevel 4-1-3",  "SpaceCakesLevel 4-1-4",  "SpaceCakesLevel 4-1-5",  "SpaceCakesLevel 4-2-0", 
                                                             "SpaceCakesLevel 4-2-1",  "SpaceCakesLevel 4-2-2",  "SpaceCakesLevel 4-3-0",  "SpaceCakesLevel 4-3-1",  "SpaceCakesLevel 4-4-0", 
                                                             "SpaceCakesLevel 4-4-1",  "SpaceCakesLevel 4-4-2",  "SpaceCakesLevel 4-4-3",  "SpaceCakesLevel 5-0-0",  "SpaceCakesLevel 5-0-1", 
                                                             "SpaceCakesLevel 5-1-0",  "SpaceCakesLevel 5-2-0",  "SpaceCakesLevel 5-3-0",  "SpaceCakesLevel 5-4-0",  "SpaceCakesLevel 6-0-0", 
                                                             "SpaceCakesLevel 6-1-0",  "SpaceCakesLevel 6-2-0"),
                                      ordered = TRUE), #need to give the order because numbers read as characters are wrong,
         highestLevel_sess_5 = factor(highestLevel_sess_5, levels = c( "SpaceCakesLevel 0-0-0", "SpaceCakesLevel 0-0-1", "SpaceCakesLevel 0-0-2",  "SpaceCakesLevel 0-0-3",  "SpaceCakesLevel 0-0-4", 
                                                             "SpaceCakesLevel 0-0-5",  "SpaceCakesLevel 0-0-6",  "SpaceCakesLevel 0-0-7",  "SpaceCakesLevel 0-0-8",  "SpaceCakesLevel 0-0-9", 
                                                             "SpaceCakesLevel 0-1-0",  "SpaceCakesLevel 0-1-1",  "SpaceCakesLevel 0-1-2",  "SpaceCakesLevel 0-1-3",  "SpaceCakesLevel 0-1-4", 
                                                             "SpaceCakesLevel 0-1-5",  "SpaceCakesLevel 0-1-6",  "SpaceCakesLevel 0-1-7",  "SpaceCakesLevel 0-2-0",  "SpaceCakesLevel 0-2-1", 
                                                             "SpaceCakesLevel 0-2-2",  "SpaceCakesLevel 0-2-3",  "SpaceCakesLevel 0-2-4",  "SpaceCakesLevel 0-2-5",  "SpaceCakesLevel 0-2-6", 
                                                             "SpaceCakesLevel 0-2-7",  "SpaceCakesLevel 0-2-8",  "SpaceCakesLevel 0-3-0",  "SpaceCakesLevel 0-3-1",  "SpaceCakesLevel 0-3-2", 
                                                             "SpaceCakesLevel 0-3-3",  "SpaceCakesLevel 0-3-4",  "SpaceCakesLevel 0-3-5",  "SpaceCakesLevel 0-3-6",  "SpaceCakesLevel 0-3-7", 
                                                             "SpaceCakesLevel 0-3-8",  "SpaceCakesLevel 0-4-0",  "SpaceCakesLevel 0-4-1", "SpaceCakesLevel 0-4-2",  "SpaceCakesLevel 0-4-3",
                                                             "SpaceCakesLevel 0-4-4",  "SpaceCakesLevel 0-4-5",  "SpaceCakesLevel 0-4-6", "SpaceCakesLevel 0-4-7",  "SpaceCakesLevel 0-4-8",
                                                             "SpaceCakesLevel 0-4-9", "SpaceCakesLevel 0-4-10", "SpaceCakesLevel 0-4-11", "SpaceCakesLevel 1-0-0", "SpaceCakesLevel 1-0-1",
                                                             "SpaceCakesLevel 1-0-2",  "SpaceCakesLevel 1-0-3",  "SpaceCakesLevel 1-0-4",  "SpaceCakesLevel 1-0-5", "SpaceCakesLevel 1-0-6",
                                                             "SpaceCakesLevel 1-0-7",  "SpaceCakesLevel 1-0-8",  "SpaceCakesLevel 1-0-9",  "SpaceCakesLevel 1-0-10", "SpaceCakesLevel 1-1-0",
                                                             "SpaceCakesLevel 1-1-1",  "SpaceCakesLevel 1-1-2",  "SpaceCakesLevel 1-1-3",  "SpaceCakesLevel 1-1-4",  "SpaceCakesLevel 1-2-0", 
                                                             "SpaceCakesLevel 1-2-1",  "SpaceCakesLevel 1-2-2",  "SpaceCakesLevel 1-2-3",  "SpaceCakesLevel 1-2-4",  "SpaceCakesLevel 1-2-5", 
                                                             "SpaceCakesLevel 1-2-6",  "SpaceCakesLevel 1-2-7", "SpaceCakesLevel 1-2-8",  "SpaceCakesLevel 1-3-0",  "SpaceCakesLevel 1-3-1", 
                                                             "SpaceCakesLevel 1-3-2",  "SpaceCakesLevel 1-3-3",  "SpaceCakesLevel 1-3-4",  "SpaceCakesLevel 1-3-5",  "SpaceCakesLevel 1-3-6", 
                                                             "SpaceCakesLevel 1-3-7",  "SpaceCakesLevel 1-3-8",  "SpaceCakesLevel 1-3-9",  "SpaceCakesLevel 1-4-0",  "SpaceCakesLevel 1-4-1", 
                                                             "SpaceCakesLevel 1-4-2",  "SpaceCakesLevel 1-4-3",  "SpaceCakesLevel 1-4-4",  "SpaceCakesLevel 1-4-5",  "SpaceCakesLevel 2-0-0", 
                                                             "SpaceCakesLevel 2-0-1",  "SpaceCakesLevel 2-0-2",  "SpaceCakesLevel 2-0-3",  "SpaceCakesLevel 2-0-4",  "SpaceCakesLevel 2-0-5", 
                                                             "SpaceCakesLevel 2-0-6",  "SpaceCakesLevel 2-0-7",  "SpaceCakesLevel 2-0-8",  "SpaceCakesLevel 2-0-9",  "SpaceCakesLevel 2-0-10",  
                                                             "SpaceCakesLevel 2-0-11", "SpaceCakesLevel 2-0-12", "SpaceCakesLevel 2-0-13", "SpaceCakesLevel 2-0-14",  "SpaceCakesLevel 2-1-0", 
                                                             "SpaceCakesLevel 2-1-1",  "SpaceCakesLevel 2-1-2",  "SpaceCakesLevel 2-1-3",  "SpaceCakesLevel 2-1-4",  "SpaceCakesLevel 2-1-5", 
                                                             "SpaceCakesLevel 2-1-6",  "SpaceCakesLevel 2-1-7",  "SpaceCakesLevel 2-1-8",  "SpaceCakesLevel 2-1-9",  "SpaceCakesLevel 2-1-10", 
                                                             "SpaceCakesLevel 2-1-11", "SpaceCakesLevel 2-1-12", "SpaceCakesLevel 2-1-13", "SpaceCakesLevel 2-1-14",  "SpaceCakesLevel 2-2-0", 
                                                             "SpaceCakesLevel 2-2-1",  "SpaceCakesLevel 2-2-2",  "SpaceCakesLevel 2-2-3",  "SpaceCakesLevel 2-2-4",  "SpaceCakesLevel 2-2-5", 
                                                             "SpaceCakesLevel 2-2-6",  "SpaceCakesLevel 2-3-0",  "SpaceCakesLevel 2-3-1",  "SpaceCakesLevel 2-3-2",  "SpaceCakesLevel 2-3-3", 
                                                             "SpaceCakesLevel 2-3-4",  "SpaceCakesLevel 2-4-0",  "SpaceCakesLevel 2-4-1",  "SpaceCakesLevel 2-4-2",  "SpaceCakesLevel 2-4-3", 
                                                             "SpaceCakesLevel 2-4-4",  "SpaceCakesLevel 3-0-0",  "SpaceCakesLevel 3-0-1",  "SpaceCakesLevel 3-0-2",  "SpaceCakesLevel 3-0-3", 
                                                             "SpaceCakesLevel 3-0-4",  "SpaceCakesLevel 3-0-5",  "SpaceCakesLevel 3-0-6",  "SpaceCakesLevel 3-0-7",  "SpaceCakesLevel 3-1-0", 
                                                             "SpaceCakesLevel 3-1-1",  "SpaceCakesLevel 3-1-2",  "SpaceCakesLevel 3-1-3",  "SpaceCakesLevel 3-2-0",  "SpaceCakesLevel 3-2-1", 
                                                             "SpaceCakesLevel 3-2-2",  "SpaceCakesLevel 3-3-0",  "SpaceCakesLevel 3-3-1",  "SpaceCakesLevel 3-3-2",  "SpaceCakesLevel 3-3-3", 
                                                             "SpaceCakesLevel 3-3-4",  "SpaceCakesLevel 3-3-5",  "SpaceCakesLevel 3-4-0",  "SpaceCakesLevel 3-4-1",  "SpaceCakesLevel 3-4-2", 
                                                             "SpaceCakesLevel 4-0-0",  "SpaceCakesLevel 4-0-1",  "SpaceCakesLevel 4-0-2",  "SpaceCakesLevel 4-0-3",  "SpaceCakesLevel 4-0-4", 
                                                             "SpaceCakesLevel 4-0-5",  "SpaceCakesLevel 4-0-6",  "SpaceCakesLevel 4-0-7",  "SpaceCakesLevel 4-1-0",  "SpaceCakesLevel 4-1-1", 
                                                             "SpaceCakesLevel 4-1-2",  "SpaceCakesLevel 4-1-3",  "SpaceCakesLevel 4-1-4",  "SpaceCakesLevel 4-1-5",  "SpaceCakesLevel 4-2-0", 
                                                             "SpaceCakesLevel 4-2-1",  "SpaceCakesLevel 4-2-2",  "SpaceCakesLevel 4-3-0",  "SpaceCakesLevel 4-3-1",  "SpaceCakesLevel 4-4-0", 
                                                             "SpaceCakesLevel 4-4-1",  "SpaceCakesLevel 4-4-2",  "SpaceCakesLevel 4-4-3",  "SpaceCakesLevel 5-0-0",  "SpaceCakesLevel 5-0-1", 
                                                             "SpaceCakesLevel 5-1-0",  "SpaceCakesLevel 5-2-0",  "SpaceCakesLevel 5-3-0",  "SpaceCakesLevel 5-4-0",  "SpaceCakesLevel 6-0-0", 
                                                             "SpaceCakesLevel 6-1-0",  "SpaceCakesLevel 6-2-0"),
                                      ordered = TRUE) #need to give the order because numbers read as characters are wrong,
                              ) %>%
 mutate(ImproverScore = case_when(ImproverScore == TRUE ~ 1, 
                                  ImproverScore == FALSE ~ 0)) %>%
  select(-c("ImproverAccuracy", "ImproverRT", "AllImprove", "ImprovedPostScoreGT7"))


#colnames(AYCET_DCCS)



#Define & Add outcomes for DCCS
  #Already calculated - improvement (based on change in DCCS NIH Score), DCCS$ImproverScore
  #New: Above the median, Accuracy of 5 vs below five
  #New: Above the median, RT Score of 2.7 vs below 2.7

#test and train data using 5 fold cross validation
#precision and accuracy graphs

###########################
#LASSO Template
#identify columsn that have Na's
#which session 5 6 can be gotten rid of
#anything with fewer than 50% of people
#do we impute mean, 0 or complete cases
#RT impute mean
#split to test train

data <- rbind(training_set, testing_set[,1:12])

x <- model.matrix(outcome ~ inspection_date + borough + cuisine + inspection_year + month +
                    weekday + num_previous_low_inspections + num_previous_med_inspections +
                    num_previous_high_inspections + num_previous_closings, data)[,-1]


x_train <- x[1:nrow(training_set),]
y_train <- training_set$outcome

x_test <- x[nrow(training_set)+1 :nrow(testing_set),] 
y_test <- testing_set$outcome


# fit lasso and ridge
#model_lasso <- glmnet(x_train, y_train, alpha=1, lambda=.01, family='binomial')
model_lasso2 <- glmnet(x_train, y_train, alpha=1, lambda=.01, family='binomial', intercept=FALSE) #without intercept

# 2. Generate predictions from both models on testing_set and calculate the AUC (for both models).
prob_lasso <- predict(model_lasso2, x_test,type='response')
pred_lasso <- prediction(prob_lasso, y_test)
perf.lasso <- performance(pred_lasso,'auc')
cat(perf.lasso@y.values[[1]])
###########################
  
#Need to remove variables with many NAs
#Lasso 1: Which variables are associated with improvement in NIH Score?

NAs_per_col <- data.frame(matrix(ncol = 2, nrow = 0))
NAs_per_col<- colSums(is.na(AYCET_DCCS))
#all _see_6 columns have a large number of NA's >140 dropping these
grep("_sess_6",colnames(AYCET_DCCS))

names(NAs_per_col)<-c("totalNA")
NAs_per_col<-NAs_per_col%>%arrange(-totalNA)
LassoNIHScore <- model.matrix(ImproverScore ~ ., AYCET_DCCS)[,-1]


ImproverScore

#Lasso 2: Which variables are associated with high accuracy at the end?

#Lasso 3: Which variables are associated with low reaction times at the end (fast, correct responses)?
