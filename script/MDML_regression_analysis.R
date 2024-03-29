require(tidyverse)
library(readr)
require(ROCR)
require(ranger)
require(glmnet)
require(dplyr)
require(ggplot2)
require(grid)
require(gridExtra)


#For Lasso regression 

#Read in AYCET gameplay data; change userID to factor for merge
AYCET_gameplay_aggregated <- read_csv("data/AYCET_gameplay_aggregated.csv") %>%mutate(userID = factor(userID))

#Read in DCCS data; change userID to factor for merge
ALL_DCCS_data <- read_csv("data/ALL_DCCS_data.csv") %>%
  mutate(userID = factor(userID)) %>%
  select(-c("dateTime.x", "dateTime.y","AllImprove", "ImprovedPostScoreGT7"))

#Merge by userID so each row represents one participant
AYCET_DCCS <- left_join(AYCET_gameplay_aggregated, ALL_DCCS_data, by = c("userID"))

#Remove users with missing DCCS data
missing_users<-c("16902", "16806", "16939" ,"16989" ,"17047", "17050","17577")

AYCET_DCCS <- AYCET_DCCS %>%
  filter(!userID %in% missing_users)

#check that each row is a unique participant
count <- AYCET_DCCS %>%
  group_by(userID) %>%
  summarize(n = n()) %>%
  filter(n > 1)

#remove variables with many NAs; Remove variables with reaction times for missed responses 
#change variable types; convert ordered factors to numbers to impute missing data
AYCET_DCCS <- AYCET_DCCS %>%
  #Drop columns that are not data for model or missing too much data
  select(-matches("_sess_6|_sess_5|Miss_AvgRT_|Miss_SDRT_")) %>%
  select(-c("accessCode", "userID", "avgRT_afterWrong_MISSED", "pre_nihScore", "post_nihScore", "pre_nihAccuracy", "post_nihAccuracy", "pre_nihRTScore", "post_nihRTScore", "DiffScore", "DiffAccuracy", "DiffRT")) %>%
  #Recode factor levels
  mutate(highestLevel_user = case_when( highestLevel_user == "SpaceCakesLevel 0-0" ~ 1, 
                                        highestLevel_user == "SpaceCakesLevel 0-1" ~ 2, 
                                        highestLevel_user == "SpaceCakesLevel 0-2" ~ 3,
                                        highestLevel_user == "SpaceCakesLevel 0-3" ~ 4, 
                                        highestLevel_user == "SpaceCakesLevel 0-4" ~ 5, 
                                        highestLevel_user == "SpaceCakesLevel 1-0" ~ 6, 
                                        highestLevel_user == "SpaceCakesLevel 1-1" ~ 7,
                                        highestLevel_user == "SpaceCakesLevel 1-2" ~ 8, 
                                        highestLevel_user == "SpaceCakesLevel 1-3" ~ 9, 
                                        highestLevel_user == "SpaceCakesLevel 1-4" ~ 10, 
                                        highestLevel_user == "SpaceCakesLevel 2-0" ~ 11,
                                        highestLevel_user == "SpaceCakesLevel 2-1" ~ 12, 
                                        highestLevel_user == "SpaceCakesLevel 2-2" ~ 13, 
                                        highestLevel_user == "SpaceCakesLevel 2-3" ~ 14, 
                                        highestLevel_user == "SpaceCakesLevel 2-4" ~ 15,
                                        highestLevel_user == "SpaceCakesLevel 3-0" ~ 16, 
                                        highestLevel_user == "SpaceCakesLevel 3-1" ~ 17, 
                                        highestLevel_user == "SpaceCakesLevel 3-2" ~ 18, 
                                        highestLevel_user == "SpaceCakesLevel 3-3" ~ 19, 
                                        highestLevel_user == "SpaceCakesLevel 3-4" ~ 20, 
                                        highestLevel_user == "SpaceCakesLevel 4-0" ~ 21, 
                                        highestLevel_user == "SpaceCakesLevel 4-1" ~ 22, 
                                        highestLevel_user == "SpaceCakesLevel 4-2" ~ 23,
                                        highestLevel_user == "SpaceCakesLevel 4-3" ~ 24,  
                                        highestLevel_user == "SpaceCakesLevel 4-4" ~ 25, 
                                        highestLevel_user == "SpaceCakesLevel 5-0" ~ 26, 
                                        highestLevel_user == "SpaceCakesLevel 5-1" ~ 27,
                                        highestLevel_user == "SpaceCakesLevel 5-2" ~ 28,  
                                        highestLevel_user == "SpaceCakesLevel 5-3" ~ 29,  
                                        highestLevel_user == "SpaceCakesLevel 5-4" ~ 30,  
                                        highestLevel_user == "SpaceCakesLevel 6-0" ~ 31, 
                                        highestLevel_user == "SpaceCakesLevel 6-1" ~ 32,  
                                        highestLevel_user == "SpaceCakesLevel 6-2" ~ 33),
         highestLevel_diff_Easy = case_when( highestLevel_diff_Easy == "SpaceCakesLevel 0-0" ~ 1, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 0-1" ~ 2, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 0-2" ~ 3,
                                             highestLevel_diff_Easy == "SpaceCakesLevel 0-3" ~ 4, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 0-4" ~ 5, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 1-0" ~ 6, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 1-1" ~ 7,
                                             highestLevel_diff_Easy == "SpaceCakesLevel 1-2" ~ 8, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 1-3" ~ 9, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 1-4" ~ 10, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 2-0" ~ 11,
                                             highestLevel_diff_Easy == "SpaceCakesLevel 2-1" ~ 12, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 2-2" ~ 13, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 2-3" ~ 14, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 2-4" ~ 15,
                                             highestLevel_diff_Easy == "SpaceCakesLevel 3-0" ~ 16, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 3-1" ~ 17, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 3-2" ~ 18, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 3-3" ~ 19, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 3-4" ~ 20, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 4-0" ~ 21, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 4-1" ~ 22, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 4-2" ~ 23,
                                             highestLevel_diff_Easy == "SpaceCakesLevel 4-3" ~ 24,  
                                             highestLevel_diff_Easy == "SpaceCakesLevel 4-4" ~ 25, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 5-0" ~ 26, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 5-1" ~ 27,
                                             highestLevel_diff_Easy == "SpaceCakesLevel 5-2" ~ 28,  
                                             highestLevel_diff_Easy == "SpaceCakesLevel 5-3" ~ 29,  
                                             highestLevel_diff_Easy == "SpaceCakesLevel 5-4" ~ 30,  
                                             highestLevel_diff_Easy == "SpaceCakesLevel 6-0" ~ 31, 
                                             highestLevel_diff_Easy == "SpaceCakesLevel 6-1" ~ 32,  
                                             highestLevel_diff_Easy == "SpaceCakesLevel 6-2" ~ 33),
         highestLevel_diff_Medium = case_when( highestLevel_diff_Medium == "SpaceCakesLevel 0-0" ~ 1, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 0-1" ~ 2, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 0-2" ~ 3,
                                               highestLevel_diff_Medium == "SpaceCakesLevel 0-3" ~ 4, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 0-4" ~ 5, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 1-0" ~ 6, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 1-1" ~ 7,
                                               highestLevel_diff_Medium == "SpaceCakesLevel 1-2" ~ 8, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 1-3" ~ 9, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 1-4" ~ 10, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 2-0" ~ 11,
                                               highestLevel_diff_Medium == "SpaceCakesLevel 2-1" ~ 12, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 2-2" ~ 13, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 2-3" ~ 14, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 2-4" ~ 15,
                                               highestLevel_diff_Medium == "SpaceCakesLevel 3-0" ~ 16, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 3-1" ~ 17, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 3-2" ~ 18, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 3-3" ~ 19, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 3-4" ~ 20, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 4-0" ~ 21, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 4-1" ~ 22, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 4-2" ~ 23,
                                               highestLevel_diff_Medium == "SpaceCakesLevel 4-3" ~ 24,  
                                               highestLevel_diff_Medium == "SpaceCakesLevel 4-4" ~ 25, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 5-0" ~ 26, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 5-1" ~ 27,
                                               highestLevel_diff_Medium == "SpaceCakesLevel 5-2" ~ 28,  
                                               highestLevel_diff_Medium == "SpaceCakesLevel 5-3" ~ 29,  
                                               highestLevel_diff_Medium == "SpaceCakesLevel 5-4" ~ 30,  
                                               highestLevel_diff_Medium == "SpaceCakesLevel 6-0" ~ 31, 
                                               highestLevel_diff_Medium == "SpaceCakesLevel 6-1" ~ 32,  
                                               highestLevel_diff_Medium == "SpaceCakesLevel 6-2" ~ 33),
         highestLevel_diff_Difficult = case_when( highestLevel_diff_Difficult == "SpaceCakesLevel 0-0" ~ 1, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 0-1" ~ 2, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 0-2" ~ 3,
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 0-3" ~ 4, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 0-4" ~ 5, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 1-0" ~ 6, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 1-1" ~ 7,
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 1-2" ~ 8, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 1-3" ~ 9, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 1-4" ~ 10, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 2-0" ~ 11,
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 2-1" ~ 12, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 2-2" ~ 13, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 2-3" ~ 14, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 2-4" ~ 15,
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 3-0" ~ 16, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 3-1" ~ 17, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 3-2" ~ 18, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 3-3" ~ 19, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 3-4" ~ 20, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 4-0" ~ 21, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 4-1" ~ 22, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 4-2" ~ 23,
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 4-3" ~ 24,  
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 4-4" ~ 25, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 5-0" ~ 26, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 5-1" ~ 27,
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 5-2" ~ 28,  
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 5-3" ~ 29,  
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 5-4" ~ 30,  
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 6-0" ~ 31, 
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 6-1" ~ 32,  
                                                  highestLevel_diff_Difficult == "SpaceCakesLevel 6-2" ~ 33),
         highestLevel_sess_1 = case_when( highestLevel_sess_1 == "SpaceCakesLevel 0-0" ~ 1, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 0-1" ~ 2, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 0-2" ~ 3,
                                          highestLevel_sess_1 == "SpaceCakesLevel 0-3" ~ 4, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 0-4" ~ 5, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 1-0" ~ 6, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 1-1" ~ 7,
                                          highestLevel_sess_1 == "SpaceCakesLevel 1-2" ~ 8, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 1-3" ~ 9, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 1-4" ~ 10, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 2-0" ~ 11,
                                          highestLevel_sess_1 == "SpaceCakesLevel 2-1" ~ 12, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 2-2" ~ 13, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 2-3" ~ 14, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 2-4" ~ 15,
                                          highestLevel_sess_1 == "SpaceCakesLevel 3-0" ~ 16, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 3-1" ~ 17, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 3-2" ~ 18, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 3-3" ~ 19, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 3-4" ~ 20, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 4-0" ~ 21, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 4-1" ~ 22, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 4-2" ~ 23,
                                          highestLevel_sess_1 == "SpaceCakesLevel 4-3" ~ 24,  
                                          highestLevel_sess_1 == "SpaceCakesLevel 4-4" ~ 25, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 5-0" ~ 26, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 5-1" ~ 27,
                                          highestLevel_sess_1 == "SpaceCakesLevel 5-2" ~ 28,  
                                          highestLevel_sess_1 == "SpaceCakesLevel 5-3" ~ 29,  
                                          highestLevel_sess_1 == "SpaceCakesLevel 5-4" ~ 30,  
                                          highestLevel_sess_1 == "SpaceCakesLevel 6-0" ~ 31, 
                                          highestLevel_sess_1 == "SpaceCakesLevel 6-1" ~ 32,  
                                          highestLevel_sess_1 == "SpaceCakesLevel 6-2" ~ 33),
         highestLevel_sess_2 = case_when( highestLevel_sess_2 == "SpaceCakesLevel 0-0" ~ 1, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 0-1" ~ 2, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 0-2" ~ 3,
                                          highestLevel_sess_2 == "SpaceCakesLevel 0-3" ~ 4, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 0-4" ~ 5, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 1-0" ~ 6, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 1-1" ~ 7,
                                          highestLevel_sess_2 == "SpaceCakesLevel 1-2" ~ 8, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 1-3" ~ 9, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 1-4" ~ 10, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 2-0" ~ 11,
                                          highestLevel_sess_2 == "SpaceCakesLevel 2-1" ~ 12, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 2-2" ~ 13, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 2-3" ~ 14, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 2-4" ~ 15,
                                          highestLevel_sess_2 == "SpaceCakesLevel 3-0" ~ 16, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 3-1" ~ 17, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 3-2" ~ 18, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 3-3" ~ 19, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 3-4" ~ 20, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 4-0" ~ 21, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 4-1" ~ 22, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 4-2" ~ 23,
                                          highestLevel_sess_2 == "SpaceCakesLevel 4-3" ~ 24,  
                                          highestLevel_sess_2 == "SpaceCakesLevel 4-4" ~ 25, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 5-0" ~ 26, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 5-1" ~ 27,
                                          highestLevel_sess_2 == "SpaceCakesLevel 5-2" ~ 28,  
                                          highestLevel_sess_2 == "SpaceCakesLevel 5-3" ~ 29,  
                                          highestLevel_sess_2 == "SpaceCakesLevel 5-4" ~ 30,  
                                          highestLevel_sess_2 == "SpaceCakesLevel 6-0" ~ 31, 
                                          highestLevel_sess_2 == "SpaceCakesLevel 6-1" ~ 32,  
                                          highestLevel_sess_2 == "SpaceCakesLevel 6-2" ~ 33),
         highestLevel_sess_3 = case_when( highestLevel_sess_3 == "SpaceCakesLevel 0-0" ~ 1, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 0-1" ~ 2, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 0-2" ~ 3,
                                          highestLevel_sess_3 == "SpaceCakesLevel 0-3" ~ 4, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 0-4" ~ 5, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 1-0" ~ 6, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 1-1" ~ 7,
                                          highestLevel_sess_3 == "SpaceCakesLevel 1-2" ~ 8, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 1-3" ~ 9, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 1-4" ~ 10, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 2-0" ~ 11,
                                          highestLevel_sess_3 == "SpaceCakesLevel 2-1" ~ 12, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 2-2" ~ 13, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 2-3" ~ 14, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 2-4" ~ 15,
                                          highestLevel_sess_3 == "SpaceCakesLevel 3-0" ~ 16, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 3-1" ~ 17, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 3-2" ~ 18, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 3-3" ~ 19, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 3-4" ~ 20, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 4-0" ~ 21, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 4-1" ~ 22, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 4-2" ~ 23,
                                          highestLevel_sess_3 == "SpaceCakesLevel 4-3" ~ 24,  
                                          highestLevel_sess_3 == "SpaceCakesLevel 4-4" ~ 25, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 5-0" ~ 26, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 5-1" ~ 27,
                                          highestLevel_sess_3 == "SpaceCakesLevel 5-2" ~ 28,  
                                          highestLevel_sess_3 == "SpaceCakesLevel 5-3" ~ 29,  
                                          highestLevel_sess_3 == "SpaceCakesLevel 5-4" ~ 30,  
                                          highestLevel_sess_3 == "SpaceCakesLevel 6-0" ~ 31, 
                                          highestLevel_sess_3 == "SpaceCakesLevel 6-1" ~ 32,  
                                          highestLevel_sess_3 == "SpaceCakesLevel 6-2" ~ 33),
         highestLevel_sess_4 = case_when( highestLevel_sess_4 == "SpaceCakesLevel 0-0" ~ 1, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 0-1" ~ 2, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 0-2" ~ 3,
                                          highestLevel_sess_4 == "SpaceCakesLevel 0-3" ~ 4, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 0-4" ~ 5, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 1-0" ~ 6, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 1-1" ~ 7,
                                          highestLevel_sess_4 == "SpaceCakesLevel 1-2" ~ 8, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 1-3" ~ 9, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 1-4" ~ 10, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 2-0" ~ 11,
                                          highestLevel_sess_4 == "SpaceCakesLevel 2-1" ~ 12, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 2-2" ~ 13, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 2-3" ~ 14, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 2-4" ~ 15,
                                          highestLevel_sess_4 == "SpaceCakesLevel 3-0" ~ 16, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 3-1" ~ 17, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 3-2" ~ 18, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 3-3" ~ 19, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 3-4" ~ 20, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 4-0" ~ 21, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 4-1" ~ 22, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 4-2" ~ 23,
                                          highestLevel_sess_4 == "SpaceCakesLevel 4-3" ~ 24,  
                                          highestLevel_sess_4 == "SpaceCakesLevel 4-4" ~ 25, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 5-0" ~ 26, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 5-1" ~ 27,
                                          highestLevel_sess_4 == "SpaceCakesLevel 5-2" ~ 28,  
                                          highestLevel_sess_4 == "SpaceCakesLevel 5-3" ~ 29,  
                                          highestLevel_sess_4 == "SpaceCakesLevel 5-4" ~ 30,  
                                          highestLevel_sess_4 == "SpaceCakesLevel 6-0" ~ 31, 
                                          highestLevel_sess_4 == "SpaceCakesLevel 6-1" ~ 32,  
                                          highestLevel_sess_4 == "SpaceCakesLevel 6-2" ~ 33),
         #Recode T/F as 1/0 for modeling
        ImproverScore = case_when(ImproverScore == TRUE ~ 1, 
                                  ImproverScore == FALSE ~ 0)) %>%
        #replace NAs with 0
        replace_na(set_names(as.list(rep(0, length(.))), names(.)))

#colnames(AYCET_gameplay_aggregated)
#colnames(AYCET_DCCS)

###########################
#LASSO: 'Improver' Based on change in NIH Score; outcome of interest is ImproverScore

#split to test train
#Remove all outcomes
    LassoNIHScore_x <- model.matrix( ~ ., AYCET_DCCS %>% select(-ImproverScore, -ImproverAccuracy, -ImproverRT))
    LassoNIHScore_y <-AYCET_DCCS$ImproverScore
    set.seed(1234)
    
    #TRAIN DATA SET
    train = AYCET_DCCS %>%
      sample_frac(0.6)
    
    #TEST DATA SET
    test = AYCET_DCCS %>%
      setdiff(train)
    
    #TRAIN x and y    
    x_train = model.matrix(~., train%>%select(-ImproverScore))
    y_train = train$ImproverScore
    
    #TEST x and y 
    x_test = model.matrix(~., test%>%select(-ImproverScore))
    y_test <- test$ImproverScore
    
    grid = 10^seq(10, -2, length = 100)
    cv.out = cv.glmnet(x_train, y_train, alpha = 1, family = 'binomial', intercept=FALSE) # Fit lasso model on training data
    plot(cv.out) 
    # Select lamda that minimizes training binomial deviance
    bestlam = cv.out$lambda.min 
    lasso_pred = predict(cv.out, s = bestlam, newx = x_test, type = 'response') # Use best lambda to predict test data
    pred_lasso <- prediction(lasso_pred, y_test) #Conduct predictions
    perf.lasso <- performance(pred_lasso,'auc') #Evaluate performance
    cat(perf.lasso@y.values[[1]]) #Obtain AUC value
    
    out = glmnet(LassoNIHScore_x, LassoNIHScore_y, alpha = 1, lambda = grid,intercept=FALSE) # Fit lasso model on full dataset
    lasso_coef = predict(out, type = "coefficients", s = bestlam) # Display coefficients using lambda chosen by CV
#lasso_coef
#str(lasso_coef)
###Bar graph of coefficients
  #Extract coefficient data & change variable type for merge
  lasso_coef_df <- as.data.frame(summary(lasso_coef))
  lasso_coef_df$i <- as.character(lasso_coef_df$i)
  
  lasso_coef_feature_names <- as.data.frame(cbind(seq(1:length(lasso_coef@Dimnames[[1]])), lasso_coef@Dimnames[[1]]))
  colnames(lasso_coef_feature_names) <- c("i", "Variable")
  
  #lasso_coef_df <- as.data.frame(lasso_coef@Dimnames[[1]], lasso_coef@x)
  lasso_coef_df <-  left_join(lasso_coef_df, lasso_coef_feature_names, by = "i") %>%
    arrange(desc(abs(x))) %>%
    rename(Coefficient = x)%>%
    slice(1:10)

  #Make Variable an ordered factor so it will be in order for ggplot
      lasso_coef_df$Variable <- factor(lasso_coef_df$Variable, levels = lasso_coef_df$Variable[order( abs(lasso_coef_df$Coefficient))])

  ImproverScoreFeatureGraph <- ggplot(data = lasso_coef_df, aes(x = Variable, y = Coefficient, fill = Coefficient)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "#0b200e", high = "#37a146") +
    coord_flip() +
    labs(title = "+Change in NIH Score")

  
###########################
  #LASSO2: 'Improver' Based on change in Accuracy; outcome of interest is ImproverAccuracy
  
  #split to test train
  #Remove all outcomes
  Lasso_Acc_x <- model.matrix( ~ ., AYCET_DCCS %>% select(-ImproverScore, -ImproverAccuracy, -ImproverRT))
  Lasso_Acc_y <-AYCET_DCCS$ImproverAccuracy
  
  #TRAIN x and y    
  x_train_Acc = model.matrix(~., train%>%select(-ImproverAccuracy))
  y_train_Acc = train$ImproverAccuracy
  
  #TEST x and y 
  x_test_Acc = model.matrix(~., test%>%select(-ImproverAccuracy))
  y_test_Acc <- test$ImproverAccuracy
  
  cv.out_Acc = cv.glmnet(x_train_Acc, y_train_Acc, alpha = 1, family = 'binomial', intercept=FALSE) # Fit lasso model on training data
  plot(cv.out_Acc) 
  bestlam_Acc = cv.out_Acc$lambda.min # Select lamda that minimizes training binomial deviance
  #commented lines throw error. sure doing something stupid here
  lasso_pred_Acc = predict(cv.out_Acc, s = bestlam_Acc, newx = x_test_Acc, type = 'response') # Use best lambda to predict test data
  pred_lasso_Acc <- prediction(lasso_pred_Acc, y_test_Acc)
  perf.lasso_Acc <- performance(pred_lasso_Acc,'auc')
  cat(perf.lasso_Acc@y.values[[1]])
  
  out_Acc = glmnet(Lasso_Acc_x, Lasso_Acc_y, alpha = 1, lambda = grid,intercept=FALSE) # Fit lasso model on full dataset
  lasso_coef_Acc = predict(out_Acc, type = "coefficients", s = bestlam_Acc) # Display coefficients using lambda chosen by CV
  lasso_coef_Acc
  str(lasso_coef_Acc)
  ###Bar graph of coefficients
  #Extract coefficient data & change variable type for merge
  lasso_coef_Acc_df <- as.data.frame(summary(lasso_coef_Acc))
  lasso_coef_Acc_df$i <- as.character(lasso_coef_Acc_df$i)
  
  lasso_coef_Acc_feature_names <- as.data.frame(cbind(seq(1:length(lasso_coef_Acc@Dimnames[[1]])), lasso_coef_Acc@Dimnames[[1]]))
  colnames(lasso_coef_Acc_feature_names) <- c("i", "Variable")
  
  #lasso_coef_df <- as.data.frame(lasso_coef@Dimnames[[1]], lasso_coef@x)
  lasso_coef_Acc_df <-  left_join(lasso_coef_Acc_df, lasso_coef_Acc_feature_names, by = "i") %>%
    arrange(desc(abs(x))) %>%
    rename(Coefficient = x)%>%
    slice(1:10)
  
  #Make Variable an ordered factor so it will be in order for ggplot
  lasso_coef_Acc_df$Variable <- factor(lasso_coef_Acc_df$Variable, levels = lasso_coef_Acc_df$Variable[order( abs(lasso_coef_Acc_df$Coefficient))])
  
  ImproverAccuracyFeatureGraph <- ggplot(data = lasso_coef_Acc_df, aes(x = Variable, y = Coefficient, fill = Coefficient)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "#0b200e", high = "#37a146") +
    coord_flip() +
    labs(title = "Posttest Accuracy Score = 5")
  
  ###################
  
  #LASSO3: 'Improver' Based on change in Reaction Time; outcome of interest is ImproverRT
  
  #split to test train
  #Remove all outcomes
  Lasso_RT_x <- model.matrix( ~ ., AYCET_DCCS %>% select(-ImproverScore, -ImproverAccuracy, -ImproverRT))
  Lasso_RT_y <-AYCET_DCCS$ImproverRT
  
  #TRAIN x and y    
  x_train_RT = model.matrix(~., train%>%select(-ImproverRT))
  y_train_RT = train$ImproverRT
  
  #TEST x and y 
  x_test_RT = model.matrix(~., test%>%select(-ImproverRT))
  y_test_RT <- test$ImproverRT
  
  cv.out_RT = cv.glmnet(x_train_RT, y_train_RT, alpha = 1, family = 'binomial', intercept=FALSE) # Fit lasso model on training data
  plot(cv.out_RT) 
  bestlam_RT = cv.out_RT$lambda.min # Select lamda that minimizes training binomial deviance
  #commented lines throw error. sure doing something stupid here
  lasso_pred_RT = predict(cv.out_RT, s = bestlam_RT, newx = x_test_RT, type = 'response') # Use best lambda to predict test data
  pred_lasso_RT <- prediction(lasso_pred_RT, y_test_RT)
  perf.lasso_RT <- performance(pred_lasso_RT,'auc')
  cat(perf.lasso_RT@y.values[[1]])
  
  out_RT = glmnet(Lasso_RT_x, Lasso_RT_y, alpha = 1, lambda = grid,intercept=FALSE) # Fit lasso model on full dataset
  lasso_coef_RT = predict(out_RT, type = "coefficients", s = bestlam_RT) # Display coefficients using lambda chosen by CV
  lasso_coef_RT
  str(lasso_coef_RT)
  ###Bar graph of coefficients
  #Extract coefficient data & change variable type for merge
  lasso_coef_RT_df <- as.data.frame(summary(lasso_coef_RT))
  lasso_coef_RT_df$i <- as.character(lasso_coef_RT_df$i)
  
  lasso_coef_RT_feature_names <- as.data.frame(cbind(seq(1:length(lasso_coef_RT@Dimnames[[1]])), lasso_coef_RT@Dimnames[[1]]))
  colnames(lasso_coef_RT_feature_names) <- c("i", "Variable")
  
  #lasso_coef_df <- as.data.frame(lasso_coef@Dimnames[[1]], lasso_coef@x)
  lasso_coef_RT_df <-  left_join(lasso_coef_RT_df, lasso_coef_RT_feature_names, by = "i") %>%
    arrange(desc(abs(x))) %>%
    rename(Coefficient = x)%>%
    slice(1:10)
  
  #Make Variable an ordered factor so it will be in order for ggplot
  lasso_coef_RT_df$Variable <- factor(lasso_coef_RT_df$Variable, levels = lasso_coef_RT_df$Variable[order( abs(lasso_coef_RT_df$Coefficient))])
  
  ImproverRTimeFeatureGraph <- ggplot(data = lasso_coef_RT_df, aes(x = Variable, y = Coefficient, fill = Coefficient)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "#0b200e", high = "#37a146") +
    coord_flip() +
    labs(title = "Posttest RT Score > 2.7")


#######################################
  
  #Combine graphs
  
  coefficient_graph <- grid.arrange(ImproverScoreFeatureGraph, ImproverAccuracyFeatureGraph, ImproverRTimeFeatureGraph, ncol = 3,
               top = textGrob("Model Coefficients for Three Outcomes", gp=gpar(fontsize=20)))
ggsave("images/coefficient_graph.png", coefficient_graph, width = 15, height = 5, units = "in")


#######################################

#Apply lasso variables to normal regression
#Outcome based on improvement
lasso_coef_df

NIHimprovement_lm <- lm(data = test, ImproverScore ~ FARate_sess_1 + Hit_AvgRT_sess_1 + avgRT_afterWrong_WRONG +
                          Accuracy_Pct_diff_Medium + count_afterWrong_NA + Wrong_AvgRT_sess_1 + percentCorrect_user +
                          DPrime_diff_Difficult + highestTrialCount_sess_3 + highestTrialCount_sess_2)
summary(NIHimprovement_lm)
#Outcome based on post test accuract criteria
lasso_coef_Acc_df

Acc_lm <- lm(data = test, ImproverAccuracy ~ accuracy_afterWrong + percentCorrect_user + HitRate_user)
summary(Acc_lm)


#Outcome based on post test RT criteria
lasso_coef_RT_df

RT_lm <- lm(data = test, ImproverRT ~ Hit_AvgRT_sess_3 + Wrong_SDRT_user + Accuracy_Pct_diff_Medium + 
              DPrime_sess_2 + Wrong_SDRT_sess_4 + Hit_AvgRT_sess_1 + highestLevel_diff_Easy + percentCorrect_user +
              fastestRT_sess_4 + nLevels_user)
summary(RT_lm)

