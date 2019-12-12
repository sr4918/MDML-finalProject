require(tidyverse)
library(readr)
require(ROCR)
require(ranger)
require(glmnet)
require(dplyr)
require(ggplot2)


#For Lasso regression 

#Read in AYCET gameplay data and DCCS data
AYCET_gameplay_aggregated <- read_csv("data/AYCET_gameplay_aggregated.csv") %>%mutate(userID = factor(userID))
#Read in AYCET gameplay data and DCCS data; change userID to factor for merge

ALL_DCCS_data <- read_csv("data/ALL_DCCS_data.csv") %>%
  mutate(userID = factor(userID)) %>%
  select(-c("dateTime.x", "dateTime.y"))

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

#remove variables with many NAs; change variable types; convert ordered factors to numbers to impute missing data
AYCET_DCCS <- AYCET_DCCS %>%
  #Drop columns that are not data for model or missing too much data
  select(-matches("_sess_6|_sess_5")) %>%
  select(-c("accessCode", "userID")) %>%
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

#NAs_per_col <- colSums(is.na(AYCET_DCCS))
# need to replace all NA's with 0 (absent users)
#AYCET_DCCS<-AYCET_DCCS%>% 
#  replace_na(set_names(as.list(rep(0, length(.))), names(.)))

#Define & Add outcomes for DCCS
  #Already calculated - improvement (based on change in DCCS NIH Score), DCCS$ImproverScore
  #New: Above the median, Accuracy of 5 vs below five
  #New: Above the median, RT Score of 2.7 vs below 2.7

#test and train data using 5 fold cross validation
#precision and accuracy graphs

###########################
#LASSO: 'Improver' Based on change in NIH Score; outcome of interest is ImproverScore

#split to test train
#Remove all outcomes
LassoNIHScore_x <- model.matrix( ~ ., AYCET_DCCS %>% select(-ImproverScore, -ImproverAccuracy, -ImproverRT, -ImprovedPostScoreGT7, -AllImprove))
LassoNIHScore_y <-AYCET_DCCS$ImproverScore

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
plot(cv.out) # Draw plot of training MSE as a function of lambda
bestlam = cv.out$lambda.min # Select lamda that minimizes training binomial deviance
lasso_pred = predict(cv.out, s = bestlam, newx = x_test, type = 'response') # Use best lambda to predict test data
pred_lasso <- prediction(lasso_pred, y_test)
perf.lasso <- performance(pred_lasso,'auc')
cat(perf.lasso@y.values[[1]])

out = glmnet(LassoNIHScore_x, LassoNIHScore_y, alpha = 1, lambda = grid,intercept=FALSE) # Fit lasso model on full dataset
lasso_coef = predict(out, type = "coefficients", s = bestlam) # Display coefficients using lambda chosen by CV
lasso_coef
str(lasso_coef)
###Bar graph of coefficients
  #Extract coefficient data & change variable type for merge
  lasso_coef_df <- as.data.frame(summary(lasso_coef))
  lasso_coef_df$i <- as.character(lasso_coef_df$i)
  
  lasso_coef_feature_names <- as.data.frame(cbind(seq(1:length(lasso_coef@Dimnames[[1]])), lasso_coef@Dimnames[[1]]))
  colnames(lasso_coef_feature_names) <- c("i", "Variable")
  
  #lasso_coef_df <- as.data.frame(lasso_coef@Dimnames[[1]], lasso_coef@x)
  lasso_coef_df <-  left_join(lasso_coef_df, lasso_coef_feature_names, by = "i") %>%
    arrange(desc(abs(x))) %>%
    rename(Coefficient = x)

  #Make Variable an ordered factor so it will be in order for ggplot
      lasso_coef_df$Variable <- factor(lasso_coef_df$Variable, levels = lasso_coef_df$Variable[order( abs(lasso_coef_df$Coefficient))])

  ImproverScoreFeatureGraph <- ggplot(data = lasso_coef_df, aes(x = Variable, y = Coefficient, fill = Coefficient)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "#0b200e", high = "#37a146") +
    coord_flip() +
    labs(title = "Model Coefficients for Outcome as Change in NIH Score")

  
###########################
  #LASSO2: 'Improver' Based on change in Accuracy; outcome of interest is ImproverAccuracy
  
  #split to test train
  #Remove all outcomes
  LassoAcc_x <- model.matrix( ~ ., AYCET_DCCS %>% select(-ImproverScore, -ImproverAccuracy, -ImproverRT, -ImprovedPostScoreGT7, -AllImprove))
  LassoAcc_y <-AYCET_DCCS$ImproverAccuracy
  
  #TRAIN DATA SET
  #train = AYCET_DCCS %>%
  #  sample_frac(0.6)
  
  #TEST DATA SET
  #test = AYCET_DCCS %>%
  #  setdiff(train)
  
  #TRAIN x and y    
  x_train_Acc = model.matrix(~., train%>%select(-ImproverAccuracy))
  y_train_Acc = train$ImproverAccuracy
  
  #TEST x and y 
  x_test_Acc = model.matrix(~., test%>%select(-ImproverAccuracy))
  y_test_Acc <- test$ImproverAccuracy
  
  #grid = 10^seq(10, -2, length = 100)
  cv.out_Acc = cv.glmnet(x_train_Acc, y_train_Acc, alpha = 1, family = 'binomial', intercept=FALSE) # Fit lasso model on training data
  plot(cv.out_Acc) 
  bestlam_Acc = cv.out_Acc$lambda.min # Select lamda that minimizes training binomial deviance
  #commented lines throw error. sure doing something stupid here
  #lasso_pred_Acc = predict(cv.out_Acc, s = bestlam_Acc, newx = x_test_Acc, type = 'response') # Use best lambda to predict test data
  #pred_lasso_Acc <- prediction(lasso_pred_Acc, y_test_Acc)
  #perf.lasso_Acc <- performance(pred_lasso_Acc,'auc')
  #cat(perf.lasso_Acc@y.values[[1]])
  
  out_Acc = glmnet(LassoNIHScore_x, LassoNIHScore_y, alpha = 1, lambda = grid,intercept=FALSE) # Fit lasso model on full dataset
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
    rename(Coefficient = x)
  
  #Make Variable an ordered factor so it will be in order for ggplot
  lasso_coef_Acc_df$Variable <- factor(lasso_coef_Acc_df$Variable, levels = lasso_coef_Acc_df$Variable[order( abs(lasso_coef_Acc_df$Coefficient))])
  
  ImproverAccuracyFeatureGraph <- ggplot(data = lasso_coef_Acc_df, aes(x = Variable, y = Coefficient, fill = Coefficient)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "#0b200e", high = "#37a146") +
    coord_flip() +
    labs(title = "Model Coefficients for Outcome as Change in Accuracy")
  
  
  ###################
  
  #LASSO3: 'Improver' Based on change in Reaction Time; outcome of interest is ImproverRT
  
  #split to test train
  #Remove all outcomes
  LassoNIHScore_x <- model.matrix( ~ ., AYCET_DCCS %>% select(-ImproverScore, -ImproverAccuracy, -ImproverRT, -ImprovedPostScoreGT7))
  LassoNIHScore_y <-AYCET_DCCS$ImproverRT
  
  #TRAIN DATA SET
  train = AYCET_DCCS %>%
    sample_frac(0.6)
  
  #TEST DATA SET
  test = AYCET_DCCS %>%
    setdiff(train)
  
  #TRAIN x and y    
  x_train_RT = model.matrix(~., train%>%select(-ImproverRT))
  y_train_RT = train$ImproverRT
  
  #TEST x and y 
  x_test_RT = model.matrix(~., test%>%select(-ImproverRT))
  y_test_RT <- test$ImproverRT
  
  grid = 10^seq(10, -2, length = 100)
  cv.out_RT = cv.glmnet(x_train_RT, y_train_RT, alpha = 1, family = 'binomial', intercept=FALSE) # Fit lasso model on training data
  plot(cv.out_RT) 
  bestlam_RT = cv.out_RT$lambda.min # Select lamda that minimizes training binomial deviance
  #commented lines throw error. sure doing something stupid here
  #lasso_pred_RT = predict(cv.out_RT, s = bestlam_RT, newx = x_test_RT, type = 'response') # Use best lambda to predict test data
  #pred_lasso_RT <- prediction(lasso_pred_RT, y_test_RT)
  #perf.lasso_RT <- performance(pred_lasso_RT,'auc')
  #cat(perf.lasso_RT@y.values[[1]])
  
  out_RT = glmnet(LassoNIHScore_x, LassoNIHScore_y, alpha = 1, lambda = grid,intercept=FALSE) # Fit lasso model on full dataset
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
    rename(Coefficient = x)
  
  #Make Variable an ordered factor so it will be in order for ggplot
  lasso_coef_RT_df$Variable <- factor(lasso_coef_RT_df$Variable, levels = lasso_coef_RT_df$Variable[order( abs(lasso_coef_RT_df$Coefficient))])
  
  ImproverRTimeFeatureGraph <- ggplot(data = lasso_coef_RT_df, aes(x = Variable, y = Coefficient, fill = Coefficient)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "#0b200e", high = "#37a146") +
    coord_flip() +
    labs(title = "Model Coefficients for Outcome as Change in Reaction Time")
  
  
  


#######################################
#Is everything below just scratch material?
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
  
#Need to remove variables with many NAs, 
#or replace NAs with 0 for all of the AYCET data.

NAs_per_col <- data.frame(matrix(ncol = 2, nrow = 0))
NAs_per_col <- colSums(is.na(AYCET_DCCS))
#all _see_6 columns have a large number of NA's >140 dropping these; DONE in like 25
grep("_sess_6",colnames(AYCET_DCCS))

names(NAs_per_col)<-c("totalNA")
NAs_per_col<-NAs_per_col%>%arrange(-totalNA)
LassoNIHScore <- model.matrix(ImproverScore ~ ., AYCET_DCCS)[,-1]

#Lasso 1: Which game play features are associated with improvement in DCCS NIH Score?
ImproverScore

#Lasso 2: Which game play features are associated with high accuracy in the DCCS Accuracy outcome?

#Lasso 3: Which game play features are associated with low reaction times in DCCS (high scores = fast, correct responses)?
