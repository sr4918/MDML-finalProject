require(tidyverse)
library(readr)
require(ROCR)
require(ranger)
require(glmnet)
require(dplyr)


#For Lasso regression 
#Read in AYCET gameplay data and DCCS data; change userID to factor for merge
AYCET_gameplay_aggregated <- read_csv("data/AYCET_gameplay_aggregated.csv") 
ALL_DCCS_data <- read_csv("data/ALL_DCCS_data.csv") %>%
  mutate(userID = factor(userID))

#Merge by userID so each row represents one participant
AYCET_DCCS <- left_join(AYCET_gameplay_aggregated, ALL_DCCS_data, by = c("userID"))

#check that each row is a unique participant
count <- AYCET_DCCS %>%
  group_by(userID) %>%
  summarize(n = n()) %>%
  filter(n > 1)

#remove variables with many NAs; change variable types; convert ordered factors to numbers to impute missing data
AYCET_DCCS <- AYCET_DCCS %>%
  select(-c(grep("_sess_6",colnames(AYCET_DCCS)))) %>%
  select(-c("accessCode", "userID")) %>%
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
         highestLevel_sess_5 = case_when( highestLevel_sess_5 == "SpaceCakesLevel 0-0" ~ 1, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 0-1" ~ 2, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 0-2" ~ 3,
                                          highestLevel_sess_5 == "SpaceCakesLevel 0-3" ~ 4, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 0-4" ~ 5, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 1-0" ~ 6, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 1-1" ~ 7,
                                          highestLevel_sess_5 == "SpaceCakesLevel 1-2" ~ 8, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 1-3" ~ 9, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 1-4" ~ 10, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 2-0" ~ 11,
                                          highestLevel_sess_5 == "SpaceCakesLevel 2-1" ~ 12, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 2-2" ~ 13, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 2-3" ~ 14, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 2-4" ~ 15,
                                          highestLevel_sess_5 == "SpaceCakesLevel 3-0" ~ 16, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 3-1" ~ 17, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 3-2" ~ 18, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 3-3" ~ 19, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 3-4" ~ 20, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 4-0" ~ 21, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 4-1" ~ 22, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 4-2" ~ 23,
                                          highestLevel_sess_5 == "SpaceCakesLevel 4-3" ~ 24,  
                                          highestLevel_sess_5 == "SpaceCakesLevel 4-4" ~ 25, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 5-0" ~ 26, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 5-1" ~ 27,
                                          highestLevel_sess_5 == "SpaceCakesLevel 5-2" ~ 28,  
                                          highestLevel_sess_5 == "SpaceCakesLevel 5-3" ~ 29,  
                                          highestLevel_sess_5 == "SpaceCakesLevel 5-4" ~ 30,  
                                          highestLevel_sess_5 == "SpaceCakesLevel 6-0" ~ 31, 
                                          highestLevel_sess_5 == "SpaceCakesLevel 6-1" ~ 32,  
                                          highestLevel_sess_5 == "SpaceCakesLevel 6-2" ~ 33),
        ImproverScore = case_when(ImproverScore == TRUE ~ 1, 
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
