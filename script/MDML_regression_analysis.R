require(tidyverse)
library(readr)
require(ROCR)
require(ranger)
require(glmnet)
require(dplyr)


#For Lasso regression 
#Read in AYCET gameplay data and DCCS data
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
table(AYCET_DCCS$highestLevel_user)
#remove variables with many NAs; change variable types
AYCET_DCCS <- AYCET_DCCS %>%
  select(-c(grep("_sess_6",colnames(AYCET_DCCS))))%>%
  select(-c("accessCode", "userID")) %>%
  mutate(highestLevel_user = factor(highestLevel_user, levels = c( "SpaceCakesLevel 0-0", "SpaceCakesLevel 0-1", "SpaceCakesLevel 0-2",
                                                          "SpaceCakesLevel 0-3", "SpaceCakesLevel 0-4", "SpaceCakesLevel 1-0", "SpaceCakesLevel 1-1",
                                                          "SpaceCakesLevel 1-2", "SpaceCakesLevel 1-3", "SpaceCakesLevel 1-4", "SpaceCakesLevel 2-0",
                                                          "SpaceCakesLevel 2-1", "SpaceCakesLevel 2-2", "SpaceCakesLevel 2-3", "SpaceCakesLevel 2-4",
                                                          "SpaceCakesLevel 3-0", "SpaceCakesLevel 3-1", "SpaceCakesLevel 3-2", "SpaceCakesLevel 3-3", 
                                                          "SpaceCakesLevel 3-4", "SpaceCakesLevel 4-0", "SpaceCakesLevel 4-1", "SpaceCakesLevel 4-2",
                                                           "SpaceCakesLevel 4-3",  "SpaceCakesLevel 4-4", "SpaceCakesLevel 5-0", "SpaceCakesLevel 5-1",
                                                          "SpaceCakesLevel 5-2",  "SpaceCakesLevel 5-3",  "SpaceCakesLevel 5-4",  "SpaceCakesLevel 6-0", 
                                                           "SpaceCakesLevel 6-1",  "SpaceCakesLevel 6-2"), ordered = TRUE), #need to give the order because numbers read as characters are in the wrong order
         highestLevel_diff_Easy = factor(highestLevel_diff_Easy, levels = c( "SpaceCakesLevel 0-0", "SpaceCakesLevel 0-1", "SpaceCakesLevel 0-2",
                                                                             "SpaceCakesLevel 0-3", "SpaceCakesLevel 0-4", "SpaceCakesLevel 1-0", "SpaceCakesLevel 1-1",
                                                                             "SpaceCakesLevel 1-2", "SpaceCakesLevel 1-3", "SpaceCakesLevel 1-4", "SpaceCakesLevel 2-0",
                                                                             "SpaceCakesLevel 2-1", "SpaceCakesLevel 2-2", "SpaceCakesLevel 2-3", "SpaceCakesLevel 2-4",
                                                                             "SpaceCakesLevel 3-0", "SpaceCakesLevel 3-1", "SpaceCakesLevel 3-2", "SpaceCakesLevel 3-3", 
                                                                             "SpaceCakesLevel 3-4", "SpaceCakesLevel 4-0", "SpaceCakesLevel 4-1", "SpaceCakesLevel 4-2",
                                                                             "SpaceCakesLevel 4-3",  "SpaceCakesLevel 4-4", "SpaceCakesLevel 5-0", "SpaceCakesLevel 5-1",
                                                                             "SpaceCakesLevel 5-2",  "SpaceCakesLevel 5-3",  "SpaceCakesLevel 5-4",  "SpaceCakesLevel 6-0", 
                                                                             "SpaceCakesLevel 6-1",  "SpaceCakesLevel 6-2"), ordered = TRUE), #need to give the order because numbers read as characters are in the wrong order
         highestLevel_diff_Medium = factor(highestLevel_diff_Medium, levels = c( "SpaceCakesLevel 0-0", "SpaceCakesLevel 0-1", "SpaceCakesLevel 0-2",
                                                                                 "SpaceCakesLevel 0-3", "SpaceCakesLevel 0-4", "SpaceCakesLevel 1-0", "SpaceCakesLevel 1-1",
                                                                                 "SpaceCakesLevel 1-2", "SpaceCakesLevel 1-3", "SpaceCakesLevel 1-4", "SpaceCakesLevel 2-0",
                                                                                 "SpaceCakesLevel 2-1", "SpaceCakesLevel 2-2", "SpaceCakesLevel 2-3", "SpaceCakesLevel 2-4",
                                                                                 "SpaceCakesLevel 3-0", "SpaceCakesLevel 3-1", "SpaceCakesLevel 3-2", "SpaceCakesLevel 3-3", 
                                                                                 "SpaceCakesLevel 3-4", "SpaceCakesLevel 4-0", "SpaceCakesLevel 4-1", "SpaceCakesLevel 4-2",
                                                                                 "SpaceCakesLevel 4-3",  "SpaceCakesLevel 4-4", "SpaceCakesLevel 5-0", "SpaceCakesLevel 5-1",
                                                                                 "SpaceCakesLevel 5-2",  "SpaceCakesLevel 5-3",  "SpaceCakesLevel 5-4",  "SpaceCakesLevel 6-0", 
                                                                                 "SpaceCakesLevel 6-1",  "SpaceCakesLevel 6-2"), ordered = TRUE), #need to give the order because numbers read as characters are in the wrong order
         highestLevel_diff_Difficult = factor(highestLevel_diff_Difficult, levels = c( "SpaceCakesLevel 0-0", "SpaceCakesLevel 0-1", "SpaceCakesLevel 0-2",
                                                                                       "SpaceCakesLevel 0-3", "SpaceCakesLevel 0-4", "SpaceCakesLevel 1-0", "SpaceCakesLevel 1-1",
                                                                                       "SpaceCakesLevel 1-2", "SpaceCakesLevel 1-3", "SpaceCakesLevel 1-4", "SpaceCakesLevel 2-0",
                                                                                       "SpaceCakesLevel 2-1", "SpaceCakesLevel 2-2", "SpaceCakesLevel 2-3", "SpaceCakesLevel 2-4",
                                                                                       "SpaceCakesLevel 3-0", "SpaceCakesLevel 3-1", "SpaceCakesLevel 3-2", "SpaceCakesLevel 3-3", 
                                                                                       "SpaceCakesLevel 3-4", "SpaceCakesLevel 4-0", "SpaceCakesLevel 4-1", "SpaceCakesLevel 4-2",
                                                                                       "SpaceCakesLevel 4-3",  "SpaceCakesLevel 4-4", "SpaceCakesLevel 5-0", "SpaceCakesLevel 5-1",
                                                                                       "SpaceCakesLevel 5-2",  "SpaceCakesLevel 5-3",  "SpaceCakesLevel 5-4",  "SpaceCakesLevel 6-0", 
                                                                                       "SpaceCakesLevel 6-1",  "SpaceCakesLevel 6-2"), ordered = TRUE), #need to give the order because numbers read as characters are in the wrong order
         highestLevel_sess_1 = factor(highestLevel_sess_1, levels = c( "SpaceCakesLevel 0-0", "SpaceCakesLevel 0-1", "SpaceCakesLevel 0-2",
                                                                       "SpaceCakesLevel 0-3", "SpaceCakesLevel 0-4", "SpaceCakesLevel 1-0", "SpaceCakesLevel 1-1",
                                                                       "SpaceCakesLevel 1-2", "SpaceCakesLevel 1-3", "SpaceCakesLevel 1-4", "SpaceCakesLevel 2-0",
                                                                       "SpaceCakesLevel 2-1", "SpaceCakesLevel 2-2", "SpaceCakesLevel 2-3", "SpaceCakesLevel 2-4",
                                                                       "SpaceCakesLevel 3-0", "SpaceCakesLevel 3-1", "SpaceCakesLevel 3-2", "SpaceCakesLevel 3-3", 
                                                                       "SpaceCakesLevel 3-4", "SpaceCakesLevel 4-0", "SpaceCakesLevel 4-1", "SpaceCakesLevel 4-2",
                                                                       "SpaceCakesLevel 4-3",  "SpaceCakesLevel 4-4", "SpaceCakesLevel 5-0", "SpaceCakesLevel 5-1",
                                                                       "SpaceCakesLevel 5-2",  "SpaceCakesLevel 5-3",  "SpaceCakesLevel 5-4",  "SpaceCakesLevel 6-0", 
                                                                       "SpaceCakesLevel 6-1",  "SpaceCakesLevel 6-2"), ordered = TRUE), #need to give the order because numbers read as characters are in the wrong order
         highestLevel_sess_2 = factor(highestLevel_sess_2, levels = c( "SpaceCakesLevel 0-0", "SpaceCakesLevel 0-1", "SpaceCakesLevel 0-2",
                                                                       "SpaceCakesLevel 0-3", "SpaceCakesLevel 0-4", "SpaceCakesLevel 1-0", "SpaceCakesLevel 1-1",
                                                                       "SpaceCakesLevel 1-2", "SpaceCakesLevel 1-3", "SpaceCakesLevel 1-4", "SpaceCakesLevel 2-0",
                                                                       "SpaceCakesLevel 2-1", "SpaceCakesLevel 2-2", "SpaceCakesLevel 2-3", "SpaceCakesLevel 2-4",
                                                                       "SpaceCakesLevel 3-0", "SpaceCakesLevel 3-1", "SpaceCakesLevel 3-2", "SpaceCakesLevel 3-3", 
                                                                       "SpaceCakesLevel 3-4", "SpaceCakesLevel 4-0", "SpaceCakesLevel 4-1", "SpaceCakesLevel 4-2",
                                                                       "SpaceCakesLevel 4-3",  "SpaceCakesLevel 4-4", "SpaceCakesLevel 5-0", "SpaceCakesLevel 5-1",
                                                                       "SpaceCakesLevel 5-2",  "SpaceCakesLevel 5-3",  "SpaceCakesLevel 5-4",  "SpaceCakesLevel 6-0", 
                                                                       "SpaceCakesLevel 6-1",  "SpaceCakesLevel 6-2"), ordered = TRUE), #need to give the order because numbers read as characters are in the wrong order
         highestLevel_sess_3 = factor(highestLevel_sess_3, levels = c( "SpaceCakesLevel 0-0", "SpaceCakesLevel 0-1", "SpaceCakesLevel 0-2",
                                                                       "SpaceCakesLevel 0-3", "SpaceCakesLevel 0-4", "SpaceCakesLevel 1-0", "SpaceCakesLevel 1-1",
                                                                       "SpaceCakesLevel 1-2", "SpaceCakesLevel 1-3", "SpaceCakesLevel 1-4", "SpaceCakesLevel 2-0",
                                                                       "SpaceCakesLevel 2-1", "SpaceCakesLevel 2-2", "SpaceCakesLevel 2-3", "SpaceCakesLevel 2-4",
                                                                       "SpaceCakesLevel 3-0", "SpaceCakesLevel 3-1", "SpaceCakesLevel 3-2", "SpaceCakesLevel 3-3", 
                                                                       "SpaceCakesLevel 3-4", "SpaceCakesLevel 4-0", "SpaceCakesLevel 4-1", "SpaceCakesLevel 4-2",
                                                                       "SpaceCakesLevel 4-3",  "SpaceCakesLevel 4-4", "SpaceCakesLevel 5-0", "SpaceCakesLevel 5-1",
                                                                       "SpaceCakesLevel 5-2",  "SpaceCakesLevel 5-3",  "SpaceCakesLevel 5-4",  "SpaceCakesLevel 6-0", 
                                                                       "SpaceCakesLevel 6-1",  "SpaceCakesLevel 6-2"), ordered = TRUE), #need to give the order because numbers read as characters are in the wrong order
         highestLevel_sess_4 = factor(highestLevel_sess_4, levels = c( "SpaceCakesLevel 0-0", "SpaceCakesLevel 0-1", "SpaceCakesLevel 0-2",
                                                                       "SpaceCakesLevel 0-3", "SpaceCakesLevel 0-4", "SpaceCakesLevel 1-0", "SpaceCakesLevel 1-1",
                                                                       "SpaceCakesLevel 1-2", "SpaceCakesLevel 1-3", "SpaceCakesLevel 1-4", "SpaceCakesLevel 2-0",
                                                                       "SpaceCakesLevel 2-1", "SpaceCakesLevel 2-2", "SpaceCakesLevel 2-3", "SpaceCakesLevel 2-4",
                                                                       "SpaceCakesLevel 3-0", "SpaceCakesLevel 3-1", "SpaceCakesLevel 3-2", "SpaceCakesLevel 3-3", 
                                                                       "SpaceCakesLevel 3-4", "SpaceCakesLevel 4-0", "SpaceCakesLevel 4-1", "SpaceCakesLevel 4-2",
                                                                       "SpaceCakesLevel 4-3",  "SpaceCakesLevel 4-4", "SpaceCakesLevel 5-0", "SpaceCakesLevel 5-1",
                                                                       "SpaceCakesLevel 5-2",  "SpaceCakesLevel 5-3",  "SpaceCakesLevel 5-4",  "SpaceCakesLevel 6-0", 
                                                                       "SpaceCakesLevel 6-1",  "SpaceCakesLevel 6-2"), ordered = TRUE), #need to give the order because numbers read as characters are in the wrong order
         highestLevel_sess_5 = factor(highestLevel_sess_5, levels = c( "SpaceCakesLevel 0-0", "SpaceCakesLevel 0-1", "SpaceCakesLevel 0-2",
                                                                       "SpaceCakesLevel 0-3", "SpaceCakesLevel 0-4", "SpaceCakesLevel 1-0", "SpaceCakesLevel 1-1",
                                                                       "SpaceCakesLevel 1-2", "SpaceCakesLevel 1-3", "SpaceCakesLevel 1-4", "SpaceCakesLevel 2-0",
                                                                       "SpaceCakesLevel 2-1", "SpaceCakesLevel 2-2", "SpaceCakesLevel 2-3", "SpaceCakesLevel 2-4",
                                                                       "SpaceCakesLevel 3-0", "SpaceCakesLevel 3-1", "SpaceCakesLevel 3-2", "SpaceCakesLevel 3-3", 
                                                                       "SpaceCakesLevel 3-4", "SpaceCakesLevel 4-0", "SpaceCakesLevel 4-1", "SpaceCakesLevel 4-2",
                                                                       "SpaceCakesLevel 4-3",  "SpaceCakesLevel 4-4", "SpaceCakesLevel 5-0", "SpaceCakesLevel 5-1",
                                                                       "SpaceCakesLevel 5-2",  "SpaceCakesLevel 5-3",  "SpaceCakesLevel 5-4",  "SpaceCakesLevel 6-0", 
                                                                       "SpaceCakesLevel 6-1",  "SpaceCakesLevel 6-2"), ordered = TRUE), #need to give the order because numbers read as characters are in the wrong order
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
  
#Need to remove variables with many NAs, 
#or replace NAs with 0 for all of the AYCET data. Are a few people just missing from the DCCS data completely?

NAs_per_col <- data.frame(matrix(ncol = 2, nrow = 0))
NAs_per_col <- colSums(is.na(AYCET_DCCS))
#all _see_6 columns have a large number of NA's >140 dropping these; DONE in like 25
grep("_sess_6",colnames(AYCET_DCCS))

names(NAs_per_col)<-c("totalNA")
NAs_per_col<-NAs_per_col%>%arrange(-totalNA)
LassoNIHScore <- model.matrix(ImproverScore ~ ., AYCET_DCCS)[,-1]

#Lasso 1: Which variables are associated with improvement in NIH Score?
ImproverScore

#Lasso 2: Which variables are associated with high accuracy at the end?

#Lasso 3: Which variables are associated with low reaction times at the end (fast, correct responses)?
