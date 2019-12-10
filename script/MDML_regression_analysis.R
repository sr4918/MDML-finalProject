require(tidyverse)
library(readr)
require(ROCR)
require(ranger)
require(glmnet)


#For Lasso regression 

#Read in AYCET gameplay data and DCCS data
AYCET_gameplay_aggregated <- read_csv("data/AYCET_gameplay_aggregated.csv") 
#Find & get rid of '2' column
AYCET_gameplay_aggregated <- AYCET_gameplay_aggregated %>%
    #select(-c("2", "X1"))
    #select(-c("X1"))
    #select(-c("avgRT_afterWrong_NA"))
    

ALL_DCCS_data <- read_csv("data/ALL_DCCS_data.csv")


#Merge by userID so each row represents one participant
AYCET_DCCS <- left_join(AYCET_gameplay_aggregated, ALL_DCCS_data, by = c("userID"))

AYCET_DCCS <- AYCET_DCCS %>%
  #select(-c("avgRT_afterWrong_NA")) %>%
  select(-c("accessCode", "userID")) %>%
  select(-c("highestLevel_user" ,
            "highestLevel_diff_Easy" ,
            "highestLevel_diff_Medium" , 
            "highestLevel_diff_Difficult" ,
            "highestLevel_sess_1" ,
            "highestLevel_sess_2" ,
            "highestLevel_sess_3" ,
            "highestLevel_sess_4" ,
            "highestLevel_sess_5" ,
            "highestLevel_sess_6")) %>%
 mutate(ImproverScore = case_when(ImproverScore == TRUE ~ 1, 
                                  ImproverScore == FALSE ~ 0)) %>%
  select(-c("ImproverAccuracy", "ImproverRT", "AllImprove", "postScore"))

#Change characters to ordered factors
AYCET_DCCS <- AYCET_DCCS %>%
  mutate(highestLevel_user ,
         highestLevel_diff_Easy ,
         highestLevel_diff_Medium , 
         highestLevel_diff_Difficult ,
         highestLevel_sess_1 ,
         highestLevel_sess_2 ,
         highestLevel_sess_3 ,
         highestLevel_sess_4 ,
         highestLevel_sess_5 ,
         highestLevel_sess_6)

#colnames(AYCET_DCCS)

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

###########################
#LASSO Template
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

LassoNIHScore <- model.matrix(ImproverScore ~ ., AYCET_DCCS)[,-1]


ImproverScore

#Lasso 2: Which variables are associated with high accuracy at the end?

#Lasso 3: Which variables are associated with low reaction times at the end (fast, correct responses)?
