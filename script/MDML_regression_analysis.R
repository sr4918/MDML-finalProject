require(tidyverse)


#For Lasso regression 

#Read in AYCET gameplay data and DCCS data
AYCET_gameplay_aggregated <- read_csv("data/AYCET_gameplay_aggregated.csv")
DCCSCombinedData <- read_csv("data/DCCSCombinedData.csv")

#Merge by userID so each row represents one participant
AYCET_DCCS <- left_join(AYCET_gameplay_aggregated, DCCSCombinedData, by = c("userID"))

table(AYCET_DCCS$userID)
#check that each row is a unique participant
count <- AYCET_DCCS %>%
  group_by(userID) %>%
  summarize(n = n()) %>%
  filter(n > 1)




  
