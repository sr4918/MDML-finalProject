#MDML
#Reading in & combining AYCET gameplay files
require(tidyverse)
require(purrr)
library(doParallel)
library(foreach)
library(lubridate)
library(ROCR)
#install.packages("compareDF")
require(compareDF)
#install.packages("sqldf")
require(sqldf)


####
getwd()

#List of access codes with AYCET data in them, to generate file paths + names
accessCodes <- c("ATHBF18", "ATHF18", "ATM1F18", "ATMBF18")
#gameCodes <- c(3009, 4003, 4004)

#Useful as a way to list all the files, but not actually used here
#filenames <- expand.grid(x=accessCodes, y=gameCodes) %>% 
#{paste0(.$x, '_', .$y)}

#filepaths <- expand.grid(x=accessCodes, y=gameCodes) %>% 
#{paste0('/Desktop/FALL2018_Intervention/PLAY/', .$x, '/AYCE/ayce_', .$y, '.csv')}

#Read in the files and append the same gameCodes to each other

#3009
#filepaths_3009 <- expand.grid(x=accessCodes) %>% 
#{paste0('./Desktop/FALL2018_Intervention/PLAY/', .$x, '/AYCE/ayce_3009.csv')}

#ayce_3009 <- do.call(rbind, lapply(filepaths_3009, read_csv))
#Note paths, in case this changes
#4003
filepaths_4003 <- expand.grid(x=accessCodes) %>% 
{paste0('../../../Desktop/FALL2018_Intervention/PLAY/', .$x, '/AYCE/ayce_4003.csv')}

ayce_4003 <- do.call(rbind, lapply(filepaths_4003, read_csv))

#4004
filepaths_4004 <- expand.grid(x=accessCodes) %>% 
{paste0('../../../Desktop/FALL2018_Intervention/PLAY/', .$x, '/AYCE/ayce_4004.csv')}

ayce_4004 <- do.call(rbind, lapply(filepaths_4004, read_csv))

#Change variable types and clean known issues with duplicate jots in 4003 and 4004 files
ayce_4003 <- ayce_4003 %>%
  mutate(jotID2 = as.integer(jotID + 1),
         accessCode = factor(accessCode),
         userID = factor(userID),
         sesID = factor(sesID),
         gsUserID = factor(gsUserID),
         gameCode = factor(gameCode),
         logTimestamp = as.POSIXct(logTimestamp, format = "%Y-%m-%d %H:%M:%OS"),
         gameLevel = factor(gameLevel, levels = c( "SpaceCakesLevel 0-0-0", "SpaceCakesLevel 0-0-1", "SpaceCakesLevel 0-0-2",  "SpaceCakesLevel 0-0-3",  "SpaceCakesLevel 0-0-4", 
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
                            ordered = TRUE), #need to give the order because numbers read as characters are wrong
         alienTypeAndColor = factor(alienTypeAndColor),
         hitType = factor(hitType, levels = c("HIT", "MISSED", "WRONG"), labels = c("HIT", "MISSED", "WRONG"))
  ) %>%
  group_by(accessCode, userID, sesID, gsUserID, gameKey, gameLevel, alienID, jotID2) %>%
  arrange(logTimestamp) %>%
  slice(1) %>% #remove duplicate logged items
  ungroup()

#from 243580 to 243557 rows


ayce_4004 <- ayce_4004 %>%
  mutate(accessCode = factor(accessCode),
         userID = factor(userID),
         sesID = factor(sesID),
         gsUserID = factor(gsUserID),
         gameCode = factor(gameCode),
         logTimestamp = as.POSIXct(logTimestamp, format = "%Y-%m-%d %H:%M:%OS"),
         gameLevel = factor(gameLevel, levels = c( "SpaceCakesLevel 0-0-0", "SpaceCakesLevel 0-0-1", "SpaceCakesLevel 0-0-2",  "SpaceCakesLevel 0-0-3",  "SpaceCakesLevel 0-0-4", 
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
                            ordered = TRUE), #need to give the order because numbers read as characters are wrong
         reactionTime = as.numeric(reactionTime),
         eyeReactionTime = as.numeric(eyeReactionTime),
         afterHighlightedReactionTime = as.numeric(afterHighlightedReactionTime)
  ) %>%
  group_by(accessCode, userID, sesID, gsUserID, gameKey, gameLevel, alienID, jotID) %>%
  arrange(logTimestamp) %>%
  slice(1) %>% #remove duplicate logged items
  ungroup()


#Merge 4003 and 4004 files

ayce_40034 <- merge(ayce_4003, ayce_4004[,c("accessCode", "userID", "sesID", "gsUserID", "gameKey", "gameLevel","alienID", "reactionTime", "eyeReactionTime", "afterHighlightedReactionTime", "jotID")], 
                    by.x = c("accessCode", "userID", "sesID", "gsUserID", "gameKey","gameLevel", "alienID", "jotID2"),
                    by.y = c("accessCode", "userID", "sesID", "gsUserID", "gameKey","gameLevel", "alienID", "jotID"))

#Import and Merge 3005 datato get wave types and identify rule changes

#3005
filepaths_3005 <- expand.grid(x=accessCodes) %>% 
{paste0('../../../Desktop/FALL2018_Intervention/PLAY/', .$x, '/AYCE/ayce_3005.csv')}

ayce_3005 <- do.call(rbind, lapply(filepaths_3005, read_csv))

#There were duplicates, so remove them 
ayce_3005_3 <- ayce_3005 %>%
  group_by(accessCode, userID, sesID, gsUserID, gameKey, gameLevel, jotID) %>%
  arrange(logTimestamp) %>%
  slice(1) %>% #remove duplicate logged items
  ungroup() %>%
  mutate(accessCode = factor(accessCode),
         userID = factor(userID),
         sesID = factor(sesID),
         gsUserID = factor(gsUserID),
         gameCode = factor(gameCode),
         logTimestamp = as.POSIXct(logTimestamp, format = "%Y-%m-%d %H:%M:%OS"),
         gameLevel = factor(gameLevel, levels = c( "SpaceCakesLevel 0-0-0", "SpaceCakesLevel 0-0-1", "SpaceCakesLevel 0-0-2",  "SpaceCakesLevel 0-0-3",  "SpaceCakesLevel 0-0-4", 
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
                            ordered = TRUE), #need to give the order because numbers read as characters are wrong
         waveType = factor(waveType)
  )

#Add a lower bound and upper bound timestamp so you can jointo 40034 based on time
ayce_3005_3$lowerBound <- (ayce_3005_3$logTimestamp - 1)
ayce_3005_3$upperBound <- lead(ayce_3005_3$logTimestamp)

#This uses the sqldf package, make sure it's loaded
ayce_40034_3005 <- sqldf::sqldf("SELECT ayce_40034.accessCode, ayce_40034.userID,  ayce_40034.sesID, ayce_40034.gsUserID, ayce_40034.gameKey, 
            ayce_40034.gameLevel, ayce_40034.alienID, ayce_40034.gameCode, ayce_40034.logTimestamp, ayce_40034.logID, 
                         ayce_40034.jotID, ayce_40034.gameTime, ayce_40034.alienTypeAndColor, ayce_40034.hitType, ayce_40034.speed, 
                         ayce_40034.reactionTime, ayce_40034.eyereactionTime, ayce_40034.afterHighlightedReactionTime,
                         ayce_3005_3.accessCode, ayce_3005_3.userID,  ayce_3005_3.sesID, ayce_3005_3.gsUserID, ayce_3005_3.gameKey, 
                         ayce_3005_3.gameLevel, ayce_3005_3.gameCode, ayce_3005_3.logTimestamp, 
                         ayce_3005_3.jotID, ayce_3005_3.waveType, ayce_3005_3.lowerBound, ayce_3005_3.upperBound
                         FROM ayce_40034 AS ayce_40034
                         LEFT JOIN ayce_3005_3 as ayce_3005_3 ON 
                         ayce_40034.accessCode = ayce_3005_3.accessCode AND
                         ayce_40034.userID = ayce_3005_3.userID AND
                         ayce_40034.sesID = ayce_3005_3.sesID AND
                         ayce_40034.gsUserID = ayce_3005_3.gsUserID AND
                         ayce_40034.gameKey = ayce_3005_3.gameKey AND
                         ayce_40034.gameLevel = ayce_3005_3.gameLevel AND
                         ayce_40034.logTimestamp > ayce_3005_3.lowerBound AND ayce_40034.logTimestamp < ayce_3005_3.upperBound") 

#Remove duplicates (only differing based on logged time)
ayce_40034_3005 <- ayce_40034_3005 %>%
  select(accessCode, userID, sesID, gsUserID, gameKey, gameLevel, alienID, gameCode, logTimestamp, logID, jotID, gameTime,
         alienTypeAndColor, hitType, speed, reactionTime, eyeReactionTime, afterHighlightedReactionTime, waveType) %>%
  distinct() %>%
  arrange(accessCode, userID, sesID, gsUserID, gameKey, gameLevel, alienID)
  #Still has 10 extra rows but I don't know how to find them...
##############
#Add needed variables for further analysis
ayce_40034_3005 <- ayce_40034_3005 %>%
  mutate(gameLevelShort = gsub('-[^-]*$', '', gameLevel),
          date = as.Date(logTimestamp, tz = ""))
  
#Create a new session count by date 
sessionCountDF <- ayce_40034_3005 %>%
  arrange(userID, date, sesID, logTimestamp, gsUserID, gameLevelShort, gameLevel, alienID) %>%
  group_by(userID, date, sesID, gsUserID) %>%
    summarize(timeFix = min(logTimestamp)) %>%
    arrange(userID, date, sesID, timeFix) %>%
  ungroup() %>%
  group_by(userID) %>%
    mutate(sesCountAll = seq(1:n()), #Counts every single session defined by sessionID/gsUserID
          sesCount = dense_rank(date)) %>% #Combines sessionIDs by date
  ungroup()
  
  ayce_40034_3005 <- left_join(ayce_40034_3005, sessionCountDF[, c("userID", "date", "sesID", "gsUserID", "sesCount", "sesCountAll")], by = c("userID", "date", "sesID", "gsUserID"))
  
#Variables per user
  AYCET_fastestHits_user <- ayce_40034_3005 %>%
    filter(hitType == "HIT") %>%
    group_by(accessCode, userID) %>%
    summarize(sessFastestRT = min(reactionTime, na.rm = TRUE))
  
  #Highest level per participant per session
  AYCET_highestLevel_user <- ayce_40034_3005 %>%
    group_by(userID) %>%
    summarize(userHighestLevel = max(gameLevel, na.rm = TRUE))
  
  #Filter rows to get counts of trials within the highest level (per user per session)
  highestLevelCount_user <- ayce_40034_3005 %>%
    filter (gameLevel == sessionHighestLevel) %>%
    group_by(userID) %>%
    summarize(userHighestTrialCount = n()) 
  
  #Count all trials per session
  AYCET_trialsCount_user <- ayce_40034_3005 %>% 
    group_by(userID)  %>%
    summarize(userTrialCount = n()) 
  
  #Number of levels played
  AYCET_levelsTotal_user <- ayce_40034_3005 %>%
    group_by(userID) %>%
    summarize(nLevels = n_distinct(gameLevel)) 
  
  #Total time played (seconds) per sessions
  AYCET_totalTimePlayed_user <- ayce_40034_3005 %>%
    group_by(userID) %>%
    summarize(
      max = max(logTimestamp),
      min = min(logTimestamp),
      TimePlayed = difftime(max(logTimestamp), min(logTimestamp), units = "secs")) 
  
  HitCount_user <- ayce_40034_3005 %>% #Hits
    group_by(userID)  %>%
    filter(hitType == "HIT") %>%
    summarize(HITS = n(), 
              AvgHitRT = mean(reactionTime, na.rm = TRUE), 
              SDHitRT = sd(reactionTime, na.rm = TRUE))
  
  #Calculate misses per user and session
  MissCount_user <- ayce_40034_3005 %>% #Misses
    group_by(userID)  %>%
    filter(hitType == "MISSED") %>%
    summarize(MISSED = n(), 
              AvgMissRT = mean(reactionTime, na.rm = TRUE), 
              SDMissRT = sd(reactionTime, na.rm = TRUE)) 
  
  #Calculate wrongs per user and session
  WrongCount_user <- ayce_40034_3005 %>% #Wrongs
    group_by(userID)  %>%
    filter(hitType == "WRONG") %>%
    summarize(WRONG = n(), 
              AvgWrongRT = mean(reactionTime, na.rm = TRUE), 
              SDWrongRT = sd(reactionTime, na.rm = TRUE)) 
  
  
#########  
#Variables per user per session
  #Fastest hits per user per session
  AYCET_fastestHits_sess <- ayce_40034_3005 %>%
    filter(hitType == "HIT") %>%
    group_by(accessCode, userID, sessionCount) %>%
    summarize(sessFastestRT = min(reactionTime, na.rm = TRUE))
  
  #Highest level per participant per session
  AYCET_highestLevel_Sess <- ayce_40034_3005 %>%
    group_by(userID, sessionCount) %>%
    summarize(sessionHighestLevel = max(gameLevel, na.rm = TRUE)) %>%
    group_by(userID) %>%
    mutate(userHighestLevel = max(sessionHighestLevel, na.rm = TRUE))
  
  #Filter rows to get counts of trials within the highest level (per user per session)
  AYCET_highestLevelCount_Sess <- ayce_40034_3005 %>%
    filter (gameLevel == sessionHighestLevel) %>%
    group_by(userID, sessionCount) %>%
    summarize(sessionHighestTrialCount = n()) 
  
  #Count all trials per session
  AYCET_trialCount_Sess <- ayce_40034_3005 %>% 
    group_by(userID, sessionCount)  %>%
    summarize(sessTrialCount = n())
  
  #Number of levels played
  AYCET_levelsTotal_sess <- ayce_40034_3005 %>%
    group_by(userID, sessionCount) %>%
    summarize(nLevels = n_distinct(gameLevel)) 
  
  #Total time played (seconds) per sessions
  AYCET_totalTimePlayed_Sess <- ayce_40034_3005 %>%
    group_by(userID, sessionCount) %>%
    summarize(
      max = max(logTimestamp),
      min = min(logTimestamp),
      TimePlayedSubtotal = difftime(max(logTimestamp), min(logTimestamp), units = "secs")) %>%
    mutate(
      TimePlayedSubtotal = ifelse(is.infinite(TimePlayedSubtotal), 0, TimePlayedSubtotal)) %>%
    group_by(userID, sesID) %>%
    summarize(
      sessTimePlayed_Sec = as.character(sum(TimePlayedSubtotal, na.rm = TRUE)) 
    )
  
  HitCount_Sess <- AYCET_40034 %>% #Hits
    group_by(userID, sessionCount)  %>%
    filter(hitType == "HIT") %>%
    summarize(HITS = n(), 
              sessAvgHitRT = mean(reactionTime, na.rm = TRUE), 
              sessSDHitRT = sd(reactionTime, na.rm = TRUE)) 

  #Calculate misses per user and session
  MissCount_Sess <- AYCET_40034 %>% #Misses
    group_by(userID, sessionCount)  %>%
    filter(hitType == "MISSED") %>%
    summarize(MISSED = n(), 
              sessAvgMissRT = mean(reactionTime, na.rm = TRUE), 
              sessSDMissRT = sd(reactionTime, na.rm = TRUE)) 
  
  #Calculate wrongs per user and session
  WrongCount_Sess <- AYCET_40034 %>% #Wrongs
    group_by(userID, sessionCount)  %>%
    filter(hitType == "WRONG") %>%
    summarize(WRONG = n(), 
              sessAvgWrongRT = mean(reactionTime, na.rm = TRUE), 
              sessSDWrongRT = sd(reactionTime, na.rm = TRUE)) 
  
  #Join hit metrics for user level info per session
  aggregatedSessList <- list(AYCET_fastestHits_sess, AYCET_highestLevel_Sess, AYCET_highestLevelCount_Sess, AYCET_trialCount_Sess, AYCET_levelsTotal_sess, 
                             AYCET_totalTimePlayed_Sess, HitCount_Sess, MissCount_Sess, WrongCount_Sess)
  aggregateSess <- reduce(aggregatedSessList, full_join, by = c("userID", "sesID"))
  
  #Calculate percent of all trials in highest level by session
  aggregateSess$highestLevelPct <- round(aggregateSess$sessionHighestTrialCount/aggregateSess$sessTrialCount, 2) * 100
  
  #Calculate Percent Correct
  aggregateSess$PercentCorrect <- round(aggregateSess$HITS/rowSums(aggregateSess[,c("HITS", "MISSED", "WRONG")], na.rm = TRUE), 2)*100
  
  #Replace NA's with 0's
  aggregateSess[, c("HITS", "MISSED", "WRONG")][is.na(aggregateSess[, c("HITS", "MISSED", "WRONG")])] <- 0
  
  #HitRate with loglinear correction
  aggregateSess$HitRate <- round(((aggregateSess$HITS + .5)/(aggregateSess$sessTrialCount + 1)),2)
  #False Alarm rate with loglinear correction
  aggregateSess$FARate <- round(((aggregateSess$WRONG + .5)/(aggregateSess$sessTrialCount + 1)),2)
  
  #D Prime
  #str(aggregateUsers)
  aggregateSess$DPrime <- (qnorm(aggregateSess$HitRate) - qnorm(aggregateSess$FARate))  