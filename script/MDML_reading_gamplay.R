#MDML
#Reading in & combining AYCET gameplay files

#Load packages
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

#setwd("corinnebrenner")
#getwd()
####

#length(unique(ayce_40034_3005Edit$userID))
#147 AYCET users, but I see at least 1 known 'bad' user; these are test accounts or student errors that need to be removed

#userIDs to remove 
badUsers <- read_csv("data/testAccounts.csv")
badUsers <- c(badUsers$userID)

#List of access codes with AYCET data in their associated folders, needed to generate file paths + names
accessCodes <- c("ATHBF18", "ATHF18", "ATM1F18", "ATMBF18")

#Read in the files and append the gameCodes from different access codes to each other

#gameCode 4003
filepaths_4003 <- expand.grid(x=accessCodes) %>% 
#{paste0('../../../Desktop/FALL2018_Intervention/PLAY/', .$x, '/AYCE/ayce_4003.csv')}
{paste0('./Desktop/FALL2018_Intervention/PLAY/', .$x, '/AYCE/ayce_4003.csv')}


ayce_4003 <- do.call(rbind, lapply(filepaths_4003, read_csv))

#gameCode 4004
filepaths_4004 <- expand.grid(x=accessCodes) %>% 
#{paste0('../../../Desktop/FALL2018_Intervention/PLAY/', .$x, '/AYCE/ayce_4004.csv')}
{paste0('./Desktop/FALL2018_Intervention/PLAY/', .$x, '/AYCE/ayce_4004.csv')}


ayce_4004 <- do.call(rbind, lapply(filepaths_4004, read_csv))

#Change variable types and clean known issues with duplicate jots in gameCode 4003 and 4004 files
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


#Merge the files from gameCode 4003 and 4004
#gameCode 4003 is written to the database immediately before gameCode 4004, so by adding 1 to its jotID as jotID2, it's 
#useful for this merge 
ayce_40034 <- merge(ayce_4003, ayce_4004[,c("accessCode", "userID", "sesID", "gsUserID", "gameKey", "gameLevel","alienID", "reactionTime", "eyeReactionTime", "afterHighlightedReactionTime", "jotID")], 
                    by.x = c("accessCode", "userID", "sesID", "gsUserID", "gameKey","gameLevel", "alienID", "jotID2"),
                    by.y = c("accessCode", "userID", "sesID", "gsUserID", "gameKey","gameLevel", "alienID", "jotID")) %>%
        arrange(accessCode, userID, sesID, gsUserID, gameKey, gameLevel, alienID)

#Import and Merge 3005 data to get wave types and identify rule changes

#gameCode 3005
filepaths_3005 <- expand.grid(x=accessCodes) %>% 
#{paste0('../../../Desktop/FALL2018_Intervention/PLAY/', .$x, '/AYCE/ayce_3005.csv')}
{paste0('./Desktop/FALL2018_Intervention/PLAY/', .$x, '/AYCE/ayce_3005.csv')}


ayce_3005 <- do.call(rbind, lapply(filepaths_3005, read_csv))

#ayce_3005_2 <- ayce_3005 %>%
#  group_by(accessCode, userID, sesID, gsUserID, gameKey, gameLevel, jotID) %>%
#  summarize(n = n()) %>%
#  filter(n > 1)
  
#There were duplicates, so remove them and change variable types
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

#Create a lower bound and upper bound timestamp so you can join 3005 to 40034 based on time
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

#Remove duplicates (only differing based on logged time, due to logging errors), and filter out bad users
ayce_40034_3005 <- ayce_40034_3005 %>%
  select(accessCode, userID, sesID, gsUserID, gameKey, gameLevel, alienID, gameCode, logTimestamp, logID, jotID, gameTime,
         alienTypeAndColor, hitType, speed, reactionTime, eyeReactionTime, afterHighlightedReactionTime, waveType) %>%
  distinct() %>%
  arrange(accessCode, userID, sesID, gsUserID, gameKey, gameLevel, alienID) %>%
  filter(!userID %in% badUsers)

##############
#Add variables needed for further analysis

#Create a short game level label that excludes the last digit; the last digit represents the number of attempts that
#user has had on the level
ayce_40034_3005 <- ayce_40034_3005 %>%
  mutate(gameLevelShort = gsub('-[^-]*$', '', gameLevel),
          date = as.Date(logTimestamp, tz = ""))
  
#Create a new session count using date information, not just sessionID; sessionID is unique for every time a participant logs in
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
  
  #Join new session count variable in to main dataset
  ayce_40034_3005 <- left_join(ayce_40034_3005, sessionCountDF[, c("userID", "date", "sesID", "gsUserID", "sesCount", "sesCountAll")], by = c("userID", "date", "sesID", "gsUserID"))
  
#Import & merge information about the complexity of each game level (as assigned by game designer)
  #complexity <- read_csv("Desktop/MDML/MDML-finalProject/MDML-finalProject/data/FA2018_Intervention_Complexity.csv")
  complexity <- read_csv("data/FA2018_Intervention_Complexity.csv")
  ayce_40034_3005 <- left_join(ayce_40034_3005, complexity[, c("gameLevelShort", "Complexity")], by = c("gameLevelShort"))
  
  #Relabel complexity levels into fewer 'difficulty' categories to streamline analyses
  ayce_40034_3005 <- ayce_40034_3005 %>%
    mutate(Difficulty = case_when(Complexity < 3 ~ "Easy", 
                                  Complexity >=3 & Complexity < 6 ~ "Medium",
                                  Complexity > 5 ~ "Difficult" ),
           Difficulty = factor(Difficulty, levels = c("Easy", "Medium", "Difficult"), ordered = T))
  #Check
  #table(ayce_40034_3005$Complexity, ayce_40034_3005$Difficulty)
  

write.csv(ayce_40034_3005, "data/ayce_40034_3005.csv", row.names = F)  
#######USER  
  #Variables per user
  #The fastest reaction time for Hits (correct responses)
  fastestHits_user <- ayce_40034_3005 %>%
    filter(hitType == "HIT") %>%
    group_by(accessCode, userID) %>%
    summarize(fastestRT_user = min(reactionTime, na.rm = TRUE))
  
  #Highest level of the game reached per user
  highestLevels_user <- ayce_40034_3005 %>%
    group_by(userID) %>%
    summarize(highestLevel_user = max(gameLevel, na.rm = TRUE))
  
  #Filter rows to get counts of trials within the highest level (per user)
  highestLevelCounts_user <- left_join(ayce_40034_3005, highestLevels_user, by = c("userID") ) %>%
    filter (gameLevel == highestLevel_user) %>%
    group_by(userID) %>%
    summarize(highestLevelTrialCount_user = n()) 
  
  #Count all trials per user
  trialCounts_user <- ayce_40034_3005 %>% 
    group_by(userID)  %>%
    summarize(trialCount_user = n()) 
  
  #Number of levels played
  levelTotals_user <- ayce_40034_3005 %>%
    group_by(userID) %>%
    summarize(nLevels_user = n_distinct(gameLevel)) 
  
  #Total time played (seconds) per user, and per difficulty level
  totalTimePlayed_user <- ayce_40034_3005 %>%
    group_by(userID, sesID, gsUserID, gameLevel, gameLevelShort) %>%
    summarize(
      max = max(logTimestamp),
      min = min(logTimestamp),
      TimePlayedSubtotal = difftime(max(logTimestamp), min(logTimestamp), units = "secs")) %>%
    select(-c("max", "min", "sesID", "gsUserID", "gameLevel")) %>%
    mutate(
      TimePlayedSubtotal = ifelse(is.infinite(TimePlayedSubtotal), 0, TimePlayedSubtotal)) %>%
    ungroup() %>%
    left_join(complexity[, c("gameLevelShort", "Complexity")], by = c("gameLevelShort")) %>%
    mutate(Difficulty = case_when(Complexity < 3 ~ "Easy", #1 and 2
                                  Complexity >=3 & Complexity < 6 ~ "Medium", #3, 4, and 5
                                  Complexity > 5 ~ "Difficult" ), #6 and 7
           Difficulty = factor(Difficulty, levels = c("Easy", "Medium", "Difficult"), ordered = T)) %>%
    group_by(userID) %>%
    summarize(
      timePlayed_difficulty = as.character(sum(TimePlayedSubtotal, na.rm = TRUE)) 
    )
  
  #Count of hits, and the average & standard deviation of the reaction time for hits
  HitCount_user <- ayce_40034_3005 %>% #Hits
    group_by(userID)  %>%
    filter(hitType == "HIT") %>%
    summarize(HITS_user = n(), 
              Hit_AvgRT_user = mean(reactionTime, na.rm = TRUE), 
              Hit_SDRT_user = sd(reactionTime, na.rm = TRUE))
  
  #Calculate misses per user
  MissCount_user <- ayce_40034_3005 %>% #Misses
    group_by(userID)  %>%
    filter(hitType == "MISSED") %>%
    summarize(MISSED_user = n(), 
             Miss_AvgRT_user = mean(reactionTime, na.rm = TRUE), 
             Miss_SDRT_user = sd(reactionTime, na.rm = TRUE)) 
  
  #Calculate wrongs per user
  WrongCount_user <- ayce_40034_3005 %>% #Wrongs
    group_by(userID)  %>%
    filter(hitType == "WRONG") %>%
    summarize(WRONG_user = n(), 
              Wrong_AvgRT_user = mean(reactionTime, na.rm = TRUE), 
              Wrong_SDRT_user = sd(reactionTime, na.rm = TRUE)) 
 
 #Accuracy % for first alien after a rule change
  #Get a count of first aliens after a rule change per user & complexity level
  RuleChange_user <- ayce_40034_3005 %>%
    group_by(userID, gameLevelShort, Complexity) %>%
      arrange(alienID) %>%
      slice(1) %>%
    group_by(userID) %>%
      summarize(first_count = n())
  #Get a count of correct responses to first aliens after a rule change per user & complexity level
  RuleChange_user_correct <- ayce_40034_3005 %>%
    group_by(userID, gameLevelShort, Complexity) %>%
      arrange(alienID) %>%
      slice(1) %>%
      filter(hitType == "HIT") %>%
    group_by(userID) %>%
      summarize(first_count_correct = n())
  #Join & calculate accuracy percentage
  RuleChange_user <- left_join(RuleChange_user, RuleChange_user_correct, by = c("userID")) %>%
    mutate(RuleChange_Accuracy_Pct = round(first_count_correct/first_count, 2))
  
  #Avg Reaction time per complexity level
  RT_complexity_user_long <- ayce_40034_3005 %>% #Hits
    group_by(userID, Complexity)  %>%
    filter(hitType == "HIT") %>%
    summarize(HITS_comp = n(), 
              Hit_AvgRT_comp = mean(reactionTime, na.rm = TRUE), 
              Hit_SDRT_comp = sd(reactionTime, na.rm = TRUE))
  
      #######Graph complexity (x axis), avg RT (y axis), line per user
        rt_vs_complexity <- ggplot(data = RT_complexity_user_long, aes(x = Complexity, y = Hit_AvgRT_comp, group = userID,
                                                                        color = userID)) + 
         geom_line() + 
         geom_point() +
         theme(legend.position = "none")
        
        
        RT_diff_user_long <- ayce_40034_3005 %>% #Hits
          group_by(userID, Difficulty)  %>%
          filter(hitType == "HIT") %>%
          summarize(HITS_diff = n(), 
                    Hit_AvgRT_diff = mean(reactionTime, na.rm = TRUE), 
                    Hit_SDRT_diff = sd(reactionTime, na.rm = TRUE))
        
        rt_vs_difficulty <- ggplot(data = RT_diff_user_long, aes(x = Difficulty, y = Hit_AvgRT_diff, group = userID,
                                                                       color = userID)) + 
          geom_line() + 
          geom_point() +
          theme(legend.position = "none") + 
          labs(y = "Average Reaction Time (for Hits)", title = "Average Reaction Time versus Level Difficulty \nper Participant")
          #ggsave("Desktop/MDML/MDML-finalProject/MDML-finalProject/images/rt_vs_difficulty.png", rt_vs_difficulty)
          ####### 
        
  #Get RT complexity in wide format to join later
        RT_Difficulty_user <- pivot_wider(data = RT_diff_user_long, names_from = "Difficulty", 
                                          values_from = c("HITS_diff", "Hit_AvgRT_diff", "Hit_SDRT_diff"))
 
  #Trial counts for Accuracy Pct per Difficulty level
  Count_Difficulty_user <- ayce_40034_3005 %>% #all trials per complexity level
    group_by(userID, Difficulty)  %>%
    summarize(DifficultyTrialCount = n())
  
  #Hits for Accuracy Pct per complexity level
  Accuracy_Difficulty_user_long <- ayce_40034_3005 %>% #Hits
    group_by(userID, Difficulty)  %>%
    filter(hitType == "HIT") %>%
    summarize(Difficulty_Hits = n())
  
  #join data in long format for graphing
  Accuracy_Difficulty_user_long <- left_join(Count_Difficulty_user, Accuracy_Difficulty_user_long, by = c("userID", "Difficulty")) %>%
    mutate(Accuracy_Pct_diff = Difficulty_Hits/DifficultyTrialCount) %>%
    filter(!userID %in% badUsers)
  
      #######Graph complexity (x axis), accuracy % (y axis), line per user
      accuracy_vs_Difficulty <- ggplot(data = Accuracy_Difficulty_user_long, aes(x = Difficulty, y = Accuracy_Pct_diff, group = userID,
                                       color = userID)) + 
        geom_line() + 
        geom_point() +
        theme(legend.position = "none") +
        labs(y = "% Accurate Responses (for Hits)", title = "Accuracy Rate versus Level Difficulty \nper Participant")
      ggsave("Desktop/MDML/MDML-finalProject/MDML-finalProject/images/acc_vs_difficulty.png", accuracy_vs_Difficulty)
      
      
      #######
      #Wide format (to join later)
      Accuracy_Difficulty_user <- pivot_wider(data = Accuracy_Difficulty_user_long, names_from = "Difficulty", 
                                        values_from = c("DifficultyTrialCount", "Difficulty_Hits", "Accuracy_Pct_diff"))
      
  
#Accuracy for beginning, middle, end of session
      #Get average complexity for each section just for reference as well? 
ayce_40034_3005 <- ayce_40034_3005 %>%
  group_by(userID, sesCount) %>%
    arrange(logTimestamp) %>%
    mutate(session_thirds = ntile(sesCount, 3)) #creates thirds of each session in terms of #trials; time varies somewhat.
      #group_by(userID, sesCount, session_thirds) %>%
      
third_count_DF <- ayce_40034_3005 %>%
    group_by(userID, sesCount, session_thirds) %>%
    summarize(#third_time = max(logTimestamp) - min(logTimestamp),
               third_count = n(),
               avgComplexity = mean(Complexity, na.rm = T))
  
accuracy_thirds <- ayce_40034_3005 %>% #Hits
    group_by(userID, sesCount, session_thirds)  %>%
        filter(hitType == "HIT") %>%
        summarize(third_Hits = n())

Accuracy_vs_timeChunk <- left_join(third_count_DF, accuracy_thirds, by = c("userID", "sesCount", "session_thirds")) %>%
  mutate(third_accuracy = third_Hits/third_count) %>%
  filter(!userID %in% badUsers)

    ######Graph how users do at the beginning, middle, and end of sessions; can account for average complexity
    #facet by sesCount
    Accuracy_vs_timeChunk_plot <- ggplot( data = Accuracy_vs_timeChunk, aes(x = factor(session_thirds), y = third_accuracy, group = userID, color = avgComplexity)) +
      geom_line() + 
      geom_point() + 
      facet_grid(~ sesCount)
    ###### Come back to this -- how to show improvement over time, facet by time on x, complexity on y, color by accuracy or performance


#Accuracy & avg RT after Wrongs
afterWrongs <- ayce_40034_3005 %>%
  mutate(next_alien_hit = lead(hitType), 
         next_alien_RT = lead(reactionTime)) %>%
  filter(hitType == "WRONG") %>%
  arrange(userID, sesCount, gameLevel, alienID) %>%
  group_by(userID, next_alien_hit) %>%
  summarize(count_afterWrong = n(),
    avgRT_afterWrong = mean(next_alien_RT, na.rm = T)) %>%
  pivot_wider(names_from = "next_alien_hit", values_from = c("count_afterWrong", "avgRT_afterWrong")) %>%
  mutate(n_afterWrong = sum(count_afterWrong_HIT, count_afterWrong_WRONG, count_afterWrong_MISSED, na.rm = T),
         accuracy_afterWrong = count_afterWrong_HIT/n_afterWrong)

   
   #Join hit metrics for user level info
  userList <- list(fastestHits_user, highestLevels_user, highestLevelCounts_user, trialCounts_user, levelTotals_user, 
                             totalTimePlayed_user, HitCount_user, MissCount_user, WrongCount_user, RuleChange_user,
                              RT_Difficulty_user, Accuracy_Difficulty_user, afterWrongs)
  aggregate <- reduce(userList, full_join, by = c("userID"))
  
  #Calculate percent of all trials in highest level by session
  aggregate$highestLevelPct_user <- round(aggregate$highestLevelTrialCount_user/aggregate$trialCount_user, 2) * 100
  
  #Calculate Percent Correct
  aggregate$percentCorrect_user <- round(aggregate$HITS_user/rowSums(aggregate[,c("HITS_user", "MISSED_user", "WRONG_user")], na.rm = TRUE), 2)*100
  
  #Replace NA's with 0's
  aggregate[, c("HITS_user", "MISSED_user", "WRONG_user")][is.na(aggregate[, c("HITS_user", "MISSED_user", "WRONG_user")])] <- 0
  
  #HitRate with loglinear correction
  aggregate$HitRate_user <- round(((aggregate$HITS_user + .5)/(aggregate$trialCount_user + 1)),2)
  #False Alarm rate with loglinear correction
  aggregate$FARate_user <- round(((aggregate$WRONG_user + .5)/(aggregate$trialCount_user + 1)),2)
  
  #D Prime
  #str(aggregateUsers)
  aggregate$DPrime_user <- (qnorm(aggregate$HitRate_user) - qnorm(aggregate$FARate_user))  
    
#write.csv(aggregate, "data/aggregate_user.csv", row.names = F)  
  write.csv(aggregate, "Desktop/MDML/MDML-finalProject/MDML-finalProject/data/aggregate_user.csv", row.names = F)  

#########  
#Variables per user per session
  #Fastest hits per user per session
  fastestHits_sess <- ayce_40034_3005 %>%
    filter(hitType == "HIT") %>%
    group_by(accessCode, userID, sesCount) %>%
    summarize(fastestRT_sess = min(reactionTime, na.rm = TRUE))
  
  #Highest level per user per session
  highestLevels_sess <- ayce_40034_3005 %>%
    group_by(userID, sesCount) %>%
    summarize(highestLevel_sess = max(gameLevel, na.rm = TRUE))
  
  #Filter rows to get counts of trials within the highest level (per user per session)
  highestLevelCount_sess <- left_join(ayce_40034_3005, highestLevels_sess, by = c("userID", "sesCount") ) %>%
    filter (gameLevel == highestLevel_sess) %>%
    group_by(userID, sesCount) %>%
    summarize(highestTrialCount_sess = n()) 
  
  #Count all trials per user per session
  trialCounts_sess <- ayce_40034_3005 %>% 
    group_by(userID, sesCount)  %>%
    summarize(trialCount_sess = n())
  
  #Number of levels played per user per session
  levelTotals_sess <- ayce_40034_3005 %>%
    group_by(userID, sesCount) %>%
    summarize(nLevels_sess = n_distinct(gameLevel)) 
  
  #Total time played (seconds) per user per session
  totalTimePlayed_sess <- ayce_40034_3005 %>%
    group_by(userID, sesID, gsUserID, sesCount, gameLevel, gameLevelShort) %>%
    summarize(
      max = max(logTimestamp),
      min = min(logTimestamp),
      TimePlayedSubtotal = difftime(max(logTimestamp), min(logTimestamp), units = "secs")) %>%
    select(-c("max", "min", "sesID", "gsUserID", "gameLevel")) %>%
    mutate(
      TimePlayedSubtotal = ifelse(is.infinite(TimePlayedSubtotal), 0, TimePlayedSubtotal)) %>%
    ungroup() %>%
    left_join(complexity[, c("gameLevelShort", "Complexity")], by = c("gameLevelShort")) %>%
    mutate(Difficulty = case_when(Complexity < 3 ~ "Easy", #1 and 2
                                  Complexity >=3 & Complexity < 6 ~ "Medium", #3, 4, and 5
                                  Complexity > 5 ~ "Difficult" ), #6 and 7
           Difficulty = factor(Difficulty, levels = c("Easy", "Medium", "Difficult"), ordered = T)) %>%
    group_by(userID, sesCount) %>%
    summarize(
      timePlayed_sess = as.character(sum(TimePlayedSubtotal, na.rm = TRUE)) 
    )
  
  
  HitCount_sess <- ayce_40034_3005 %>% #Hits
    group_by(userID, sesCount)  %>%
    filter(hitType == "HIT") %>%
    summarize(HITS_sess = n(), 
              Hit_AvgRT_sess = mean(reactionTime, na.rm = TRUE), 
              Hit_SDRT_sess = sd(reactionTime, na.rm = TRUE)) 

  #Calculate misses per user and session
  MissCount_sess <- ayce_40034_3005 %>% #Misses
    group_by(userID, sesCount)  %>%
    filter(hitType == "MISSED") %>%
    summarize(MISSED_sess = n(), 
              Miss_AvgRT_sess = mean(reactionTime, na.rm = TRUE), 
              Miss_SDRT_sess = sd(reactionTime, na.rm = TRUE)) 
  
  #Calculate wrongs per user and session
  WrongCount_sess <- ayce_40034_3005 %>% #Wrongs
    group_by(userID, sesCount)  %>%
    filter(hitType == "WRONG") %>%
    summarize(WRONG_sess = n(), 
              Wrong_AvgRT_sess = mean(reactionTime, na.rm = TRUE), 
              Wrong_SDRT_sess = sd(reactionTime, na.rm = TRUE)) 
  
  #Join hit metrics for user level info per session
  aggregatedSessList <- list(fastestHits_sess, highestLevels_sess, highestLevelCount_sess, trialCounts_sess, levelTotals_sess, 
                             totalTimePlayed_sess, HitCount_sess, MissCount_sess, WrongCount_sess)
  aggregateSess <- reduce(aggregatedSessList, full_join, by = c("userID", "sesCount"))
  
  #Calculate percent of all trials in highest level by session
  aggregateSess$highestLevelPct_sess <- round(aggregateSess$highestTrialCount_sess/aggregateSess$trialCount_sess, 2) * 100
  
  #Calculate Percent Correct
  aggregateSess$percentCorrect_sess <- round(aggregateSess$HITS_sess/rowSums(aggregateSess[,c("HITS_sess", "MISSED_sess", "WRONG_sess")], na.rm = TRUE), 2)*100
  
  #Replace NA's with 0's
  aggregateSess[, c("HITS_sess", "MISSED_sess", "WRONG_sess")][is.na(aggregateSess[, c("HITS_sess", "MISSED_sess", "WRONG_sess")])] <- 0
  
  #HitRate with loglinear correction
  aggregateSess$HitRate_sess <- round(((aggregateSess$HITS_sess + .5)/(aggregateSess$trialCount_sess + 1)),2)
  #False Alarm rate with loglinear correction
  aggregateSess$FARate_sess <- round(((aggregateSess$WRONG_sess + .5)/(aggregateSess$trialCount_sess + 1)),2)
  
  #D Prime
  #str(aggregateUsers)
  aggregateSess$DPrime_sess <- (qnorm(aggregateSess$HitRate_sess) - qnorm(aggregateSess$FARate_sess))  
  
  
  #Spread out long format to wide, for as many sessions as needed
  aggregateSessWide <- aggregateSess %>%
    filter(!userID %in% badUsers) %>%
    #select(-c(sesID, max, min)) %>% #Don't actually need AYCE_max, AYCE_min, can take them out here
    pivot_wider(names_from = "sesCount", values_from = c("fastestRT_sess",  "highestLevel_sess",  "highestTrialCount_sess", "trialCount_sess",
                                                         "nLevels_sess", "timePlayed_sess", "HITS_sess", "Hit_AvgRT_sess", "Hit_SDRT_sess",
                                                         "MISSED_sess", "Miss_AvgRT_sess", "Miss_SDRT_sess",
                                                         "WRONG_sess", "Wrong_AvgRT_sess", "Wrong_SDRT_sess", "highestLevelPct_sess", 
                                                         "percentCorrect_sess", "HitRate_sess", "FARate_sess", "DPrime_sess"))
  
  write.csv(aggregateSessWide, "Desktop/MDML/MDML-finalProject/MDML-finalProject/data/aggregate_session.csv", row.names = F)  
  
  #write.csv(aggregateSessWide, "data/aggregate_session.csv", row.names = F)  
  #########  
  #Variables per user per complexity (translated to Difficulty as easy, medium, hard)
  #table(ayce_40034_3005$Complexity)

  
  #Fastest hits per user per difficulty
  fastestHits_diff <- ayce_40034_3005 %>%
    filter(hitType == "HIT") %>%
    group_by(accessCode, userID, Difficulty) %>%
    summarize(fastestRT_diff = min(reactionTime, na.rm = TRUE))
  
  #Highest level per user per difficulty
  highestLevels_diff <- ayce_40034_3005 %>%
    group_by(userID, Difficulty) %>%
    summarize(highestLevel_diff = max(gameLevel, na.rm = TRUE))
  
  #Filter rows to get counts of trials within the highest level (per user per difficulty)
  highestLevelCounts_diff <- left_join(ayce_40034_3005, highestLevels_diff, by = c("userID", "Difficulty") ) %>%
    filter (gameLevel == highestLevel_diff) %>%
    group_by(userID, Difficulty) %>%
    summarize(highestLevelTrialCount_diff = n()) 
  
  #Count all trials per user per difficulty
  trialCounts_diff <- ayce_40034_3005 %>% 
    group_by(userID, Difficulty)  %>%
    summarize(trialCount_diff = n())
  
  #Number of levels played per user per difficulty
  levelsTotals_diff <- ayce_40034_3005 %>%
    group_by(userID, Difficulty) %>%
    summarize(nLevels_diff = n_distinct(gameLevel)) 
  
  #Total time played (seconds) per user per difficulty #this isn't right yet. 
  totalTimePlayed_diff <- ayce_40034_3005 %>%
    group_by(userID, sesID, gsUserID, gameLevel, gameLevelShort) %>%
    summarize(
      max = max(logTimestamp),
      min = min(logTimestamp),
      TimePlayedSubtotal = difftime(max(logTimestamp), min(logTimestamp), units = "secs")) %>%
    select(-c("max", "min", "sesID", "gsUserID", "gameLevel")) %>%
    mutate(
      TimePlayedSubtotal = ifelse(is.infinite(TimePlayedSubtotal), 0, TimePlayedSubtotal)) %>%
    ungroup() %>%
    left_join(complexity[, c("gameLevelShort", "Complexity")], by = c("gameLevelShort")) %>%
    mutate(Difficulty = case_when(Complexity < 3 ~ "Easy", #1 and 2
                                  Complexity >=3 & Complexity < 6 ~ "Medium", #3, 4, and 5
                                  Complexity > 5 ~ "Difficult" ), #6 and 7
           Difficulty = factor(Difficulty, levels = c("Easy", "Medium", "Difficult"), ordered = T)) %>%
    group_by(userID, Difficulty) %>%
    summarize(
      timePlayed_difficulty = as.character(sum(TimePlayedSubtotal, na.rm = TRUE)) 
    )

      
  HitCount_diff <- ayce_40034_3005 %>% #Hits
    group_by(userID, Difficulty)  %>%
    filter(hitType == "HIT") %>%
    summarize(HITS_diff = n(), 
              Hit_AvgRT_diff = mean(reactionTime, na.rm = TRUE), 
              Hit_SDRT_diff = sd(reactionTime, na.rm = TRUE)) 
  
  #Calculate misses per user and session
  MissCount_diff <- ayce_40034_3005 %>% #Misses
    group_by(userID, Difficulty)  %>%
    filter(hitType == "MISSED") %>%
    summarize(MISSED_diff = n(), 
              Miss_AvgRT_diff = mean(reactionTime, na.rm = TRUE), 
              Miss_SDRT_diff = sd(reactionTime, na.rm = TRUE)) 
  
  #Calculate wrongs per user and session
  WrongCount_diff <- ayce_40034_3005 %>% #Wrongs
    group_by(userID, Difficulty)  %>%
    filter(hitType == "WRONG") %>%
    summarize(WRONG_diff = n(), 
              Wrong_AvgRT_diff = mean(reactionTime, na.rm = TRUE), 
              Wrong_SDRT_diff = sd(reactionTime, na.rm = TRUE)) 
  
  #Join hit metrics for user level info per difficulty #INCOMPLETE
  aggregatedDiffList <- list(fastestHits_diff, highestLevels_diff, highestLevelCounts_diff, trialCounts_diff, levelsTotals_diff, 
                             totalTimePlayed_diff, HitCount_diff, MissCount_diff, WrongCount_diff)
  aggregateDiff <- reduce(aggregatedDiffList, full_join, by = c("userID", "Difficulty"))
  
  #Calculate percent of all trials in highest level by difficulty
  aggregateDiff$highestLevelPct_diff <- round(aggregateDiff$highestLevelTrialCount_diff/aggregateDiff$trialCount_diff, 2) * 100
  
  #Calculate Percent Correct
  aggregateDiff$percentCorrect_diff <- round(aggregateDiff$HITS_diff/rowSums(aggregateDiff[,c("HITS_diff", "MISSED_diff", "WRONG_diff")], na.rm = TRUE), 2)*100
  
  #Replace NA's with 0's
  aggregateDiff[, c("HITS_diff", "MISSED_diff", "WRONG_diff")][is.na(aggregateDiff[, c("HITS_diff", "MISSED_diff", "WRONG_diff")])] <- 0
  
  #HitRate with loglinear correction
  aggregateDiff$HitRate_diff <- round(((aggregateDiff$HITS_diff + .5)/(aggregateDiff$trialCount_diff + 1)),2)
  #False Alarm rate with loglinear correction
  aggregateDiff$FARate_diff <- round(((aggregateDiff$WRONG_diff + .5)/(aggregateDiff$trialCount_diff + 1)),2)
  
  #D Prime
  #str(aggregateUsers)
  aggregateDiff$DPrime_diff <- (qnorm(aggregateDiff$HitRate_diff) - qnorm(aggregateDiff$FARate_diff))  
  
  #Spread out long format to wide, for as many difficulty levels as needed
  aggregateDiffWide <- aggregateDiff %>%
    pivot_wider(names_from = "Difficulty", values_from = c("fastestRT_diff", "highestLevel_diff", "highestLevelTrialCount_diff",
                                                           "trialCount_diff", "nLevels_diff", "timePlayed_difficulty", 
                                                           "HITS_diff", "Hit_AvgRT_diff", "Hit_SDRT_diff", 
                                                           "MISSED_diff", "Miss_AvgRT_diff", "Miss_SDRT_diff",
                                                           "WRONG_diff", "Wrong_AvgRT_diff", "Wrong_SDRT_diff",
                                                           "highestLevelPct_diff", "percentCorrect_diff", "HitRate_diff", "FARate_diff",
                                                           "DPrime_diff"))
  
write.csv(aggregateDiffWide, "Desktop/MDML/MDML-finalProject/MDML-finalProject/data/aggregate_difficulty.csv", row.names = F)  
#Aggregate everything at the user level  
aggregate <- arrange(aggregate, accessCode, userID)
aggregateDiffWide <- arrange(aggregateDiffWide, accessCode, userID)
aggregateSessWide <- arrange(aggregateSessWide, accessCode, userID)


gameplay_aggregated <- bind_cols(aggregate, aggregateDiffWide[,3:62])
gameplay_aggregated <- bind_cols(gameplay_aggregated, aggregateSessWide[,3:122])


#All NA:
gameplay_aggregated <- gameplay_aggregated %>%
  select(-c("avgRT_afterWrong_NA"))
  
#Export
  write.csv(gameplay_aggregated, "Desktop/MDML/MDML-finalProject/MDML-finalProject/data/AYCET_gameplay_aggregated.csv", row.names = F)