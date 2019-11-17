#SAMPLE
#AYCET - set working directory, import files, change variable types
getwd()
#local 
#setwd("FALL2018_Intervention/PLAY/AHF18/AYCE")

#Import all gamecodes
AYCET_4001 <- read.csv("ayce_4001.csv", header = TRUE)
AYCET_4002 <- read.csv("ayce_4002.csv", header = TRUE)
AYCET_4003 <- read.csv("ayce_4003.csv", header = TRUE)
AYCET_4004 <- read.csv("ayce_4004.csv", header = TRUE)
AYCET_4005 <- read.csv("ayce_4005.csv", header = TRUE)
AYCET_4006 <- read.csv("ayce_4006.csv", header = TRUE)


AYCET_2001 <- read.csv("ayce_2001.csv", header = TRUE)
#Bad merge
#AYCET_40034 <- merge(AYCET_4003, AYCET_4004[,c("accessCode", "userID", "participantID", "sesID", "gsUserID", "gameKey", "gameLevel", "logTimestamp" ,"alienID", "reactionTime", "eyeReactionTime", "afterHighlightedReactionTime")], by = c("accessCode", "userID", "sesID", "gsUserID", "gameKey", "logTimestamp","gameLevel", "alienID"))

#Add 1 to Jot ID in gamecode 4003, and use that in merge
AYCET_4003$jotID2 <- as.integer(AYCET_4003$jotID + 1)
#Check
summary(AYCET_4004$jotID)
summary(AYCET_4003$jotID)
summary(AYCET_4003$jotID2)

#Merge - as close as I can get
AYCET_40034 <- merge(AYCET_4003, AYCET_4004[,c("accessCode", "userID", "sesID", "gsUserID", "gameKey", "gameLevel","alienID", "reactionTime", "eyeReactionTime", "afterHighlightedReactionTime", "jotID")], 
                     by.x = c("accessCode", "userID", "sesID", "gsUserID", "gameKey","gameLevel", "alienID", "jotID2"),
                     by.y = c("accessCode", "userID", "sesID", "gsUserID", "gameKey","gameLevel", "alienID", "jotID"))

write.csv(AYCET_40034, "AHF18_AYCET_40034_remerge.csv", row.names = F)

#Not sure why the merge isn't working. Sorted & cut & paste off R.
#AYCET_40034 <- read.csv("test4003_4 - Sheet1.csv", header = TRUE)
#table(AYCET_40034$userID, AYCET_40034$userID.2)

#Change data types
AYCET_40034$userID <- factor(AYCET_40034$userID)
AYCET_40034$sesID <- factor(AYCET_40034$sesID)
AYCET_40034$gsUserID <- factor(AYCET_40034$gsUserID)
AYCET_40034$gameCode <- factor(AYCET_40034$gameCode)
AYCET_40034$logTimestamp <- as.POSIXct(AYCET_40034$logTimestamp, format = "%Y-%m-%d %H:%M:%OS")
#AYCET_4003$gameKey <- factor(AYCET_4003$gameKey)
#AYCET_4003$logID <- as.integer(AYCET_4003$logID)
#AYCET_4003$jotID <- as.integer(AYCET_4003$jotID)
AYCET_40034$gameLevel <- factor(AYCET_40034$gameLevel, ordered = TRUE) #order it, revise as necessary
AYCET_40034$alienTypeAndColor <- factor(AYCET_40034$alienTypeAndColor)
AYCET_40034$hitType <- factor(AYCET_40034$hitType, levels = c("HIT", "MISSED", "WRONG"), labels = c("HIT", "MISSED", "WRONG"))
AYCET_40034$reactionTime <- as.numeric(AYCET_40034$reactionTime)
AYCET_40034$eyeReactionTime <- as.numeric(AYCET_40034$eyeReactionTime)
AYCET_40034$afterHighlightedReactionTime <- as.numeric(AYCET_40034$afterHighlightedReactionTime)

#Calculate key variables 

#Fastest hits per user per session
AYCET_fastestHits <- AYCET_40034 %>%
  filter(hitType == "HIT") %>%
  group_by(accessCode, userID, sesID) %>%
  summarize(sessFastestRT = min(reactionTime, na.rm = TRUE)) %>% 
  #userAvgHitRT = mean(reactionTime, na.rm = TRUE), 
  #userSDHitRT = sd(reactionTime, na.rm = TRUE)) %>%
  as.data.frame()

#First (earliest) & Last (Most recent) session per user
AYCET_lastSes <- AYCET_40034 %>%
  group_by(userID, sesID) %>%
  summarize(earliestTime = min(logTimestamp, na.rm = TRUE),
            latestTime = max(logTimestamp, na.rm = TRUE)) %>%
  as.data.frame()

#Highest level per participant per session
AYCET_highestLevel <- AYCET_40034 %>%
  group_by(userID, sesID) %>%
  summarize(sessionHighestLevel = max(gameLevel, na.rm = TRUE)) %>%
  group_by(userID) %>%
  mutate(userHighestLevel = max(sessionHighestLevel, na.rm = TRUE))%>% 
  as.data.frame() 

AYCET_40034 <- 
  left_join(AYCET_40034, AYCET_highestLevel, by = c("userID", "sesID")) %>%
  as.data.frame()

#Filter rows to get counts of trials within the highest level (per user per session)
sessionLevelInfo <- AYCET_40034 %>%
  filter (gameLevel == sessionHighestLevel) %>%
  group_by(userID, sesID) %>%
  summarize(sessionHighestTrialCount = n()) %>%
  as.data.frame()

#Count all trials per session
AYCET_userTrials <- AYCET_40034 %>% 
  group_by(userID, sesID)  %>%
  summarize(sessTrialCount = n()) %>%
  as.data.frame()

#Number of levels played
AYCET_levelsTotal_sess <- AYCET_40034 %>%
  group_by(userID, sesID) %>%
  summarize(nLevels = n_distinct(gameLevel)) %>%
  as.data.frame()

#Total time played (seconds) per sessions
AYCET_totalTimePlayed <- AYCET_40034 %>%
  group_by(userID, sesID) %>%
  summarize(
    max = max(logTimestamp),
    min = min(logTimestamp),
    TimePlayedSubtotal = difftime(max(logTimestamp), min(logTimestamp), units = "secs")) %>%
  mutate(
    TimePlayedSubtotal = ifelse(is.infinite(TimePlayedSubtotal), 0, TimePlayedSubtotal)) %>%
  group_by(userID, sesID) %>%
  summarize(
    sessTimePlayed_Sec = as.character(sum(TimePlayedSubtotal, na.rm = TRUE)) 
  )%>%
  as.data.frame()

HitCount_Sess <- AYCET_40034 %>% #Hits
  group_by(userID, sesID)  %>%
  filter(hitType == "HIT") %>%
  summarize(HITS = n(), 
            sessAvgHitRT = mean(reactionTime, na.rm = TRUE), 
            sessSDHitRT = sd(reactionTime, na.rm = TRUE)) %>%
  as.data.frame()

#Calculate misses per user and session
MissCount_Sess <- AYCET_40034 %>% #Misses
  group_by(userID, sesID)  %>%
  filter(hitType == "MISSED") %>%
  summarize(MISSED = n(), 
            sessAvgMissRT = mean(reactionTime, na.rm = TRUE), 
            sessSDMissRT = sd(reactionTime, na.rm = TRUE)) %>%
  as.data.frame()

#Calculate wrongs per user and session
WrongCount_Sess <- AYCET_40034 %>% #Wrongs
  group_by(userID, sesID)  %>%
  filter(hitType == "WRONG") %>%
  summarize(WRONG = n(), 
            sessAvgWrongRT = mean(reactionTime, na.rm = TRUE), 
            sessSDWrongRT = sd(reactionTime, na.rm = TRUE)) %>%
  as.data.frame()

###Join hit metrics for user level info per session
aggregatedSessList <- list(AYCET_fastestHits, AYCET_lastSes, AYCET_highestLevel, AYCET_userTrials, AYCET_levelsTotal_sess, AYCET_totalTimePlayed, sessionLevelInfo, HitCount_Sess, MissCount_Sess, WrongCount_Sess)
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

#Add AYCE to column names
colnames(aggregateSess)[4:26] <- paste("AYCE", colnames(aggregateSess[, c(4:26)]), sep = "_")


#Spread out long format to wide, for as many sessions as needed
aggregateSessWide <- aggregateSess %>%
  group_by(userID) %>%
  #filter(userID != 221) %>%
  arrange(userID, AYCE_latestTime) %>%
  mutate(sesCount = seq(1, n()), 
         accessCode = "AHF18", #TK
         AYCE_earliestTime = as.character(AYCE_earliestTime), 
         AYCE_latestTime = as.character(AYCE_latestTime)) %>% #Had to change variable types because otherwise it gets converted to a numeric value in the gather
  select(-c(sesID)) %>% #Don't actually need AYCE_max, AYCE_min, can take them out here
  gather(temp, ayce_items, c(AYCE_sessFastestRT,     AYCE_earliestTime, AYCE_latestTime, AYCE_sessionHighestLevel,  
                             AYCE_sessTrialCount, AYCE_userHighestLevel, AYCE_sessionHighestTrialCount, AYCE_highestLevelPct,            
                             AYCE_nLevels,      AYCE_sessTimePlayed_Sec, 
                             AYCE_HITS,               AYCE_sessAvgHitRT,     AYCE_sessSDHitRT,       
                             AYCE_MISSED,             AYCE_sessAvgMissRT,    AYCE_sessSDMissRT,      
                             AYCE_WRONG,              AYCE_sessAvgWrongRT,   AYCE_sessSDWrongRT,     
                             AYCE_PercentCorrect,     
                             AYCE_HitRate,            AYCE_FARate,               AYCE_DPrime)) %>%
  unite(temp2, temp, sesCount, sep = "_") %>%
  group_by(userID) %>%
  spread(temp2, ayce_items) %>%
  as.data.frame()    

write.csv(aggregateSessWide, "AHF18_AYCE_SessLevel_Output.csv", row.names = FALSE)
