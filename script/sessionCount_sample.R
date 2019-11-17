sessionCountDF <- gameplayEdit %>%
  arrange(userID, date, sesID, logTimestamp, gsUserID, gameLevelShort, gameLevel, alienID) %>%
  group_by(userID, date, sesID, gsUserID) %>%
  summarize(timeFix = min(logTimestamp)) %>%
  arrange(userID, date, sesID, timeFix) %>%
  ungroup() %>%
  group_by(userID) %>%
  mutate(sesCountAll = seq(1:n()), #Counts every single session defined by sessionID/gsUserID
         sesCount = dense_rank(date)) %>% #Combines sessionIDs by date
  ungroup() %>%
  as.data.frame()

gameplayEdit <- left_join(gameplayEdit, sessionCountDF[, c("userID", "date", "sesID", "gsUserID", "sesCount", "sesCountAll")], by = c("userID", "date", "sesID", "gsUserID"))
