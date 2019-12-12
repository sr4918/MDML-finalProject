#This is a test script file to read selected variables from pretest and post test folders containg DCCS and Flanker
#data

library(tidyverse)
library(data.table)
library(dplyr)
library(corr)
library(GGally)

#This uses the common file path and naming convention of data files to pull 
#in relevant files and relevant variables
  common_path = "C:/Users/samee/Dropbox/NYU-PhD/3. Fall 2019/Messy Data and ML/Final Project/data/Posttest"
  common_path2 = "C:/Users/samee/Dropbox/NYU-PhD/3. Fall 2019/Messy Data and ML/Final Project/data/Pretest"

#list of userID's that were not real players or did not complete the experiment
  badUsers <- c(115,   
                132,
                17214:17228,
                17191:17213,
                17348,
                17386,
                17393,
                17394,
                18128,
                18129,
                18536)

#1 Collecting post test data FOR DCCS
          # Creates a list of path names to get files from
          folders<- list.files(common_path, pattern = "*_20181221", full.names=TRUE)
          folders<-file.path(folders)
          subfolder = list.files(folders, pattern = "DCCS", full.names=TRUE)

         #creates a list of all relevant files in those folders
          files<-paste0(subfolder, "/",list.files(subfolder, pattern = "output_nih_dccs.csv"))
        
          
          #extracts relevant variables from these files and row binds them
            df_posttest_DCCS <- files %>% 
              map(function(x) {
                fread(x,select=c("id","userID", "accessCode", "task","nihScore", "nihAccuracy", "nihRTScore", "dataTimestamp"))
                }) %>%
              reduce(rbind, fill = T)
    
            names(df_posttest_DCCS)<-c("id","userID", "accessCode", "task","post_nihScore", "post_nihAccuracy", "post_nihRTScore", "dateTime")
            
          #remove duplicate attempts of post test with just the first one
          df_posttest_DCCS<-df_posttest_DCCS%>%group_by(userID)%>%arrange(dateTime)%>%slice(1)
          write.csv(df_posttest_DCCS,"output/posttestDCCS.csv", row.names = F)
            
#2a. Collecting Pretest Data  for DCCS followed same steps as above
      folders2<- list.files(common_path2, pattern = "*_20181221", full.names=TRUE)
      folders2<-file.path(folders2)
      subfolder2 = list.files(folders2, pattern = "DCCS", full.names=TRUE)
      
      files2<-paste0(subfolder2, "/",list.files(subfolder2, pattern = "output_nih_dccs.csv"))
      
        df_pretest_DCCS <- files2 %>% 
        map(function(x) {
          fread(x,select=c("id", "userID","accessCode", "task","nihScore", "nihAccuracy", "nihRTScore","dataTimestamp"))
        }) %>%
        reduce(rbind, fill= T)
        names(df_pretest_DCCS)<-c("id","userID", "accessCode", "task","pre_nihScore", "pre_nihAccuracy", "pre_nihRTScore","dateTime")
        df_pretest_DCCS<-df_pretest_DCCS%>%group_by(userID)%>%arrange(dateTime)%>%slice(1)
        
        write.csv(df_pretest_DCCS,"output/pretestDCCS.csv", row.names = F)

#3a. inner joining pretest and post test FOR DCCS
        #join pre test and post test data for evey user
      df_posttest_DCCS<-filter(df_posttest_DCCS, !is.na(userID))
      combined_file<-inner_join(df_pretest_DCCS, df_posttest_DCCS, by = "userID")
      combined_file<-filter(combined_file, !userID %in% badUsers)
      write.csv(combined_file,"output/DCCS_PrePostCombinedData.csv", row.names = F)


