#This is a test script file to read selected variables from pretest and post test folders containg DCCS and Flanker
#data

library(tidyverse)
library(data.table)
library(dplyr)
library(corr)
library(GGally)
#setwd("C:/Users/samee/Dropbox/NYU-PhD/3. Fall 2019/Messy Data and ML/Final Project/MDML-finalProject")
#setwd("C:/Users/samee/Dropbox/NYU-PhD/3. Fall 2019/Messy Data and ML/Final Project/data/Posttest/FA18UCP1_20181221/DCCS")
common_path = "C:/Users/samee/Dropbox/NYU-PhD/3. Fall 2019/Messy Data and ML/Final Project/data/Posttest"
common_path2 = "C:/Users/samee/Dropbox/NYU-PhD/3. Fall 2019/Messy Data and ML/Final Project/data/Pretest"
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

#1a Collecting post test data FOR DCCS
      #hardcoding paths of files that were different
          folders<- list.files(common_path, pattern = "*_20181221", full.names=TRUE)
          folders<-file.path(folders)
          subfolder = list.files(folders, pattern = "DCCS", full.names=TRUE)
      #extracting. csv files from each folder
          files<-paste0(subfolder, "/",list.files(subfolder, pattern = "output_nih_dccs.csv"))
          
          
      #combining files and selected variables
            df_posttest_DCCS <- files %>% 
              map(function(x) {
                fread(x,select=c("id","userID", "accessCode", "task","nihScore", "nihAccuracy", "nihRTScore", "dataTimestamp"))
                }) %>%
              reduce(rbind, fill = T)
    
            names(df_posttest_DCCS)<-c("id","userID", "accessCode", "task","post_nihScore", "post_nihAccuracy", "post_nihRTScore", "dateTime")
            
            #remove duplicate attempts of post test with just the first one
            df_posttest_DCCS<-df_posttest_DCCS%>%group_by(userID)%>%arrange(dateTime)%>%slice(1)
            write.csv(df_posttest_DCCS,"output/posttestDCCS.csv", row.names = F)
            
          #data check
            total<-0
            for(i in 1: length(files))
            {
              data<-fread(files[[i]])
              n<-nrow(data)
              total<-total+n
            }
            total==nrow(df_posttest_DCCS)
-------------
#1b: Collecting Posttest data for FLANKER
            subfolder3 = list.files(folders, pattern = "FLANKER", full.names=TRUE)
            #extracting. csv files from each folder
            files3<-paste0(subfolder3, "/",list.files(subfolder3, pattern = "output_nih_flanker.csv"))
            
            #combining files and selected variables
            df_posttest_FLANKER <- files3 %>% 
              map(function(x) {
                fread(x,select=c("id","userID", "accessCode", "task","nihScore", "nihAccuracy", "nihRTScore"))
              }) %>%
              reduce(rbind, fill = T)
            
            names(df_posttest_FLANKER)<-c("id","userID", "accessCode", "task","post_nihScore", "post_nihAccuracy", "post_nihRTScore")
            write.csv(df_posttest_FLANKER,"output/combine_posttest_FLANKER.csv", row.names = F)
#Data check
            total2<-0
            for(i in 1: length(files3))
            {
              data2<-fread(files3[[i]])
              n2<-nrow(data2)
              total2<-total2+n2
            }
            total2==nrow(df_posttest_FLANKER)
#--------------------            
#2a. Collecting Pretest Data  for DCCS
      folders2<- list.files(common_path2, pattern = "*_20181221", full.names=TRUE)
      folders2<-file.path(folders2)
      subfolder2 = list.files(folders2, pattern = "DCCS", full.names=TRUE)
      
      files2<-paste0(subfolder2, "/",list.files(subfolder2, pattern = "output_nih_dccs.csv"))
      
      #loop over every file in files:
        df_pretest_DCCS <- files2 %>% 
        map(function(x) {
          fread(x,select=c("id", "userID","accessCode", "task","nihScore", "nihAccuracy", "nihRTScore","dataTimestamp"))
        }) %>%
        reduce(rbind, fill= T)
        names(df_pretest_DCCS)<-c("id","userID", "accessCode", "task","pre_nihScore", "pre_nihAccuracy", "pre_nihRTScore","dateTime")
        df_pretest_DCCS<-df_pretest_DCCS%>%group_by(userID)%>%arrange(dateTime)%>%slice(1)
        
        write.csv(df_pretest_DCCS,"output/pretestDCCS.csv", row.names = F)
#Datacheck
        total3<-0
        for(i in 1: length(files2))
            {
              data3<-fread(files2[[i]])
              n3<-nrow(data3)
                total3<-total3+n3
              }
              total3==nrow(df_pretest_DCCS)
#-----------------------------------------
#2b. Collecting Pre Test data for FLANKER
              folders4<- list.files(common_path2, pattern = "*_20181221", full.names=TRUE)
              folders4<-file.path(folders4)
              subfolder4 = list.files(folders4, pattern = "FLANKER", full.names=TRUE)
              
              files4<-paste0(subfolder4, "/",list.files(subfolder4, pattern = "output_nih_flanker.csv"))
              
              #loop over every file in files:
              df_pretest_FLANKER <- files4 %>% 
                map(function(x) {
                  fread(x,select=c("id", "userID","accessCode", "task","nihScore", "nihAccuracy", "nihRTScore"))
                }) %>%
                reduce(rbind, fill= T)
              names(df_pretest_FLANKER)<-c("id","userID", "accessCode", "task","pre_nihScore", "pre_nihAccuracy", "pre_nihRTScore")
              write.csv(df_pretest_FLANKER,"output/combine_pretest_FLANKER.csv", row.names = F)
 #Data Check             
              total4<-0
              for(i in 1: length(files4))
              {
                data4<-fread(files4[[i]])
                n4<-nrow(data4)
                total4<-total4+n4
              }
              total4==nrow(df_pretest_FLANKER)
#------------------------------------------------
#3a. inner joining pretest and post test FOR DCCS
      df_posttest_DCCS<-filter(df_posttest_DCCS, !is.na(userID))
      combined_file<-inner_join(df_pretest_DCCS, df_posttest_DCCS, by = "userID")
      combined_file<-filter(combined_file, !userID %in% badUsers)
      write.csv(combined_file,"output/DCCS_PrePostCombinedData.csv", row.names = F)

#3b. inner joining pretest and post test FOR FLANKER
      df_posttest_FLANKER<-filter(df_posttest_FLANKER, !is.na(userID))
      combined_file_FLANKER<-inner_join(df_pretest_FLANKER, df_posttest_FLANKER, by = "userID")
      combined_file_FLANKER<-filter(combined_file_FLANKER, !userID %in% badUsers)
      write.csv(combined_file_FLANKER,"output/combine_pre-post_FLANKER.csv", row.names = F)
#-------------------------------     
      
