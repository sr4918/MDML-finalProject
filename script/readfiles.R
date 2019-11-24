#This is a test script file to read selected variables from pretest and post test folders containg DCCS and Flanker
#data
library(tidyverse)
library(data.table)
library(dplyr)
#setwd("C:/Users/samee/Dropbox/NYU-PhD/3. Fall 2019/Messy Data and ML/Final Project/MDML-finalProject")
#setwd("C:/Users/samee/Dropbox/NYU-PhD/3. Fall 2019/Messy Data and ML/Final Project/data/Posttest/FA18UCP1_20181221/DCCS")
common_path = "C:/Users/samee/Dropbox/NYU-PhD/3. Fall 2019/Messy Data and ML/Final Project/data/Posttest"
common_path2 = "C:/Users/samee/Dropbox/NYU-PhD/3. Fall 2019/Messy Data and ML/Final Project/data/Pretest"

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
                fread(x,select=c("id","userID", "accessCode", "task","nihScore", "nihAccuracy", "nihRTScore"))
              }) %>%
              reduce(rbind, fill = T)
    
            names(df_posttest_DCCS)<-c("id","userID", "accessCode", "task","post_nihScore", "post_nihAccuracy", "post_nihRTScore")
            write.csv(df_posttest_DCCS,"output/combine_posttestDCCS.csv", row.names = F)
            
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
#--------------------            
#2a. Collecting Pretest Data  for DCCS
      folders2<- list.files(common_path2, pattern = "*_20181221", full.names=TRUE)
      folders2<-file.path(folders2)
      subfolder2 = list.files(folders2, pattern = "DCCS", full.names=TRUE)
      
      files2<-paste0(subfolder2, "/",list.files(subfolder2, pattern = "output_nih_dccs.csv"))
      
      #loop over every file in files:
        df_pretest_DCCS <- files2 %>% 
        map(function(x) {
          fread(x,select=c("id", "userID","accessCode", "task","nihScore", "nihAccuracy", "nihRTScore"))
        }) %>%
        reduce(rbind, fill= T)
        names(df_pretest_DCCS)<-c("id","userID", "accessCode", "task","pre_nihScore", "pre_nihAccuracy", "pre_nihRTScore")
              write.csv(df_pretest_DCCS,"output/combine_pretestDCCS.csv", row.names = F)
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
#------------------------------------------------
#3a. inner joining pretest and post test FOR DCCS
      df_posttest_DCCS<-filter(df_posttest_DCCS, !is.na(userID))
      combined_file<-inner_join(df_pretest_DCCS, df_posttest_DCCS, by = "userID")
      write.csv(combined_file,"output/combine_pre-post_DCCS.csv", row.names = F)

#3b. inner joining pretest and post test FOR FLANKER
      df_posttest_FLANKER<-filter(df_posttest_FLANKER, !is.na(userID))
      combined_file_FLANKER<-inner_join(df_pretest_FLANKER, df_posttest_FLANKER, by = "userID")
      write.csv(combined_file_FLANKER,"output/combine_pre-post_FLANKER.csv", row.names = F)
#-------------------------------     
      
#4 Analysis:
# Co relation matrix between pre_nihScore against post_nihSore, pre_nihAccuracy against post_nihAccuracy,
      #pre_nihRTScore against post nihRTScore
      #DCCS:
      cor_data_DCCS<-select(combined_file,pre_nihScore, pre_nihAccuracy,pre_nihRTScore ,post_nihScore, post_nihAccuracy, post_nihRTScore)
      res<-cor(cor_data_DCCS[,1:3],cor_data_DCCS[,4:6])
      res
      write.csv(res,"output/Corelation_DCCS.csv", row.names = T)
      
       #FLANKER
      cor_data_FLANKER<-select(combined_file_FLANKER,pre_nihScore, pre_nihAccuracy,pre_nihRTScore ,post_nihScore, post_nihAccuracy, post_nihRTScore)
      res2<-cor(cor_data_FLANKER[,1:3],cor_data_FLANKER[,4:6])
      res2
      write.csv(res2,"output/Corelation_FLANKER.csv", row.names = T)
      
      ################################### FLANKER ################################################################