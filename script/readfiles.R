library(tidyverse)
library(data.table)

setwd("C:/Users/samee/Dropbox/NYU-PhD/3. Fall 2019/Messy Data and ML/Create Project Data/Posttest/FA18UCP1_20181221/DCCS")
common_path = "C:/Users/samee/Dropbox/NYU-PhD/3. Fall 2019/Messy Data and ML/Create Project Data/Posttest"
common_path2 = "C:/Users/samee/Dropbox/NYU-PhD/3. Fall 2019/Messy Data and ML/Create Project Data/Pretest"

#primary_dirs = list.files(common_path);

    folders = list(
      file.path(paste0(common_path, '/FA18UCP1_20181221')),
      file.path(paste0(common_path, '/FA18UCpost_20181221')),
      file.path(paste0(common_path, '/JRPOF18_20181221'))
    )
    subfolder = unlist(sapply(folders, function(folder) {
      list.files(folder, pattern = "DCCS", full.names=TRUE)
    }))
#loop over every file in files:
  
    files<-unlist(sapply(subfolder, function(folder){paste0(folder,"/",list.files(folder, pattern = "*.csv"))}))

      df_posttest_DCCS <- files %>% 
        map(function(x) {
          fread(x,select=c("id", "accessCode", "task","nihScore", "nihAccuracy", "nihRTScore"))
        }) %>%
        reduce(rbind)
write.csv(df_posttest_DCCS,"combine_posttestDCCS.csv")
-------------
    #  folders2<- list(file.path((list.files(common_path2, pattern = "*_20181221", full.names=TRUE))))
      
      folders2<- list.files(common_path2, pattern = "*_20181221", full.names=TRUE)
      folders2<-file.path(folders2)
      subfolder2 = list.files(folders2, pattern = "DCCS", full.names=TRUE)
      #loop over every file in files:
      
      files2<-paste0(subfolder2, "/",list.files(subfolder2, pattern = "*.csv"))
     
      df_pretest_DCCS <- files2 %>% 
        map(function(x) {
          fread(x,select=c("id", "accessCode", "task","nihScore", "nihAccuracy", "nihRTScore"))
        }) %>%
        reduce(rbind)
write.csv(df_pretest_DCCS,"combine_pretestDCCS.csv")
      
  
