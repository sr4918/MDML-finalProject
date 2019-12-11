#data
library(tidyverse)
library(data.table)
library(dplyr)
library(corr)
library(GGally)
library(ggpubr)


#4 Analysis:
# Co relation matrix between pre_nihScore against post_nihSore, pre_nihAccuracy against post_nihAccuracy,
#pre_nihRTScore against post nihRTScoo_re

#DCCS:
      data<-read.csv("output/DCCS_PrePostCombinedData.csv")
     
      cor_data_DCCS<-select(data,pre_nihScore, pre_nihAccuracy,pre_nihRTScore ,post_nihScore, post_nihAccuracy, post_nihRTScore)
      names(cor_data_DCCS)<-c("pre_TScore", "pre_Acc", "pre_ReactionT", "post_TScore", "post_Acc", "post_ReactionT")
      
      #pre against post scores co relation
      res<-cor(cor_data_DCCS[,1:3],cor_data_DCCS[,4:6])
      res
      write.csv(res,"output/CorelationValues_DCCS.csv", row.names = T)

      #multiple scatter plots visualization
      DCCS_scatterplot<- ggpairs(cor_data_DCCS,title="Correlation Plot: Pre and Post Test Scores", aes(fill = "pink"))
      ggsave("images/DCCS_prepost_scatterplot.png",DCCS_scatterplot)

#FLANKER
      data2<-read.csv("output/combine_pre-post_FLANKER.csv")
      
      cor_data_FLANKER<-select(data2,pre_nihScore, pre_nihAccuracy,pre_nihRTScore ,post_nihScore, post_nihAccuracy, post_nihRTScore)
      names(cor_data_FLANKER)<-c("pre_TScore", "pre_Acc", "pre_ReactionT", "post_TScore", "post_Acc", "post_ReactionT")
      res2<-cor(cor_data_FLANKER[,1:3],cor_data_FLANKER[,4:6])
      res2
      write.csv(res2,"output/Corelation_FLANKER.csv", row.names = T)
      FLANKER_scatterplot<- pairs(cor_data_FLANKER,title="FLANKER Pre and Post Test Scores", aes(fill = "blue"))
      ggsave("output/FLANKER_prepost_scatterplot.png",FLANKER_scatterplot)


### DCCS PLOTS #477 rows of data
      DCCS<-data %>% select(userID, pre_nihScore, post_nihScore, pre_nihAccuracy, post_nihAccuracy,pre_nihRTScore,post_nihRTScore,dateTime.x,dateTime.y) %>%
        filter(complete.cases(.)) %>%
        mutate(DiffScore = post_nihScore - pre_nihScore,
               DiffAccuracy =post_nihAccuracy- pre_nihAccuracy,
               DiffRT=post_nihRTScore- pre_nihRTScore)
     
      #Improvers in TotalScore, Accuracy, RT time respectively
      Improvers1<- select(filter(DCCS, DiffScore >0), userID, DiffScore)
      summary(Improvers1$DiffScore)
      p1<-ggplot(Improvers1, aes(x=DiffScore)) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.1, fill="#FF6666") 
      Improvers2<- select(filter(DCCS, DiffAccuracy >0), userID,DiffAccuracy)
      summary(Improvers2$DiffAccuracy)
     q1<- ggplot(Improvers2, aes(x=DiffAccuracy)) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.1, fill="#FF6666")       
      Improvers3<- select(filter(DCCS, DiffRT >0), userID, DiffRT)
      summary(Improvers3$DiffRT)
     r1<- ggplot(Improvers3, aes(x=DiffRT)) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.1, fill="#FF6666") 

     g<-ggarrange(p1, q1, r1 , 
                labels = c("Improvers Diff in Score", "Diff in Accuracy", "Diff in ReactionTime"),
                ncol = 3, nrow = 1)
      ggsave( "images/ImproversPlotsDCCS.png",g)

      DCCS_TotalScore<-DCCS%>%select(userID, pre_nihScore, post_nihScore, DiffScore)%>% gather(PrePost, Score, pre_nihScore, post_nihScore) 
      DCCS_AccuracyScore<-DCCS%>%select(userID, pre_nihAccuracy, post_nihAccuracy, DiffAccuracy)%>%gather(PrePost, Score, pre_nihAccuracy, post_nihAccuracy) 
      DCCS_RTScore<-DCCS%>%select(userID, pre_nihRTScore, post_nihRTScore, DiffRT)%>%gather(PrePost, Score, pre_nihRTScore, post_nihRTScore) 
      
      DCCS_TotalScore$PrePost <- factor( DCCS_TotalScore$PrePost, levels = c("pre_nihScore", "post_nihScore"), labels = c("Before", "After"))
      DCCS_AccuracyScore$PrePost <- factor( DCCS_AccuracyScore$PrePost, levels = c("pre_nihAccuracy", "post_nihAccuracy"), labels = c("Before", "After"))
      DCCS_RTScore$PrePost <- factor( DCCS_RTScore$PrePost, levels = c("pre_nihRTScore", "post_nihRTScore"), labels = c("Before", "After"))
     
      p<- ggplot(aes(x = PrePost, y = Score,color = DiffScore, group = userID), data = DCCS_TotalScore) +geom_line()
      q<- ggplot(aes(x = PrePost, y = Score,color = DiffAccuracy, group = userID), data = DCCS_AccuracyScore) +geom_line()
      r<-      ggplot(aes(x = PrePost, y = Score,color = DiffRT, group = userID), data = DCCS_RTScore) +geom_line()

      t2<-ggarrange(p, q, r + rremove("x.text"), 
                labels = c("Score", "Accuracy", "ReactionTime"),
                ncol = 2, nrow = 2)
       
      ggsave("images/PrePostPlots.png", t2)
      
      #see how many get a score mre than 4 on DCCS
      DCCS$ImproverScore<-ifelse(DCCS$DiffScore>0, 1, 0)
      DCCS$ImproverAccuracy<-ifelse(DCCS$DiffAccuracy>0, 1, 0)
      DCCS$ImproverRT<-ifelse(DCCS$DiffRT >0, 1, 0)
      DCCS$AllImprove<-ifelse(DCCS$ImproverScore==1 &DCCS$ImproverAccuracy==1&DCCS$ImproverRT==1, 1, 0)
      DCCS$ImprovedPostScoreGT7<-ifelse(DCCS$post_nihScore>7.2, 1, 0)
     
      write.csv(DCCS, "data/ALL_DCCS_data.csv", row.names = F)
      
      
      #FinalLabels<-select(DCCS, userID, ImproverScore, ImproverAccuracy,ImproverRT, AllImprove, postScore)
      #write.csv(FinalLabels, "data/UserLabels.csv", row.names = F)
      #sum(FinalLabels$postScore==T)
      