#Graphing pre vs post

#I used your dataset
t_data <- read.csv("FA17Intervention_20180424.csv", header = TRUE)

#Reformatting
t_long_dccs <- t_data %>%
  select(userID, Game, DCCSPre_nihScore, DCCSPost_nihScore) %>%
  filter(complete.cases(.)) %>%
  mutate(Diff = DCCSPost_nihScore - DCCSPre_nihScore) %>%
  gather(PrePost, DCCSScore, DCCSPre_nihScore, DCCSPost_nihScore) 

#Change the features of the factor
t_long_dccs$PrePost <- factor(t_long_dccs$PrePost, levels = c("DCCSPre_nihScore", "DCCSPost_nihScore"), labels = c("Before", "After"))

#Plot it
#Use the DCCS scores as the Y value, and Before and After ax the X value; govern the color by the difference between scores, and connect lines based on userID; label using userID
increase_graph_dccs <- ggplot(aes(x = PrePost, y = DCCSScore, color = Diff, group = userID, label = userID), data = t_long_dccs) +
  #Use points for data
  geom_point() +
  #Color everything on a gradient form red to green
  scale_colour_gradient(low = "#D55E00", high = "#009E73") +
  #Connect the dots with lines
  geom_line() + 
  #Label the points where the After score is less than 4; move them over to the right side
  geom_text(data = t_long_dccs[ which(t_long_dccs$PrePost == "After" & t_long_dccs$DCCSScore < 4), ], nudge_x = .2) +
  #Split up games into separate facets
  facet_grid(. ~ Game) +
  #Increase the text size
  theme( axis.text = element_text(size = 20), axis.title = element_text(size = 24), strip.text = element_text(size = 20))

#Save it
ggsave("increase_graph_dccs.png")