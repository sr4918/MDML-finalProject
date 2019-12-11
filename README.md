#MDML Final Project 2019
##Associating Features of Game Play with Improved Executive Function Using Lasso Regression
###Corinne Brenner & Sameen Reza

##Getting Started
This repository contains the code used to import and clean a complex dataset, aggregated data which we are able to share, the code to analyse aggregated data, and code to visualize elements of the data and analysis. 

##Data sources
This repository does not contain the raw data, but you can review the data assembly, preprocessing, and many cleaning steps in the 'script' folder. 
There are 2 sources of messy data: 
 * A set of game log files for a digital game that is used to train executive function ("All You Can E.T., abbreviated AYCET; ), and 
 * The output for an executive function task (Dimensional Change Card Sort task, abbreviated DCCS) used to measure shifting skills before and after playing the game.
 
This data is messy both in itself, and also since the full dataset is stored in a complex file structure representing different conditions of a study and steps in the post-experimental data processing workflow.

Additional data sources were used to create the features and clean the data. The 'data' folder contains:
* testAccounts.csv: A list of known 'bad' users, like test accounts, or accounts which exist due to participant error. 
* FA2018_Intervention_Complexity.csv: A table of the levels of the game and the complexity of each level, on a scale from 1 (easiest) to 7 (most difficult). 

##Preprocessing
The 'script' folder contains code for the data assembly, preprocessing, and many cleaning steps for the AYCET and DCCS data.

The file "MDML_reading_gameplay.R" contains the steps used to import, restructure, and clean the data from the AYCET game logs. Once the raw data was important and restructured, the AYCET gameplay data was manipulated to create a variety of features for modeling. Since this process required many steps, we exported aggregated data in the 'output' folder. 

SAMEEN TO EDIT: The file "PrePostTest_Analysis.R" contains the steps used to import, explore, and clean the data from the DCCS task logs, and combine the pretest results with the posttest results into a wide format file. Additional variables representing outcomes were also calculated. 

The DCCS task has a complex scoring system developed by the NIH. To investigate our research questions, we needed to do some preliminary, exploratory analyses of the DCCS scores. Data created in these preliminary analyses have been exported to the 'output' folder.

We exported processed data to the 'data' folder.  
AYCET_gameplay_aggregated.csv contains the preprocessed AYCET data. 
ALL_DCCS_data.csv contains the preprocessed DCCS data.

Images which helped us understand the distributions, correlations, or other comparisons of these aggregated data were exported to the 'images' folder.


##Images

##Lasso Regression Analyses



