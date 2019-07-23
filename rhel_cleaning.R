library(tidyr)
library(dplyr)
library(googlesheets)

####authorizing user
gs_auth()

####Importing the data from google sheets
import <- gs_title("rhel_top_tasks")
data <- gs_read_csv(import)


####Cleaning Data####

##Step 1: Get rid of useless columns and fix column names to a useable format
data <- data[c(9,14:481)]
names(data)<- c(data[1,]) ###concerts the questions into the column names

data <- data[3:38,] ##trims the superfluous rows from the data


##Step 2: Convert data object into long format
###isolating variables 
pick5 <- grep("Select 5", names(data))
pick5_2 <-grep("Select 2", names(data)) 

pick5_3 <- append(pick5, pick5_2)

rank5 <- grep("Rank the following", names(data))
difficulty5 <- grep("difficult", names(data))

data_pick5 <- data[,c(1, 22, pick5_3)]
names(data_pick5)[c(1,2)] <- c("response_id", "primary_area")
pick5_data <- gather(data_pick5, type, value, -response_id, -primary_area)
names(pick5_data) <- c("response_id", "primary_area", "question_pick5", "pick5")
pick5_data <- pick5_data[complete.cases(pick5_data$pick5),]

data_area <- data[,c(22, pick5)]
names(data_area)[1] <- c("primary_area")
area_data <- gather(data_area, type, value, -primary_area)


data_rank5 <- data[,c(1, 22, rank5)]
names(data_rank5)[c(1,2)] <- c("response_id", "primary_area")
rank5_data <- gather(data_rank5, type, value, -response_id, -primary_area)
names(rank5_data) <- c("response_id","primary_area","question_rank5", "rank5")
rank5_data <- rank5_data[complete.cases(rank5_data$rank5),]

top_tasks <- cbind(pick5_data, rank5_data)
top_tasks <- top_tasks[,c(1:2,4,8)]
top_tasks <- top_tasks[!is.na(top_tasks$rank5),]
  
  
top_tasks$tasks <- as.factor(top_tasks$tasks) ###as.factor makes it easter for analysis

write.csv(top_tasks, "top_tasks_cleaned(use this one).csv")





  
  


         