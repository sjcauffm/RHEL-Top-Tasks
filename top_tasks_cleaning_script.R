library(tidyverse)
library(googlesheets)

##Set the working directory to the folder with the raw data
dir <- setwd("~/Google Drive File Stream/My Drive/UXD-Share/Usability and User Research/Studies 2019/RHEL Top Tasks/Post Summit 2019")

##make sure google account is authorized
gs_auth()

##Import sheet and convert to .CSV so R can use it. 

import <- gs_title("rhel_post_summit_top_tasks")
raw_data <- gs_read_csv(import)


####Cleaning####
##Step 1: trim columns that are not needed
data_trim <- raw_data[,c(9,14:15, 18, 22:2734)]

##Step 2: Change Column names so that it becomes informative. 
##names(data_trim) <- c(data_trim[1,])

##Step 3: Get rid of extra rows
data_trim <- data_trim[3:40,]
names(data_trim)[1] <- c("response_id")

area_names <- c("security_", "filesystem and stor_", "networking_", "virtualization_", "identity management_", 
                "_entitlements_","system management_", "_errata update_", "_migration_")

top_tasks_by_section <- function (area){
 area_grep <- grep(area, names(data_trim))
  area_data <- data_trim[,c(1, area_grep)]
  area <- gather(area_data, type, value, -response_id)
}

top_tasks_by_section_split <- function (area){
  tasks <- data.frame(response_id = 0, type = 0, value = 0)
  ranks <- data.frame(response_id = 0, type = 0, value = 0)
  temp <- grep("TEXT", area$type)
  ranks <- rbind(ranks, area[temp,])
  ranks <- ranks[complete.cases(ranks$value),]
  tasks <- rbind(tasks, area[-temp,])
  tasks <- tasks[complete.cases(tasks$value),]
  combine <- cbind(tasks, ranks)
  combine <- combine[which(combine$type != 0), ]
  combine <- combine[, c(1:3,6)]
}

unclean <- lapply(area_names, top_tasks_by_section)
unclean_data <- lapply(unclean, top_tasks_by_section_split)
clean <- do.call(rbind.data.frame, unclean_data)
names(clean) <- c("response_id", "question", "pick5", "rank5")
clean_data <- clean

area_split <- strsplit(clean_data$question, "_")
area_names <- do.call(rbind.data.frame, area_split)

clean_data$area <- area_names[,2]

clean_data$response_id <- as.factor(clean_data$response_id) 

write.csv(clean_data, "top_task_data.csv")


test <- top_tasks_by_section("errata update")
test_2 <- top_tasks_by_section_split(test)




