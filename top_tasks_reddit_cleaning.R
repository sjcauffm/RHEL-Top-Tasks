library(tidyverse)
library(googlesheets)

dir <- setwd("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/RHEL Top Tasks/Phase 4 - Identifying Top Tasks/Post Summit 2019/Red Hat SubReddit/RHEL-Top-Tasks")

##importing data 
gs_auth()
import <- gs_title("rhel_top_tasks_reddit")
raw_data <- gs_read_csv(import)

#### Adapting code from previous script. Survey structure is slightly different from previous surveys
data <- raw_data[3:47,]
data_trim <- data[,c(9, 18:484)]

names(data_trim)[1] <- c("response_id")

diff <- 

area_names <- c("security_", "storage_", "network_", "virtual_", "identity_", 
                "entitle_","sysm_", "errata_pick", "migrate_")

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
clean <- do.call(rbind.data.frame, unclean)
names(clean) <- c("response_id", "question", "pick5", "rank5")
clean_data <- clean