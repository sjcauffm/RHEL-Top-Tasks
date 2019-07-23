library(tidyr)
library(dplyr)
library(googlesheets)
library(reshape2)


####Cleaning post summit RHEL top tasks raw data

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
names(data_trim) <- c(data_trim[1,])

##Step 3: Get rid of extra rows
data_trim <- data_trim[3:40,]
names(data_trim)[1] <- c("response_id")

####subsetting by area
security_grep <- grep("_security_", names(data_trim))
data_security <- data_trim[,c(1, security_grep)]
security <- gather(data_security, type, value, -response_id)
security_text <- grep("TEXT", security$type)
security_rank <- security[security_text,]
security_task <- security[-security_text,]

security_data <- cbind(security_task, security_rank)
security_data <- security_data[,c(1,2,3,6)]
names(security_data) <- c("response_id", "question", "pick5", "rank5")

##file management
files_grep <- grep("filesystem and stor", names(data_trim))
data_files <- data_trim[,c(1, files_grep)]
files <- gather(data_files, type, value, -response_id)
files_text <- grep("TEXT", files$type)
files_rank <- files[files_text,]
files_task <- files[-files_text,]

files_data <- cbind(files_task, files_rank)
files_data <- files_data[,c(1,2,3,6)]
names(files_data) <- c("response_id", "question", "pick5", "rank5")

##Neteorking
network_grep <- grep("networking_", names(data_trim))
data_network <- data_trim[,c(1, network_grep)]
network <- gather(data_network, type, value, -response_id)
network_text <- grep("TEXT", network$type)
network_rank <- network[network_text,]
network_task <- network[-network_text,]

network_data <- cbind(network_task, network_rank)
network_data <- network_data[,c(1,2,3,6)]
names(network_data) <- c("response_id", "question", "pick5", "rank5")


##virtualization
virtual_grep <- grep("virtualization", names(data_trim))
data_virtual <- data_trim[,c(1, virtual_grep)]
virtual <- gather(data_virtual, type, value, -response_id)
virtual_text <- grep("TEXT", virtual$type)
virtual_rank <- virtual[virtual_text,]
virtual_task <- virtual[-virtual_text,]

virtual_data <- cbind(virtual_task, virtual_rank)
virtual_data <- virtual_data[,c(1,2,3,6)]
names(virtual_data) <- c("response_id", "question", "pick5", "rank5")

##Identity management
identity_grep <- grep("identity", names(data_trim))
data_identity <- data_trim[,c(1, identity_grep)]
identity <- gather(data_identity, type, value, -response_id)
identity_text <- grep("TEXT", identity$type)
identity_rank <- identity[identity_text,]
identity_task <- identity[-identity_text,]

identity_data <- cbind(identity_task, identity_rank)
identity_data <- identity_data[,c(1,2,3,6)]
names(identity_data) <- c("response_id", "question", "pick5", "rank5")

##entitlement
entitle_grep <- grep("_entitlements_", names(data_trim))
data_entitle <- data_trim[,c(1, entitle_grep)]
entitle <- gather(data_entitle, type, value, -response_id)
entitle_text <- grep("TEXT", entitle$type)
entitle_rank <- entitle[entitle_text,]
entitle_task <- entitle[-entitle_text,]

entitle_data <- cbind(entitle_task, entitle_rank)
entitle_data <- entitle_data[,c(1,2,3,6)]
names(entitle_data) <- c("response_id", "question", "pick5", "rank5")

##system management
sys_grep <- grep("system management", names(data_trim))
data_sys <- data_trim[,c(1, sys_grep)]
sys <- gather(data_sys, type, value, -response_id)
sys_text <- grep("TEXT", sys$type)
sys_rank <- sys[sys_text,]
sys_task <- sys[-sys_text,]

sys_data <- cbind(sys_task, sys_rank)
sys_data <- sys_data[,c(1,2,3,6)]
names(sys_data) <- c("response_id", "question", "pick5", "rank5")

##Errate
errata_grep <- grep("errata update", names(data_trim))
data_errata <- data_trim[,c(1, errata_grep)]
errata <- gather(data_errata, type, value, -response_id)
errata_text <- grep("TEXT", errata$type)
errata_rank <- errata[errata_text,]
errata_task <- errata[-errata_text,]
errata_rank_test <- errata_rank[complete.cases(errata_rank$value),]
errata_task_test <- errata_task[complete.cases(errata_task$value),]

errata_data <- cbind(errata_task_test, errata_rank_test)
errata_data <- errata_data[,c(1,2,3,6)]
names(errata_data) <- c("response_id", "question", "pick5", "rank5")

##Migration
migrate_grep <- grep("_migration_", names(data_trim))
data_migrate <- data_trim[,c(1, migrate_grep)]
migrate <- gather(data_migrate, type, value, -response_id)
migrate_text <- grep("TEXT", migrate$type)
migrate_rank <- migrate[migrate_text,]
migrate_task <- migrate[-migrate_text,]

migrate_data <- cbind(migrate_task, migrate_rank)
migrate_data <- migrate_data[,c(1,2,3,6)]
names(migrate_data) <- c("response_id", "question", "pick5", "rank5")


###Binding everything together
unclean <- rbind(security_data, files_data, virtual_data, identity_data, entitle_data, sys_data, network_data,
                               errata_data, migrate_data)

clean <- unclean[complete.cases(unclean$pick5),]

area_split <- strsplit(clean$question, "_")
area_names <- do.call(rbind.data.frame, area_split)

clean$area <- area_names[,2]


###test_5 <- full_join(test_2, test_3, by = c("response_id", "question_num"))
##test_7 <- test_5[complete.cases(test_5$task),]
#test_8 <- test_7[complete.cases(test_7$rank),]

#area_name <- strsplit(test_8$question_num, "_")
#area_name <- do.call(rbind.data.frame, area_name)

#test_8$area <- area_name[,2]



###need to save now. 

write.csv(clean, "post_summit_top_tasks(use this one).csv")
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
