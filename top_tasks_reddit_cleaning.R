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

diff <- grep("difficulty", names(data_trim))
data_trim2 <- data_trim[,-diff]

area_names <- c("security_", "storage_", "network_", "virtual_", "identity_", 
                "entitle_","sysm_", "errata_pick", "migrate_")

top_tasks_by_section <- function (area){
  area_grep <- grep(area, names(data_trim2))
  area_data <- data_trim2[,c(1, area_grep)]
  area <- gather(area_data, type, value, -response_id)
}

pick5 <- grep("pick5", clean$type)
rank5 <- grep("rank5", clean$type)

picks <- clean[pick5,]
picks2 <- x[complete.cases(x$value),]
ranks <- clean[rank5,]
ranks2 <- y[complete.cases(y$value),] 

temp <- picks2$response_id %in% ranks2$response_id
picks2$complete <- temp

picks2 <- picks2[which(picks2$complete == TRUE),]
temp2 <- y2$response_id %in% x2$response_id

x <- strsplit(picks2$type, "_pick5_")
x <- do.call(rbind.data.frame, x)
names(x) <- c("area", "num")
picks2$question_id <- paste0(x$area,sep = "_", x$num)

y <- strsplit(ranks2$type, "_rank5_")
y <- do.call(rbind.data.frame, y)
names(y) <- c("area", "num")
ranks2$question_id <- paste0(y$area, sep = "_", y$num)

bleh <- picks2$question_id %in% ranks2$question_id
picks2$bleh <- bleh

picks2 <- picks2[which(picks2$bleh == TRUE),]

picks2 <- picks2[,c(1:3, 5)]

combined <- full_join(picks2, ranks2, by = c("response_id", "question_id"))
names(combined) <- 






clean_data <- clean