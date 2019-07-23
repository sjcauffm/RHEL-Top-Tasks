library(tidyverse)
library(googlesheets)

dir <- setwd("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/RHEL Top Tasks/Phase 4 - Identifying Top Tasks/Post Summit 2019/Red Hat SubReddit/RHEL-Top-Tasks")

##importing data 
gs_auth()
import <- gs_title("rhel_top_tasks_reddit")
raw_data <- gs_read_csv(import)

#### Adapting code from previous script. Survey structure is slightly different from previous surveys
data <- raw_data[3:47,]
