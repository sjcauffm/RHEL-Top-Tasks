library(tidyverse)
library(ggplot2)
library(googlesheets)
library(ggthemes)

#Point R to the right working directory
dir <- setwd("~/Google Drive File Stream/My Drive/UXD-Share/Usability and User Research/Studies 2019/RHEL Top Tasks")

####Graphing Scripts for top tasks####

###Need to import the data
gs_auth()
import <- gs_title("combined_top_tasks")

data <- gs_read_csv(import)
data$pick5 <- as.factor(data$pick5)

test <- table(data$pick5, data$primary_area)
test_2 <- as.data.frame(unlist(test))
test_2 <- test_2[which(test_2$Freq != 0),]
test_2$prop <- test_2$Freq/sum(test_2$Freq)
names(test_2) <- c("pick5", "area","frequency", "proportion")

##### Need to calculate confidence intervals
mean_freq <- mean(data_2$frequency)
sd_freq <- sd(data_2$frequency)

mean_prop <- mean(data_2$proportion)
sd_prop <- sd(data_2$proportion)

#### caluclating adjusted proportions
test_2$adj_prop <- (test_2$frequency + 2) / (sum(test_2$frequency) + 4) ###get adjusted sample proportion
test_2$x <- test_2$adj_prop * (1 - test_2$adj_prop) ###standard error calculation - step 1
test_2$y <- test_2$x/(sum(test_2$frequency) + 4) ###standard error calculation - step 2
test_2$z <- sqrt(test_2$y) ###standard error calculation - step 3
test_2$margin_error <- test_2$z * 2 ###gets margin of error
test_2$upper_ci <- test_2$proportion + test_2$margin_error ###calculate upper ci
test_2$lower_ci <- test_2$proportion - test_2$margin_error###caluculate lower ci

test_3 <- test_2[,c(1:4, 9:11)] ###gets us the columns we want to merge back into the data frame

test_4 <- test_3[which(test_3$frequency > 3),]


## Need to trasnform data a bit to get into a more usable format for R
### aggreate to a new data frame where each task is listed along with the proportion of votes it got

##Plotting data

tasks <- ggplot(test_4, aes(x = reorder(pick5, -proportion), 
                            y = proportion)) +
  theme_tufte() +
  theme(axis.text.x=element_text(angle=60,hjust=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "left") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.1)) +
  geom_bar(stat = "identity", aes(fill = test_4$area)) +
  geom_errorbar(ymin = test_4$proportion - test_4$margin_error, 
                ymax = test_4$proportion + test_4$margin_error, 
                width = .5) +
  labs(x = "Top Tasks in RHEL", y = "Proportion", fill = "Task Category")

ggsave("top_tasks_graph.png", tasks, height = 12, width = 20)









  