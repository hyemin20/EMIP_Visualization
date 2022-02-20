###' ###########################################################################'
###' 
###' Project(project name): EM-IP
###' 
###' Category(stage in the project): data Cleaning
###' 
###' Task(specific task in the category): import and management
###' 
###' data(data source): `PISA 2012 CBA 038q01`
###' 
###' date: 2022.02.18
###' 
###' Author: Hyemin Park(`hyemin.park@snu.ac.kr`)
###' 
###'

###' ###########################################################################'
###' 
###' Basic settings
###' 
###' 

### Start with clean state
gc(); rm(list=ls())


### Set working directory and data directory
work_dir <- c("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/EMIP_Visualization")
data_dir <- file.path(work_dir, "datasets")
setwd(work_dir)


### Call libraries
library(haven); library(tidyverse); library(readr); library(writexl); library(dplyr)
library(rsconnect); library(bupaR); library(edeaR); library(processmapR); library(petrinetR); library(pm4py)



###' ###########################################################################'
###' 
###' Import data
###' 
###' 

### Set file path
original <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/EMIP_Visualization/datasets/original.csv", header = TRUE, stringsAsFactors = FALSE) %>% tibble()
scoring <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/EMIP_Visualization/datasets/scoring.csv", header = TRUE, stringsAsFactors = FALSE) %>% tibble()



###' ###########################################################################'
###' 
###' data with score
###' 
###' 

### original data ID padding & produce
original <- original[,-1]
head(original$cnt)
original$cnt <- str_pad(original$cnt, 3, pad = "0")
original$schoolid <- str_pad(original$schoolid, 7, pad = "0")
original$StIDStd <- str_pad(original$StIDStd, 5, pad = "0")
original[,12] <- paste0(original$cnt,original$schoolid,original$StIDStd)
colnames(original)[12] <- "ID"

original <- original %>%
  relocate("ID", .before = cnt)


### scoring data ID padding & produce
colnames(scoring)[1] <- 'cnt'
scoring$cnt <- str_pad(scoring$cnt, 3, pad = "0")
scoring$SCHOOLID <- str_pad(scoring$SCHOOLID, 7, pad = "0")
scoring$StIDStd <- str_pad(scoring$StIDStd, 5, pad = "0")
scoring[,320] <- paste0(scoring$cnt,scoring$SCHOOLID,scoring$StIDStd)
colnames(scoring)[320] <- "ID"


### double check whether selected variables are enough
scoring %>%
  select(starts_with("CP025"),starts_with("CP038"))
table(scoring$CP025Q01)
table(scoring$CP025Q02)
table(scoring$CP038Q01)
table(scoring$CP038Q02)
table(scoring$CP038Q03)


### select CP038Q01 and change name to credit
### delete 7, 8 and recode 0,1,2 to 0,0,1
scoring_S <- scoring %>%
  relocate("ID", .before = cnt) %>%
  select(ID, OECD, CP038Q01) %>%
  filter(CP038Q01 %in% c(0,1,2)) %>%
  mutate(CP038Q01 = factor(CP038Q01,
                           levels = c(0,1,2),
                           labels = c(0,0,1)))

colnames(scoring_S)[3] <- 'credit'
table(scoring_S$credit)


### join logdata and credit
original_scored <- inner_join(original, scoring_S, by = "ID")
write.csv(original_scored, "original_scored.csv")



###' ###########################################################################'
###' 
###' data for event log
###' 
###' 

### TIME padding & produce
minute100 <- original_scored$time%/%60
second100 <- original_scored$time%%60
second100 <- floor(second100)

minute200 <- str_pad(minute100, 2, pad = "0")
second200 <- str_pad(second100, 2, pad = "0")
minutesecond100 <- paste0("14", ":", minute200,":", second200)
original_scored$timestamp <- paste("2021-12-22", minutesecond100)
original_scored$timestamp <- strptime(original_scored$timestamp, "%Y-%m-%d %H:%M:%S", tz="EST5EDT")
class(original_scored$timestamp)
original_scored$timestamp <- as.POSIXct(original_scored$timestamp)
class(original_scored$timestamp)


### event_value: Start End
for (i in 1: length(original_scored$event_value))
{
  if (original_scored$event_value[i]=="NULL"& original_scored$event[i]=="START_ITEM")
  {original_scored$event_value[i] <- "Start"}
  if (original_scored$event_value[i]=="NULL"& original_scored$event[i]=="END_ITEM")
  {original_scored$event_value[i] <- "End"}
  
}


### trip 1,2,3,5 통일
for (i in 1: length(original_scored$event_value))
{
  if (original_scored$event_value[i]=='trip_1'){original_scored$event_value[i]<-'trip_n'}
  if (original_scored$event_value[i]=='trip_2'){original_scored$event_value[i]<-'trip_n'}
  if (original_scored$event_value[i]=='trip_3'){original_scored$event_value[i]<-'trip_n'}
  if (original_scored$event_value[i]=='trip_5'){original_scored$event_value[i]<-'trip_n'}
}


### instance padding & produce(activity에 숫자 부여)
p <-322874
instance <-rep(0,p)
original_scored$instance <- instance

for (i in 1: length(original_scored$event_value))
{
  if (original_scored$event_value[i]=='Start'){original_scored$instance[i]<-'1'}
  if (original_scored$event_value[i]=='city_subway'){original_scored$instance[i]<-'2'}
  if (original_scored$event_value[i]=='country_trains'){original_scored$instance[i]<-'3'}
  if (original_scored$event_value[i]=='full_fare'){original_scored$instance[i]<-'4'}
  if (original_scored$event_value[i]=='concession'){original_scored$instance[i]<-'5'}
  if (original_scored$event_value[i]=='individual'){original_scored$instance[i]<-'6'}
  if (original_scored$event_value[i]=='daily'){original_scored$instance[i]<-'7'}
  if (original_scored$event_value[i]=='trip_n'){original_scored$instance[i]<-'8'}
  if (original_scored$event_value[i]=='trip_4'){original_scored$instance[i]<-'9'}
  if (original_scored$event_value[i]=='Buy'){original_scored$instance[i]<-'10'}
  if (original_scored$event_value[i]=='Cancel'){original_scored$instance[i]<-'11'}
  if (original_scored$event_value[i]=='End'){original_scored$instance[i]<-'12'}
}


## make activity_instance_id
original_scored$instance <- str_pad(original_scored$instance, 2, pad = "0")
original_scored$instance_id <- paste0(original_scored$ID,"-", original_scored$instance)


### delete double clicked data
#original_scored$X <- str_pad(original_scored$X,6,pad='0') # padding
#original_scored$new_instance_id <- paste0(original_scored$ID,'-',original_scored$X)

write.csv(original_scored, "original_scored_eventlog.csv")


nrow(original_scored)


###' ###########################################################################'
###' 
###' Check event log
###' 
###' 

### Import final data

logdata_final <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/EM-IP/datasets/original_scored_eventlog.csv", 
                          header = TRUE, stringsAsFactors = FALSE) %>% tibble()
logdata_final$timestamp <- as.POSIXct(logdata_final$timestamp)
class(logdata_final$timestamp)

### make event_log
event_log <- eventlog(logdata_final, 
                      case_id = "ID", 
                      activity_id = "event_value", 
                      activity_instance_id = "instance_id", 
                      lifecycle_id = "event",
                      timestamp = "timestamp", 
                      resource_id = "cnt")


### check eventlog: .6
event_log %>%
  filter(credit == 1) %>%
  filter_trace_frequency(percentage = 0.65) %>% 
  process_map()

event_log %>%
  filter(credit == 0) %>%
  filter_trace_frequency(percentage = 0.6) %>% 
  process_map()


### check eventlog: .5
event_log %>%
  filter(credit == 1) %>%
  filter_trace_frequency(percentage = 0.5) %>% 
  process_map()

event_log %>%
  filter(credit == 0) %>%
  filter_trace_frequency(percentage = 0.6) %>% 
  process_map()


event_log_full %>%   
  idotted_chart

event_log %>%   
  filter(credit == 0) %>%
  dotted_chart


event_log_full %>%
  filter_trace_frequency(percentage = 0.65) %>%    
  process_map()

event_log_no %>%
  filter_trace_frequency(percentage = 0.65) %>%    
  process_map()


event_log %>%
  dotted_chart(x = "relative", y = "duration", color = "ID")

event_log_full %>%
  dotted_chart(x = "relative", y = "duration", color = "ID")

event_log_full %>%
  dotted_chart(x = "relative", sort = "start", color = "ID", units = "secs", main = "full credit")


