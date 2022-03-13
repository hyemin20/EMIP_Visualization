###' ###########################################################################'
###' 
###' Project(project name): EMIP_Visualization
###' 
###' Category(stage in the project): visualization
###' 
###' Task(specific task in the category): 
###' 
###' Data(data source): `PISA 2012. process data`
###' 
###' Date: 
###'        2022.02.16 `start`
###'        2022.02.19 `update`
###'        2020.02.20 `change web address`
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


### Call libraries
library(rsconnect); library(readr); library(tidyverse); library(shiny); library(shinydashboard); library(quantmod);library(plyr)
library(bupaR); library(edeaR); library(reshape); library(stringr); library(stringi); library(processmapR); library(petrinetR); library(DiagrammeR); library(pm4py)



###' ###########################################################################'
###' 
###' Import data
###' 
###' 

### Set file path
logdata_cnt <- read.csv("https://raw.githubusercontent.com/hyemin20/EMIP_Visualization/main/datasets/logdata_cntA.csv",
                        header = TRUE, stringsAsFactors = FALSE) %>% tibble()
logdata_full_credit <- read.csv("https://raw.githubusercontent.com/hyemin20/EMIP_Visualization/main/datasets/full_creditA.csv",
                                header = TRUE, stringsAsFactors = FALSE) %>% tibble()
logdata_no_credit <- read.csv("https://raw.githubusercontent.com/hyemin20/EMIP_Visualization/main/datasets/no_creditA.csv",
                              header = TRUE, stringsAsFactors = FALSE) %>% tibble()



###' ###########################################################################'
###' 
###' Data for event log
###' 
###' 

### Check datasets
str(logdata_cnt)
str(logdata_full_credit)
nrow(distinct(logdata_full_credit, ID))
nrow(distinct(logdata_no_credit, ID))


### event_log
logdata_full_credit$timestamp <- as.POSIXct(logdata_full_credit$timestamp)
logdata_no_credit$timestamp <- as.POSIXct(logdata_no_credit$timestamp)


### make event_log
event_log_full <- eventlog(logdata_full_credit,
                           case_id = "ID", 
                           activity_id = "event_value", 
                           activity_instance_id = "instance_id", 
                           lifecycle_id = "event",
                           timestamp = "timestamp", 
                           resource_id = "cnt")

event_log_no <- eventlog(logdata_no_credit, 
                         case_id = "ID", 
                         activity_id = "event_value", 
                         activity_instance_id = "instance_id", 
                         lifecycle_id = "event",
                         timestamp = "timestamp", 
                         resource_id = "cnt")



  
  ##1
event_log_full %>%
  filter(cnt == "FIN") %>%
  filter_trace_frequency(percentage = 0.7) %>%
  process_map()
    

  
  ## 2
event_log_no %>%
  filter(cnt == "FIN") %>%
  filter_trace_frequency(percentage = 0.7) %>%
  process_map()
    

  
  ##3
event_log_full %>%
  filter(cnt == "FIN") %>%
  dotted_chart(x = "relative", units = "secs")
    
  
  
  ##4
event_log_no %>%
  filter(cnt == "FIN") %>%
  dotted_chart(x = "relative", units = "secs")
