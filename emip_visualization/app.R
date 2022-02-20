###' ###########################################################################'
###' 
###' Project(project name): EM-IP
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


### Set working directory and data directory
work_dir <- c("D:/HYEM'S/GraduatedSchool/STUDY/EM-IP")
data_dir <- file.path(work_dir, "dataset")

### Call libraries
library(rsconnect)
library(tidyverse); library(shiny); library(shinydashboard); library(quantmod);library(rsconnect)
library(plyr); library(bupaR); library(edeaR); library(reshape); library(stringr); library(stringi)
library(processmapR); library(petrinetR); library(DiagrammeR); library(pm4py)



###' ###########################################################################'
###' 
###' Import data
###' 
###' 

### Set file path
logdata_cnt <- read.csv("https://raw.githubusercontent.com/hyemin20/EM-IP/main/datasets/final_cnt.csv?token=GHSAT0AAAAAABQLQJAFUKLYPUAXEKZKO7TYYQZW4RA", 
                        header = TRUE, stringsAsFactors = FALSE) %>% tibble()
logdata_full_credit <- read.csv("https://raw.githubusercontent.com/hyemin20/EM-IP/main/datasets/full_credit.csv?token=GHSAT0AAAAAABQLQJAEDHSDCCLU7EHU7JE2YQZWXSQ", 
                                header = TRUE, stringsAsFactors = FALSE) %>% tibble()
logdata_no_credit <- read.csv("https://raw.githubusercontent.com/hyemin20/EM-IP/main/datasets/no_credit.csv?token=GHSAT0AAAAAABQLQJAFCDLPK7FC7LZZC6LMYQZWYEQ", 
                              header = TRUE, stringsAsFactors = FALSE) %>% tibble()


###' ###########################################################################'
###' 
###' Data for event log
###' 
###' 

### Check datasets
table(logdata_full_credit$credit)
table(logdata_no_credit$credit)


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



###' ###########################################################################'
###' 
###' Shiny: ui
###' 
###' 


### Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("PISA 2012 Process Data"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a Country"),
      
      selectInput("Country", "Country", 
                  choices = distinct(logdata_cnt, cnt), 
                  selected = 1),
      width = 3
    ),
    
    
    mainPanel(
      fluidRow(splitLayout(cellWidths = c("50%","50%"),
                           grVizOutput("plot1"), grVizOutput("plot2"))),
      HTML('&nbsp;'),
      span(),
      div(),
      stri_dup(intToUtf8(160), 6),
      
      fluidRow(splitLayout(cellWidths = c("50%","50%"),
                           plotOutput("plot3"), plotOutput("plot4")))
    ))
)


###' ###########################################################################'
###' 
###' Shiny: server
###' 
###' 


### Define server logic required to draw a histogram
server <- function(input, output) {
  
  ##1
  output$plot1 <- renderGrViz({
    
    event_log_full %>%
      filter(cnt == input$Country) %>%
      filter_trace_frequency(percentage = 0.7) %>%    
      process_map()
    
  })
  
  ## 2
  output$plot2 <- renderGrViz({
    
    event_log_no %>%
      filter(cnt == input$Country) %>%
      filter_trace_frequency(percentage = 0.7) %>%    
      process_map()
    
  })
  
  ##3
  output$plot3 <- renderPlot({
    
    event_log_full %>%
      filter(cnt == input$Country) %>%
      dotted_chart(x = "relative", units = "secs")
    
  })
  
  ##4
  output$plot4 <- renderPlot({
    
    event_log_no %>%
      filter(cnt == input$Country) %>%
      dotted_chart(x = "relative", units = "secs")
    
  })
}


###' ###########################################################################'
###' 
###' Shiny: run
###' 
###' 


### Run the application 
shinyApp(ui = ui, server = server)
