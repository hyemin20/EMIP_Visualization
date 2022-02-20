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
      
      selectInput("Country", "Select a Country", 
                  choices = distinct(logdata_cnt, cnt), 
                  selected = 1),
      width = 3,
      
      # adding the new div tag to the sidebar            
      tags$div(class="header", checked=NA,
               tags$p("Ready to take the Shiny tutorial? If so"),
               tags$a(href="shiny.rstudio.com/tutorial", "Click Here!")
      
               )
      
    ),
    
    
    mainPanel(
      fluidRow(splitLayout(cellWidths = c("50%","50%"),
                           grVizOutput("plot1"), grVizOutput("plot2"))),
#      HTML('&nbsp;'),
#      tags$div(tags$dbr),
      tags$hr(),
      tags$div(tags$dbr),
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

