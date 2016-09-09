#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#   https://rstudio.github.io/shinydashboard/get_started.html
#
library(shinydashboard)
#library(data.table)
library(openxlsx)
library(tidyr)
library(dplyr)
library(plotly)
library(RColorBrewer)
#options(java.parameters = "-Xmx1024m" )
library(shiny)

#setwd("F:/Official/KEICRMreporting/CRMReporting/Data")
if (FALSE){
  OppClosed = read.xlsx("closed.xlsx",sheet = 1,startRow = 1, colNames = TRUE,
                         detectDates = TRUE,rowNames = FALSE)
  OppOpen = read.xlsx("open.xlsx",sheet = 1,startRow = 1, colNames = TRUE,
                        detectDates = TRUE,rowNames = FALSE)
  ClosedMarketSeg=unique(OppClosed$Market.segment)
  OppClosed$Won=ifelse(OppClosed$Stage=="Order Received (Won)",1,0)
  OppClosed$Lost=ifelse(OppClosed$Stage=="Order Received (Won)",0,1)
  if (grepl("*T*",OppClosed$`Capacity/Inclination`,ignore.case = TRUE)==TRUE)
  {
    OppClosed$Load=as.numeric(sub("T", "e3", OppClosed$`Capacity/Inclination`, fixed = TRUE))
  }
  if (grepl("*K*",OppClosed$`Capacity/Inclination`,ignore.case = TRUE)==TRUE)
  {
    OppClosed$Load=as.numeric(sub("K", "e3", OppClosed$`Capacity/Inclination`, fixed = TRUE))
  }
  if (OppClosed$Load<1000)
  {
    OppClosed$Load=as.numeric(OppClosed$`Capacity/Inclination`,rm.na=TRUE)*100
  }
 
}
# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "KEI CRM Reporting"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Market Analysis", tabName = "widgets", icon = icon("th"))
       # menuItem("Closed opportunities", tabName = "widgets2", icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                fluidRow(
                  #valueBoxOutput("ThisMonth"),
                  selectInput(inputId = "MarSegChoose","Market Segment",ClosedMarketSeg,multiple = FALSE),
                  # Dynamic valueBoxes
                 # valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
                  valueBoxOutput("ClosedSuccess"),
                  
                 valueBoxOutput("ClosedSuccessPer")
                  
                ),#fluidrow ends
                fluidRow(
                  box(plotlyOutput("DashSuccessChart", height = 350),status = "success"),
                  
                  box(
                    title = "Controls",
                    sliderInput("slider", "Number of observations:", 1, 100, 50)
                  )
                )#Fluid row ends
        ),
        
        # Second tab content
        tabItem(tabName = "widgets",
                fluidRow(
                  box(title="OVERALL MARKET",
                      plotlyOutput("LoadvsSpeedPlotMkt", height = 350),width=12,status = "warning", solidHeader = TRUE,collapsible = TRUE)
                ),
                fluidRow(
                  box(title = "KONE MARKET",
                    plotlyOutput("LoadvsSpeedPlot", height = 350),width=12,status = "success", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE)
                        ),
                fluidRow(
                  box(title = "COMPETITOR MARKET",
                    plotlyOutput("LoadvsSpeedPlotLost", height = 350),width=12,status = "danger", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE)
                )
        )
      )
      
    )#dashboard Body Close
  )#dashboard page close
  
 
  
)
