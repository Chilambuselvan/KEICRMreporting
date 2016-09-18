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
library(ggmap)
library(data.table)
library(plotly)
library(leaflet)
library(htmltools)
library(RColorBrewer)
library(threejs)
library(DT)
#options(java.parameters = "-Xmx1024m" )
library(shiny)

#setwd("F:/Official/KEICRMreporting/CRMReporting/Data")
if (FALSE){
  OppClosed = read.xlsx("closed.xlsx",sheet = 1,startRow = 1, colNames = TRUE,
                        detectDates = TRUE,rowNames = FALSE)
  OppOpen = read.xlsx("open.xlsx",sheet = 1,startRow = 1, colNames = TRUE,
                      detectDates = TRUE,rowNames = FALSE)
  OppClosed$Branch.Office=sub(" MP", "", OppClosed$Branch.Office, fixed = TRUE)
  OppOpen$Branch.Office=sub(" MP", "", OppOpen$Branch.Office, fixed = TRUE)
  ClosedMarketSeg=unique(OppClosed$Market.segment)
  OppClosed$Won=ifelse(OppClosed$Stage=="Order Received (Won)",1,0)
  OppClosed$Lost=ifelse(OppClosed$Stage=="Order Received (Won)",0,1)
  OppOpen$Won=ifelse(OppOpen$Stage=="Order Received (Won)",1,0)
  OppOpen$Lost=ifelse(OppOpen$Stage=="Order Received (Won)",0,1)
  BranchMapping=fread("BranchGeoCode.csv",stringsAsFactors = FALSE, header=TRUE)
  BranchMapping=data.frame(BranchMapping)
  OppClosed=left_join(OppClosed,BranchMapping,by=c("Branch.Office"="Bcity...1."))
  OppOpen=left_join(OppOpen,BranchMapping,by=c("Branch.Office"="Bcity...1."))
  
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
  
  if (grepl("*T*",OppOpen$`Capacity/Inclination`,ignore.case = TRUE)==TRUE)
  {
    OppOpen$Load=as.numeric(sub("T", "e3", OppOpen$`Capacity/Inclination`, fixed = TRUE))
  }
  if (grepl("*K*",OppOpen$`Capacity/Inclination`,ignore.case = TRUE)==TRUE)
  {
    OppOpen$Load=as.numeric(sub("K", "e3", OppOpen$`Capacity/Inclination`, fixed = TRUE))
  }
  if (OppOpen$Load<1000)
  {
    OppOpen$Load=as.numeric(OppOpen$`Capacity/Inclination`,rm.na=TRUE)*100
  }
  ConsolidatedOpp = rbind(OppClosed,OppOpen)
  Regionpal=c("forestgreen", "darkorange", "deepskyblue", "dimgray")
  ################################################# Preparing Geo Code fetching####################################
  
  ###### Binding Branch & disctrict based on Branchcode OrigOffice
  # BranchCity=unique(ConsolidatedOpp$Branch.Office)
  # BranchCity=BranchCity[!is.na(BranchCity)]
  # Bcity=data.frame(BranchCity,stringsAsFactors = FALSE)
  # Bcity=sub(" MP", "", Bcity$BranchCity, fixed = TRUE)
  # Bcity=unique(Bcity)
  # Bcity=data.frame(Bcity)
  # 
  # geocodes <- geocode(as.character(Bcity$Bcity),output = "more")
  # geocodes = geocodes %>%
  #   select(lat,lon,administrative_area_level_1)
  # Bcity <- data.frame(Bcity[,1],geocodes,stringsAsFactors = FALSE)
  # write.csv(Bcity,file="BranchGeoCode.csv",row.names = FALSE)
  # 
}
# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "KEI CRM Reporting"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Market Analysis", tabName = "widgets", icon = icon("balance-scale")),
        menuItem("Market Analysis 3D", tabName = "3Dview", icon = icon("bars")),
        menuItem("Price Analysis", tabName = "OppAnalysis", icon = icon("inr")),
        menuItem("Data View", tabName = "DataView", icon = icon("th")),
        menuItem("Elevator Volumes", tabName = "ElevVolumes", icon = icon("map-marker")),
        menuItem("Missing Data", tabName = "missingSummary", icon = icon("warning"))
       # menuItem("Closed opportunities", tabName = "widgets2", icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
        #### DF summary ########
        tabItem(tabName = "missingSummary",
                fluidRow(
                  box(title = "Missing Data Summary (Closed opportunities)",width=12,status = "warning", solidHeader = TRUE,collapsible = FALSE,collapsed = FALSE,
                      dataTableOutput("tabmissingSummary"))
                )
              ),
        # First tab content
        tabItem(tabName = "dashboard",
                fluidRow(
                 selectInput(inputId = "MarSegChoose","Market Segment",ClosedMarketSeg,multiple = FALSE),
                 valueBoxOutput("TotalOpp"),
                 valueBoxOutput("ClosedSuccess"),
                 valueBoxOutput("ClosedSuccessPer")
                 ),#fluidrow ends
                fluidRow(
                  box(plotlyOutput("DashSuccessChart", height = 350),status = "success"),
                  box(plotlyOutput("TopCompetitor", height = 350),status = "warning")
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
        ),# Second tab content ends
        # Third tab content
        tabItem(tabName = "3Dview",
              fluidRow(
                  box(title = "3D view MARKET",width=12,status = "info", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                      scatterplotThreeOutput("LoadvsSpeedPlot3js"))
                )
        ),# Third tab content ends
        # Fourth tab content
        tabItem(tabName = "OppAnalysis",
                fluidRow(
                selectInput(inputId = "selectRow","Choose Column",colnames(OppClosed),multiple = TRUE)
                ),
                fluidRow(
                  box(title = "Price Margin (LOST)",width=12,status = "warning", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                      dataTableOutput("PriceMarginLost"))
                ),
                fluidRow(
                  box(title = "Price Margin (WON)",width=12,status = "success", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                      dataTableOutput("PriceMarginWon"))
                )
        ),# Fourth tab content ends
        # Fifth tab content
        tabItem(tabName = "DataView",
                fluidRow(
                  box(title = "Overall Market",width=12,status = "warning", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                      dataTableOutput("tabOverAllMarket"))
                ),
                fluidRow(
                  box(title = "KONE Market",width=12,status = "success", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                      dataTableOutput("tabKONEMarket"))
                ),
                fluidRow(
                  box(title = "COMPETITOR Market",width=12,status = "danger", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                      dataTableOutput("tabCompMarket"))
                )
        ),# Sixth tab content ends
        tabItem(tabName = "ElevVolumes",
                fluidRow(
                  box(title = "Overall Market",width=12,status = "warning", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                      leafletOutput("mapOverAllMarket"))
                ),
                fluidRow(
                  box(title = "Market Analysis",width=12,status = "success", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                      leafletOutput("mapKONEMarket"),
                      absolutePanel(top = 10, right = 10,
                                    sliderInput("range", "Magnitudes", min(1), max(100),
                                                value = 1, step = 10
                                    ),
                                    checkboxInput("legend", "Show legend", TRUE)
                                  )
                      
                      )
                )
                # ,
                # fluidRow(
                #   box(title = "COMPETITOR Market",width=12,status = "danger", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                #       leafletOutput("mapCompMarket"))
                # )
        )# Sixth tab content ends
      )
    )#dashboard Body Close
  )#dashboard page close
)
