#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#   https://rstudio.github.io/shinydashboard/get_started.html
#

# install.packages("shinydashboard")
# install.packages("openxlsx")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("ggmap")
# install.packages("data.table")
# install.packages("plotly")
# install.packages("leaflet")
# install.packages("htmltools")
# install.packages("RColorBrewer")
# install.packages("threejs")
# install.packages("DT")
# install.packages("shiny")

suppressMessages(library(shinydashboard))
#library(data.table)
suppressMessages(library(openxlsx))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(ggmap))
suppressMessages(library(data.table))
suppressMessages(library(plotly))
suppressMessages(library(leaflet))
suppressMessages(library(htmltools))
suppressMessages(library(RColorBrewer))
suppressMessages(library(threejs))
suppressMessages(library(DT))
suppressMessages(library(shiny))

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
  
  OppClosed$PerUnitPrice =  OppClosed$Total.Price/OppClosed$Quantity
  OppOpen$PerUnitPrice =  OppOpen$Total.Price/OppOpen$Quantity
  
  OppClosed$`Winning.Competitor's.Bid` = ifelse(grepl("*kone*",OppClosed$Winning.Competitor,ignore.case = TRUE),
    OppClosed$Amount,OppClosed$`Winning.Competitor's.Bid`)
  
 
    OppClosed$Load=as.numeric(sub("T", "e3", OppClosed$`Capacity/Inclination`, fixed = TRUE))
    OppClosed$Load=as.numeric(sub("K", "e3", OppClosed$`Capacity/Inclination`, fixed = TRUE))
    OppClosed$Load=as.numeric(OppClosed$`Capacity/Inclination`,rm.na=TRUE)*68

    OppOpen$Load=as.numeric(sub("T", "e3", OppOpen$`Capacity/Inclination`, fixed = TRUE))
    OppOpen$Load=as.numeric(sub("K", "e3", OppOpen$`Capacity/Inclination`, fixed = TRUE))
    OppOpen$Load=as.numeric(OppOpen$`Capacity/Inclination`,rm.na=TRUE)*68

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
        # menuItem("Price Analysis", tabName = "OppAnalysis", icon = icon("inr")),
        menuItem("Data View", tabName = "DataView", icon = icon("th")),
        menuItem("Elevator Volumes (ALL)", tabName = "ElevVolumes", icon = icon("map-marker")),
        menuItem("Elevator Analysis", tabName = "ElevAnalysis", icon = icon("map-marker")),
        menuItem("Price Analysis", tabName = "priceAnalysis", icon = icon("inr")),
        menuItem("TOP SELLER", tabName = "UnitAnalysis", icon = icon("bar-chart")),
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
                 valueBoxOutput("TotalOpp",width=6)
                 # ,
                 # valueBoxOutput("ClosedSuccess"),
                 # valueBoxOutput("ClosedSuccessPer")
                 ),#fluidrow ends
                fluidRow(
                  box(plotlyOutput("DashSuccessChart", height = 350),status = "success"),
                  box(plotlyOutput("TopCompetitor", height = 350),status = "warning")
                )#Fluid row ends
        ),
        # Second tab content
        tabItem(tabName = "widgets",
                fluidRow(
                  box(title="OVERALL",
                      plotlyOutput("LoadvsSpeedPlotMkt", height = 350),width=12,status = "warning", solidHeader = TRUE,collapsible = TRUE)
                ),
                fluidRow(
                  box(title = "KONE",
                    plotlyOutput("LoadvsSpeedPlot", height = 350),width=12,status = "success", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE)
                        ),
                fluidRow(
                  box(title = "COMPETITOR",
                    plotlyOutput("LoadvsSpeedPlotLost", height = 350),width=12,status = "danger", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE)
                )
        ),# Second tab content ends
        # Third tab content
        tabItem(tabName = "3Dview",
              fluidRow(
                  box(title = "3D view",width=12,status = "info", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                      scatterplotThreeOutput("LoadvsSpeedPlot3js"))
                )
        ),# Third tab content ends
        # Fifth tab content
        tabItem(tabName = "DataView",
                fluidRow(
                  box(title = "Overall",width=12,status = "warning", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                      dataTableOutput("tabOverAllMarket"))
                ),
                fluidRow(
                  box(title = "KONE",width=12,status = "success", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                      dataTableOutput("tabKONEMarket"))
                ),
                fluidRow(
                  box(title = "COMPETITOR",width=12,status = "danger", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                      dataTableOutput("tabCompMarket"))
                )
        ),# Sixth tab content ends
        tabItem(tabName = "ElevVolumes",
                fluidRow(
                  column(6,
                  selectInput(inputId = "SelLoad","Choose Load",ConsolidatedOpp$Load,multiple = TRUE)
                  ),
                  column(6,
                  selectInput(inputId = "SelSpeed","Choose Speed",ConsolidatedOpp$Speed,multiple = TRUE)
                  )
                ),
                fluidRow(
                  box(title = "Overall (Region Wise)",width=12,status = "warning", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                      leafletOutput("mapOverAllMarket"))
                ),
                fluidRow(
                  box(title = "Market Analysis (State Wise)",width=12,status = "success", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                      leafletOutput("mapKONEMarket")
                      # ,
                      # absolutePanel(top = 20, right = 20,
                      #                checkboxInput("legend", "Show legend", TRUE)
                      #             )
                      )
                )
                # ,
                # fluidRow(
                #   box(title = "COMPETITOR",width=12,status = "danger", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                #       leafletOutput("mapCompMarket"))
                # )
        ),# Sixth tab content ends
        # Seventh tab content Starts
        tabItem(tabName = "ElevAnalysis",
                fluidRow(
                  column(6,
                         selectInput(inputId = "SelLoad_An","Choose Load",OppClosed$Load,multiple = TRUE)
                  ),
                  column(6,
                         selectInput(inputId = "SelSpeed_An","Choose Speed",OppClosed$Speed,multiple = TRUE)
                  )
                ),
                fluidRow(

                  box(title = "KONE",width=6,status = "success", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                      leafletOutput("mapKONEOppClosed")),

                  box(title = "Competitor",width=6,status = "danger", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                      leafletOutput("mapCompOppClosed"))
                ),
                fluidRow(
                  
                  box(title = "KONE",width=6,status = "success", solidHeader = FALSE,collapsible = TRUE,collapsed = FALSE,
                      dataTableOutput("tabKONEMarket_sub1")),
                  
                  box(title = "COMPETITOR",width=6,status = "danger", solidHeader = FALSE,collapsible = TRUE,collapsed = FALSE,
                      dataTableOutput("tabCompMarket_sub1"))
                )
                # ,
                # fluidRow(
                #   box(title = "COMPETITOR Market",width=12,status = "danger", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                #       leafletOutput("mapCompMarket"))
                # )
        ),# Seventh tab content ends 
        tabItem(tabName = "priceAnalysis",
                fluidRow(
                  column(4,
                         selectInput(inputId = "SelLoad_Pr1","Choose Load",OppClosed$Load,multiple = TRUE)
                  ),
                  column(4,
                         selectInput(inputId = "SelSpeed_Pr1","Choose Speed",OppClosed$Speed,multiple = TRUE)
                  ),
                  column(4,
                         selectInput(inputId = "SelRegion_Pr1","Choose Region",unique(OppClosed$Region),multiple = TRUE)
                  )
                  
                ),
                fluidRow(
                  box(title = "Price Analysis",width=12,status = "success", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                      plotlyOutput("priceComparison"))
                ),
                fluidRow(
                  box(title = "Price Analysis (Option2)",width=12,status = "success", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                      plotlyOutput("priceComparison2"))
                ),
                fluidRow(
                box(title = "TOP SELLER FOR SELECTION",width=12,status = "warning", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                    dataTableOutput("tabPriceAnalysis"))
                ) 
                
                # ,
                # fluidRow(
                #   box(title = "COMPETITOR Market",width=12,status = "danger", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                #       leafletOutput("mapCompMarket"))
                # )
        ),# Seventh tab content ends UnitAnalysis
        
        tabItem(tabName = "UnitAnalysis",
                fluidRow(
                  column(4,
                         selectInput(inputId = "SelLoad_br1","Choose Load",OppClosed$Load,multiple = TRUE)
                  ),
                  column(4,
                         selectInput(inputId = "SelSpeed_br1","Choose Speed",OppClosed$Speed,multiple = TRUE)
                  ),
                  column(4,
                         selectInput(inputId = "SelRegion_br1","Choose Region",unique(OppClosed$Region),multiple = TRUE)
                  )
                ),
                fluidRow(
                  box(title = "TOP SELLERS",width=12,status = "warning", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                      plotlyOutput("SellerComparison"))
                ) 
              )
        
        )
      )#dashboard Body Close
  )#dashboard page close
)
