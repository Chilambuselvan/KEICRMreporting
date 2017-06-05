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
