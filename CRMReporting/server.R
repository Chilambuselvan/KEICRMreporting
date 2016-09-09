#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
   reactdataCon=reactive({
     subcon=OppClosed%>%
       filter(Market.segment==input$MarSegChoose)
  })
   reactdataLoadvsSpeedMkt=reactive({
     subconL_S=OppClosed%>%
       #filter(Market.segment==input$MarSegChoose)%>%
       group_by(Region,Load,Speed)%>%
       summarise(cnt=n())
   })
   reactdataLoadvsSpeed=reactive({
     subconL_S=OppClosed%>%
       #filter(Market.segment==input$MarSegChoose)%>%
       group_by(Region,Load,Speed)%>%
       summarise(cnt=sum(Won))
   })
   reactdataLoadvsSpeedLost=reactive({
     subconL_S=OppClosed%>%
       #filter(Market.segment==input$MarSegChoose)%>%
       group_by(Region,Load,Speed)%>%
       summarise(cnt=sum(Lost))
   })
   
   reactdataConSum = reactive({
     subcon1=OppClosed%>%
       filter(Market.segment==input$MarSegChoose)%>%
       group_by(Region)%>%
       summarise(cnt=sum(Won))
   })
  ################################ DASHBOARD TAB ########################  
  output$ClosedSuccess <- renderValueBox({
    StageCounts=count(reactdataCon(),Stage)
    #   perct=subset(StageCounts,Stage=="Order Received (Won)")$n/nrow(reactdataCon())
       valueBox(
         paste0(subset(StageCounts,Stage=="Order Received (Won)")$n, " Nos "), "WON", icon = icon("list"),
         color = "purple"
       )
    
    })
   output$ClosedSuccessPer <- renderValueBox({
    StageCounts=count(reactdataCon(),Stage)
    perct=subset(StageCounts,Stage=="Order Received (Won)")$n/nrow(reactdataCon())
    valueBox(
      paste0(round(perct*100,0), " %"), "WON", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "aqua"
    )
  })
   
   output$DashSuccessChart<-renderPlotly({
     plot_ly(data=reactdataConSum(),x = Region ,y = cnt,showlegend=FALSE,type="bar")%>%
     add_trace(x = Region, y=cnt,text=cnt,mode="text",textposition ="top middle",
               showlegend=FALSE,hoverinfo="none")%>%
      layout(xaxis=list(title = "Region"),yaxis=list(title = "Quantity"))%>%
       layout(title="Opportunity Won Region Wise")
   })
   ################################ Analysis I TAB ########################  
   output$LoadvsSpeedPlotMkt<- renderPlotly({
     pal <- RColorBrewer::brewer.pal(nlevels(OppClosed$Region), "Set1")
     plot_ly(data = reactdataLoadvsSpeedMkt(), x = Load, y = Speed,mode = "markers",
             marker=list(size=cnt),color = Region,colors = pal) 
   })
   output$LoadvsSpeedPlot<- renderPlotly({
     pal <- RColorBrewer::brewer.pal(nlevels(OppClosed$Region), "Set1")
         plot_ly(data = reactdataLoadvsSpeed(), x = Load, y = Speed,mode = "markers",
           marker=list(size=cnt),color = Region,colors = pal) 
     })
   output$LoadvsSpeedPlotLost<- renderPlotly({
     pal <- RColorBrewer::brewer.pal(nlevels(OppClosed$Region), "Set1")
     plot_ly(data = reactdataLoadvsSpeedLost(), x = Load, y = Speed,mode = "markers",
             marker=list(size=cnt),color = Region,colors = pal) 
   })
})
