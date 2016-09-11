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
shinyServer(function(input, output,session) {
   reactdataContot=reactive({
    subcon=OppClosed%>%
      filter(Market.segment==input$MarSegChoose)%>%
      summarise(cnt=n())
  })
   reactdataCon=reactive({
     subcon=OppClosed%>%
       filter(Market.segment==input$MarSegChoose)
  })
   reactdataLoadvsSpeedMkt=reactive({
     subconL_S=OppClosed%>%
       filter(Market.segment==input$MarSegChoose)%>%
       group_by(Region,Load,Speed)%>%
       summarise(cnt=n()) %>%
      arrange(desc(Region))
     })
   reactdatamarker=reactive({
     subconL_S=OppClosed%>%
       filter(Market.segment==input$MarSegChoose)%>%
       group_by(Load,Speed)%>%
       summarise(cnt=n())%>%
       ungroup() %>%
         top_n(5,cnt)
     #arrange(desc(Region))
       
   })
   reactdataLoadvsSpeed=reactive({
     subconL_S=OppClosed%>%
       filter(Market.segment==input$MarSegChoose)%>%
       group_by(Region,Load,Speed)%>%
       summarise(cnt=sum(Won))%>%
       arrange(desc(Region))
   })
   reactdataLoadvsSpeedLost=reactive({
     subconL_S=OppClosed%>%
       filter(Market.segment==input$MarSegChoose)%>%
       group_by(Region,Load,Speed)%>%
       summarise(cnt=sum(Lost))%>%
       arrange(desc(Region))
   })
   reactdataQVT=reactive({
     subconL_S=OppClosed%>%
       filter(Market.segment==input$MarSegChoose)%>%
       group_by(`Floors/Travel/Rise`,Load,Speed,Region)%>%
       summarise(cnt=n())
   })
   reactdataConSum = reactive({
     subcon1=OppClosed%>%
       filter(Market.segment==input$MarSegChoose)%>%
       group_by(Region)%>%
       summarise(cnt=sum(Won)) %>%
         arrange(desc(Region))
   })
   reactdataCompet = reactive({
     OppClosed%>%
       filter(Winning.Competitor!="Kone")%>%
       filter(Market.segment==input$MarSegChoose)%>%
       group_by(Region)%>%
       summarise(cnt=n())%>%
       arrange(desc(Region))
     })
  ################################ DASHBOARD TAB ########################  
   output$TotalOpp <- renderValueBox({
     TotCounts=reactdataContot()
      valueBox(
       paste0(TotCounts$cnt, " Nos "), "Total", icon = icon("list"),
       color = "aqua"
     )
     
   })
   output$ClosedSuccess <- renderValueBox({
    StageCounts=count(reactdataCon(),Stage)
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
     plot_ly(data=reactdataConSum(),x = Region ,y = cnt,showlegend=FALSE,type="bar",color = Region,colors = Regionpal)%>%
     add_trace(x = Region, y=cnt,text=cnt,mode="text",textposition ="top middle",
               showlegend=FALSE,hoverinfo="none")%>%
      layout(xaxis=list(title = "Region"),yaxis=list(title = "Quantity"))%>%
       layout(title="Opportunity Won Region Wise")
   })
   output$TopCompetitor<-renderPlotly({
     plot_ly(data=reactdataCompet(),x = Region ,y = cnt,showlegend=FALSE,type="bar",color = Region,colors = Regionpal)%>%
       add_trace(x = Region, y=cnt,text=cnt,mode="text",textposition ="top middle",
                 showlegend=FALSE,hoverinfo="none")%>%
       layout(mode="stack",xaxis=list(title = "Region"),yaxis=list(title = "Quantity"))%>%
       layout(title="Opportunity Lost Region Wise")
   })
   ################################ Market Analysis  ########################  
   output$LoadvsSpeedPlotMkt<- renderPlotly({
     # reactdf1=reactdatamarker()
     # a <- list()
     # for (i in seq_len(nrow(reactdf1))) {
     #   m <- reactdf1[i, ]
     #   a[[i]] <- list(
     #     x = m$Load,
     #     y = m$Speed,
     #     text = paste0(m$Winning.Competitor," ",m$Region," ",m$cnt),
     #     xref = "x",
     #     yref = "y",
     #     showarrow = TRUE,
     #     arrowhead = 7,
     #     ax = 20,
     #     ay = -40
     #   )
     # }
     plot_ly(data = reactdataLoadvsSpeedMkt(), x = Load, y = Speed,mode = "markers",text=paste0(cnt," nos "),
            marker=list(size=cnt),color = Region,colors = Regionpal)
    # layout(annotations = a)   
     # add_trace(data = reactdatamarker(),x = Load, y = Speed,mode = "text",
     #           text=paste0("Load = ",Load," Speed=",Speed," & ",cnt," nos "),
     #                     marker=list(size=cnt))
   })
   output$LoadvsSpeedPlot<- renderPlotly({
     
         plot_ly(data = reactdataLoadvsSpeed(), x = Load, y = Speed,mode = "markers",
           marker=list(size=cnt),color = Region,colors = Regionpal) 
     })
   output$LoadvsSpeedPlotLost<- renderPlotly({
    
     plot_ly(data = reactdataLoadvsSpeedLost(), x = Load, y = Speed,mode = "markers",
             marker=list(size=cnt),color = Region,colors = Regionpal) 
     
   })
   ################################ Market Analysis 3D ########################  
   output$LoadvsSpeedPlot3js <- renderScatterplotThree({
     subconL_S=OppClosed%>%
       #filter(Market.segment==input$MarSegChoose)%>%
       group_by(`Floors/Travel/Rise`,Load,Speed,Region)%>%
       summarise(cnt=n())
     scatterplot3js(x = as.numeric(subconL_S$Load), 
                    y = as.numeric(subconL_S$Speed), 
                    z=as.numeric(subconL_S$`Floors/Travel/Rise`),
                    color=rainbow(length(subconL_S$Region)),
                    size= subconL_S$cnt
                    )
  
   })
})
