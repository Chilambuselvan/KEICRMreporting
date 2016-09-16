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
  #Total Qty of Opportunities Open + Closed 
  reactdataContot=reactive({ 
    subcon=ConsolidatedOpp%>%
      filter(Market.segment==input$MarSegChoose)%>%
      summarise(cnt=sum(Quantity,na.rm = TRUE))
  })
  #DataFrame for Closed Opp  & selected Market 
   reactdataCon=reactive({
     subcon=OppClosed%>%
       filter(Market.segment==input$MarSegChoose)
  })
   #DataFrame for Closed + Open Opp  & selected Market
   reactdataLoadvsSpeedMkt=reactive({
     subconL_S=ConsolidatedOpp%>%
       filter(Market.segment==input$MarSegChoose)%>%
       group_by(Region,Load,Speed)%>%
       summarise(cnt=sum(as.numeric(Quantity),na.rm = TRUE)) %>%
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
   #DataFrame for Closed Opp = Won Qty & Percentage
   reactdataLoadvsSpeed=reactive({
     subconL_S=OppClosed%>%
       filter(Market.segment==input$MarSegChoose & Won ==1)%>%
       group_by(Region,Load,Speed)%>%
       summarise(cnt=sum(Quantity,na.rm = TRUE),percent=paste0(round(sum(Quantity,na.rm = TRUE)/sum(OppClosed$Quantity,na.rm = TRUE)*100,0)," %"))%>%
       arrange(desc(Region))
   })
   #DataFrame for Closed Opp = Lost Qty & Percentage
   reactdataLoadvsSpeedLost=reactive({
     subconL_S=OppClosed%>%
       filter(Market.segment==input$MarSegChoose & Lost ==1)%>%
       group_by(Region,Load,Speed,`Winning.Competitor's.Bid`)%>%
       summarise(cnt=sum(Quantity,na.rm = TRUE),percent=paste0(round(sum(Quantity,na.rm = TRUE)/sum(OppClosed$Quantity,na.rm = TRUE)*100,0)," %"))%>%
       arrange(desc(Region))
   })
   reactdataQVT=reactive({
     subconL_S=ConsolidatedOpp%>%
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
   
   reactdataOppWon = reactive({
     OppClosed%>%
       filter(Winning.Competitor=="Kone")%>%
       group_by(KONE.Opportunity.Number,Opportunity.Name,Winning.Competitor,Region,Opportunity.Quantity)%>%
       summarise(SalesPrice=mean(Total.Price))
      })
   reactdataOppLost = reactive({
     OppClosed%>%
       filter(!is.na(`Winning.Competitor's.Bid`) & Winning.Competitor!="Kone")%>%
       group_by(KONE.Opportunity.Number,Opportunity.Name,Winning.Competitor,Region,Opportunity.Quantity,
                `Winning.Competitor's.Bid`,`Winning.Competitor's.Bid.Currency`)%>%
      summarise(DiffPrice=mean(Amount-`Winning.Competitor's.Bid`)) %>%
      arrange(desc(DiffPrice))
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
   ################################ Opportunities Analysis ########################  
   output$PriceMarginWon = renderDataTable({
     Dt=reactdataOppWon()
     columns=c("Region","Opportunity.Name","SalesPrice","Opportunity.Quantity")
     datatable(Dt[,columns,drop=FALSE],filter="top")
   })
   output$PriceMarginLost = renderDataTable({
     Dt=reactdataOppLost()
     #Dt$`Winning.Competitor's.Bid` = paste(Dt$`Winning.Competitor's.Bid`, Dt$`Winning.Competitor's.Bid.Currency`, sep="")
     columns=c("Region","Opportunity.Name","Opportunity.Quantity","Winning.Competitor's.Bid","DiffPrice")
     datatable(Dt[,columns,drop=FALSE],filter="top",
               colnames = c('Region', 'Opp.Name', 'Qty', 'Competitor price', 'Price Difference'))
      
   })
   ################################ Data View ########################  
   output$tabOverAllMarket = renderDataTable({
     Dt=reactdataLoadvsSpeedMkt()
     Dt=Dt %>%
       arrange(desc(cnt))
     #Dt$`Winning.Competitor's.Bid` = paste(Dt$`Winning.Competitor's.Bid`, Dt$`Winning.Competitor's.Bid.Currency`, sep="")
     columns=c("Region","Load","Speed","cnt")
     datatable(Dt[,columns,drop=FALSE],filter="bottom",class = 'cell-border stripe',rownames = FALSE,
               colnames = c('REGION', 'LOAD', 'SPEED','Quantity/Count'))
     })
   output$tabKONEMarket = renderDataTable({
     Dt=reactdataLoadvsSpeed()
     Dt=Dt %>%
       arrange(desc(cnt))
     #Dt$`Winning.Competitor's.Bid` = paste(Dt$`Winning.Competitor's.Bid`, Dt$`Winning.Competitor's.Bid.Currency`, sep="")
     columns=c("Region","Load","Speed","cnt","percent")
     datatable(Dt[,columns,drop=FALSE],filter="bottom",class = 'cell-border stripe',rownames = FALSE,
               colnames = c('REGION', 'LOAD', 'SPEED','Quantity/Count','Pecentage'))
   })
   output$tabCompMarket = renderDataTable({
     Dt=reactdataLoadvsSpeedLost()
     Dt=Dt %>%
       group_by(Region,Load,Speed)%>%
       arrange(desc(cnt))%>%
     summarise(Topcomp = paste(`Winning.Competitor's.Bid`[min(cnt) == cnt], collapse = ","))
     #Dt$`Winning.Competitor's.Bid` = paste(Dt$`Winning.Competitor's.Bid`, Dt$`Winning.Competitor's.Bid.Currency`, sep="")
     columns=c("Region","Load","Speed","cnt","percent","Topcomp")
     datatable(Dt[,columns,drop=FALSE],filter="bottom",class = 'cell-border stripe',rownames = FALSE,
               colnames = c('REGION', 'LOAD', 'SPEED','Quantity/Count','Pecentage','Competitor'))
   })
   
})
