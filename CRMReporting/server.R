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
       filter(Market.segment==input$MarSegChoose) %>%
       group_by(Stage) %>%
       summarise(n=sum(Quantity,na.rm = TRUE))
  })
   #DataFrame for Closed + Open Opp  & selected Market
   reactdataLoadvsSpeedMkt=reactive({
     ConsolidatedOpp%>%
       filter(Market.segment==input$MarSegChoose)%>%
       group_by(Region,Load,Speed)%>%
       summarise(cnt=sum(as.numeric(Quantity),na.rm = TRUE)) %>%
      arrange(desc(Region))
     })
   reactdatamarker=reactive({
     OppClosed%>%
       filter(Market.segment==input$MarSegChoose)%>%
       group_by(Load,Speed)%>%
       summarise(cnt=n())%>%
       ungroup() %>%
         top_n(5,cnt)
     #arrange(desc(Region))
       
   })
   #DataFrame for Closed Opp = Won Qty & Percentage
   reactdataLoadvsSpeed=reactive({
     OppClosed%>%
       filter(Market.segment==input$MarSegChoose & Won ==1)%>%
       group_by(Region,Load,Speed)%>%
       summarise(cnt=sum(Quantity,na.rm = TRUE),percent=paste0(round(sum(Quantity,na.rm = TRUE)/sum(OppClosed$Quantity,na.rm = TRUE)*100,0)," %"))%>%
       arrange(desc(Region))
   })
   #DataFrame for Closed Opp = Lost Qty & Percentage
   reactdataLoadvsSpeedLost=reactive({
     OppClosed%>%
       filter(Lost ==1)%>%
       filter(Market.segment==input$MarSegChoose)%>%
       group_by(Region,Load,Speed,`Winning.Competitor's.Bid`,Winning.Competitor)%>%
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
       filter(Won==1)%>%
       filter(Market.segment==input$MarSegChoose)%>%
       group_by(Region)%>%
       summarise(cnt=sum(Quantity,na.rm=TRUE)) %>%
         arrange(desc(Region))
   })
   reactdataCompet = reactive({
     OppClosed%>%
       filter(Winning.Competitor!="Kone")%>%
       filter(Market.segment==input$MarSegChoose)%>%
       group_by(Region)%>%
       summarise(cnt=sum(Quantity,na.rm=TRUE))%>%
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
   #######################DataFrame_Summary################ tabmissingSummary
   output$tabmissingSummary = renderDataTable({
      missing.summary <- sapply(OppClosed, function(x) sum(is.na(x))) 
      indexs.missing <- sapply(OppClosed, function(x) sum(is.na(x))) > 0 
      num.variable.missing <- length(missing.summary[indexs.missing])
     
     freq.table.miss <- data.frame( Variable = names(missing.summary[indexs.missing]), Number.of.Missing = as.integer(missing.summary[indexs.missing]), 
                                    Percentage.of.Missing = paste0(round(as.numeric(prop.table(missing.summary[indexs.missing]))*100,2)," %") )
     
     freq.table.miss <- freq.table.miss %>% 
       select(Variable:Percentage.of.Missing) %>%
       arrange(desc(Number.of.Missing))
     
     datatable(freq.table.miss)
   })
   ########################DataFrame_SummaryEnds###############
   
  ################################ DASHBOARD TAB ########################  
   output$TotalOpp <- renderValueBox({
     TotCounts=reactdataContot()
      valueBox(
       paste0(TotCounts$cnt, " Nos "), paste0("Total number of Opportunity in ",input$MarSegChoose), icon = icon("list"),
       color = "aqua"
     )
     
   })
   output$ClosedSuccess <- renderValueBox({
     StageCounts=reactdataCon()
      valueBox(
         paste0(subset(StageCounts,Stage=="Order Received (Won)")$n, " Nos "), "WON", icon = icon("list"),
         color = "purple"
       )
    })
   output$ClosedSuccessPer <- renderValueBox({
     StageCounts=reactdataCon()
    perct=subset(StageCounts,Stage=="Order Received (Won)")$n/colSums(StageCounts[,2])
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
     Df=reactdataQVT()
     Df=Df%>%
       #filter(Market.segment==input$MarSegChoose)%>%
       group_by(`Floors/Travel/Rise`,Load,Speed,Region)%>%
       summarise(cnt=n())
     scatterplot3js(x = as.numeric(Df$Load), 
                    y = as.numeric(Df$Speed), 
                    z=as.numeric(Df$`Floors/Travel/Rise`),
                    color=rainbow(length(Df$Region)),
                    size= Df$cnt
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
       group_by(Region,Load,Speed,cnt)%>%
       arrange(desc(cnt))%>%
     summarise(Topcomp = paste(Winning.Competitor[max(cnt) == cnt], collapse = ","))
     Dt$Topcomp=vapply(strsplit(Dt$Topcomp, ","), function(x) paste(unique(x), collapse = ","), character(1L))
     Dt=Dt %>%
       group_by(Region,Load,Speed)%>%
       slice(which.max(cnt))%>%
       arrange(desc(cnt))
     #Dt$`Winning.Competitor's.Bid` = paste(Dt$`Winning.Competitor's.Bid`, Dt$`Winning.Competitor's.Bid.Currency`, sep="")
     columns=c("Region","Load","Speed","cnt","Topcomp")
     datatable(Dt[,columns,drop=FALSE],filter="bottom",class = 'cell-border stripe',rownames = FALSE,
               colnames = c('REGION', 'LOAD', 'SPEED','Quantity/Count','Competitor'))
   })
   ################################ Elev Volumes View ########################  
   reactmapOverall = reactive({
     Filter_ConOpp=ConsolidatedOpp
     if(!is.null(input$SelLoad)){
       Filter_ConOpp = subset(ConsolidatedOpp,ConsolidatedOpp$Load %in% input$SelLoad)
     }
     if(!is.null(input$SelSpeed)){
       Filter_ConOpp = subset(ConsolidatedOpp,ConsolidatedOpp$Speed %in% input$SelSpeed)
     }
     if(!is.null(input$SelSpeed) && !is.null(input$SelLoad) ){
       Filter_ConOpp = subset(ConsolidatedOpp,ConsolidatedOpp$Speed %in% input$SelSpeed)
       Filter_ConOpp = subset(Filter_ConOpp,Filter_ConOpp$Load %in% input$SelLoad)
     }
     Dt1=Filter_ConOpp %>%
       filter(Market.segment==input$MarSegChoose) %>%
       group_by(administrative_area_level_1) %>%
       summarise(cnt=sum(Quantity,na.rm=TRUE),
                 lat=mean(lat,na.rm=TRUE),lon=mean(lon,na.rm=TRUE))
     
   })
   output$mapOverAllMarket = renderLeaflet({
     pal <- colorFactor(Regionpal, domain = c("West","South","North","East"))
     Filter_ConOpp=ConsolidatedOpp
     if(!is.null(input$SelLoad)){
       Filter_ConOpp = subset(ConsolidatedOpp,ConsolidatedOpp$Load %in% input$SelLoad)
     }
     if(!is.null(input$SelSpeed)){
       Filter_ConOpp = subset(ConsolidatedOpp,ConsolidatedOpp$Speed %in% input$SelSpeed)
     }
     if(!is.null(input$SelSpeed) && !is.null(input$SelLoad) ){
       Filter_ConOpp = subset(ConsolidatedOpp,ConsolidatedOpp$Speed %in% input$SelSpeed)
       Filter_ConOpp = subset(Filter_ConOpp,Filter_ConOpp$Load %in% input$SelLoad)
     }
      Dt=Filter_ConOpp %>%
      filter(Market.segment==input$MarSegChoose) %>%
        
        group_by(Region,Branch.Office,lat,lon) %>%
          summarise(cnt=sum(Quantity,na.rm=TRUE))
      content <- paste0(Dt$Branch.Office," Nos: ",Dt$cnt)
      m=leaflet(na.omit(Dt)) %>% addTiles() %>%
      addCircleMarkers(data = Dt, lng = ~ lon, lat = ~ lat,
                        color= ~pal(Region), radius = ~cnt/10,
                        stroke = FALSE, fillOpacity = 0.5,
                        popup = ~htmlEscape(content))  %>%
          addLegend(position="bottomright",labels=unique(Dt$Region),colors=Regionpal)
      print(m)
   })
  
   output$mapKONEMarket = renderLeaflet({
     Dt1=reactmapOverall()
     content <- paste0(Dt1$administrative_area_level_1," Nos: ",Dt1$cnt)
     m=leaflet(na.omit(Dt1)) %>% addTiles() %>%
       #addProviderTiles("Stamen.TonerHybrid") %>%
         addCircleMarkers(data = Dt1, lng = ~ lon, lat = ~ lat
                        ,color= brewer.pal(30,"Dark2"), radius = ~cnt/10,
                        stroke = FALSE, fillOpacity = 0.5,
        popup = ~htmlEscape(content)) 
     print(m)
   })
   ################################ Elev Analysis View ########################  
   reacttabMapviewKONE = reactive({
     Filter_ClosedOpp=OppClosed
     if(!is.null(input$SelLoad_An)){
       Filter_ClosedOpp = subset(OppClosed,OppClosed$Load %in% input$SelLoad_An)
     }
     if(!is.null(input$SelSpeed_An)){
       Filter_ClosedOpp = subset(OppClosed,OppClosed$Speed %in% input$SelSpeed_An)
     }
     if(!is.null(input$SelSpeed_An) && !is.null(input$SelLoad_An)){
       Filter_ClosedOpp = subset(OppClosed,OppClosed$Speed %in% input$SelSpeed_An)
       Filter_ClosedOpp = subset(Filter_ClosedOpp,Filter_ClosedOpp$Load %in% input$SelLoad_An)
     }
     Filter_ClosedOpp=Filter_ClosedOpp %>%
       filter(Winning.Competitor=="Kone") %>%
       filter(Market.segment==input$MarSegChoose) %>%
       group_by(Region,Branch.Office,lat,lon) %>%
       summarise(cnt=sum(Quantity,na.rm=TRUE))
   })
   reacttabMapviewComp = reactive({
     Filter_ClosedOpp=OppClosed
     if(!is.null(input$SelLoad_An)){
       Filter_ClosedOpp = subset(OppClosed,OppClosed$Load %in% input$SelLoad_An)
     }
     if(!is.null(input$SelSpeed_An)){
       Filter_ClosedOpp = subset(OppClosed,OppClosed$Speed %in% input$SelSpeed_An)
     }
     if(!is.null(input$SelSpeed_An) && !is.null(input$SelLoad_An)){
       Filter_ClosedOpp = subset(OppClosed,OppClosed$Speed %in% input$SelSpeed_An)
       Filter_ClosedOpp = subset(Filter_ClosedOpp,Filter_ClosedOpp$Load %in% input$SelLoad_An)
     }
     Filter_ClosedOpp=Filter_ClosedOpp %>%
       filter(Market.segment==input$MarSegChoose && Winning.Competitor!="Kone") %>%
       group_by(Region,Branch.Office,lat,lon) %>%
       summarise(cnt=sum(Quantity,na.rm=TRUE))
   })
   output$mapKONEOppClosed = renderLeaflet({
     pal <- colorFactor(Regionpal, domain = c("West","South","North","East"))
     Dt = reacttabMapviewKONE()
     content <- paste0(Dt$Branch.Office," Nos: ",Dt$cnt)
     m=leaflet(na.omit(Dt)) %>% addTiles() %>%
       addCircleMarkers(data = Dt, lng = ~ lon, lat = ~ lat,
                        color= ~pal(Region), radius = ~cnt/5,
                        stroke = FALSE, fillOpacity = 0.5,
                        popup = ~htmlEscape(content))  %>%
       addLegend(position="bottomright",labels=unique(Dt$Region),colors=Regionpal)
     print(m)
   })
   output$mapCompOppClosed = renderLeaflet({
     pal <- colorFactor(Regionpal, domain = c("West","South","North","East"))
     Dt = reacttabMapviewComp()
     content <- paste0(Dt$Branch.Office," Nos: ",Dt$cnt)
     m=leaflet(na.omit(Dt)) %>% addTiles() %>%
       addCircleMarkers(data = Dt, lng = ~ lon, lat = ~ lat,
                        color= ~pal(Region), radius = ~cnt/5,
                        stroke = FALSE, fillOpacity = 0.5,
                        popup = ~htmlEscape(content))  %>%
       addLegend(position="bottomright",labels=unique(Dt$Region),colors=Regionpal)
     print(m)
   })
   output$tabKONEMarket_sub1 = renderDataTable({
     Dt=reacttabMapviewKONE()
     Dt=Dt %>%
       arrange(desc(cnt))
     #Dt$`Winning.Competitor's.Bid` = paste(Dt$`Winning.Competitor's.Bid`, Dt$`Winning.Competitor's.Bid.Currency`, sep="")
     columns=c("Region","Branch.Office","cnt")
     datatable(Dt[,columns,drop=FALSE],filter="bottom",class = 'cell-border stripe',rownames = FALSE,
               colnames = c('REGION', 'BRANCH', 'Quantity/Count'))
   })
   output$tabCompMarket_sub1 = renderDataTable({
     Dt=reacttabMapviewComp()
     Dt=Dt %>%
       arrange(desc(cnt))
     #Dt$`Winning.Competitor's.Bid` = paste(Dt$`Winning.Competitor's.Bid`, Dt$`Winning.Competitor's.Bid.Currency`, sep="")
     columns=c("Region","Branch.Office","cnt")
     datatable(Dt[,columns,drop=FALSE],filter="bottom",class = 'cell-border stripe',rownames = FALSE,
               colnames = c('REGION', 'BRANCH', 'Quantity/Count'))
   })
   ################################ Price Analysis View ########################  
   reactPriceAnalysis = reactive({
     Filter_ClosedOpp=OppClosed %>%
       filter(!is.na(Winning.Competitor)) %>%
         filter(Market.segment==input$MarSegChoose)
     
     # if(!is.null(input$SelRegion_Pr1)){
     # Filter_ClosedOpp=Filter_ClosedOpp %>%
     #   filter(Region %in% SelRegion_Pr1)
     # }
     })
   reacttabMarkettop5=reactive({
     Dt=OppClosed%>%
       filter(Market.segment==input$MarSegChoose)%>%
       group_by(Region,Load,Speed,Winning.Competitor)%>%
       summarise(cnt=sum(Quantity,na.rm = TRUE),percent=paste0(round(sum(Quantity,na.rm = TRUE)/sum(OppClosed$Quantity,na.rm = TRUE)*100,0)," %"))%>%
       arrange(desc(Region))
   })
   
   output$priceComparison<- renderPlotly({
     
    
     
       Filter_ClosedOpp=reactPriceAnalysis()
     
     if(!is.null(input$SelLoad_Pr1)){
       Filter_ClosedOpp = subset(Filter_ClosedOpp,Filter_ClosedOpp$Load %in% input$SelLoad_Pr1)
     }
     if(!is.null(input$SelSpeed_Pr1)){
       Filter_ClosedOpp = subset(Filter_ClosedOpp,Filter_ClosedOpp$Speed %in% input$SelSpeed_Pr1)
     }
     if(!is.null(input$SelRegion_Pr1)){
       Filter_ClosedOpp = subset(Filter_ClosedOpp,Filter_ClosedOpp$Region %in% input$SelRegion_Pr1)
     }
       
       WestDF = Filter_ClosedOpp%>%
        group_by(Winning.Competitor)%>%
         summarise(cnt=median(PerUnitPrice,na.rm=TRUE))
     
     plot_ly(data = Filter_ClosedOpp, y = Filter_ClosedOpp$PerUnitPrice,color = Filter_ClosedOpp$Winning.Competitor, type = "box")%>%
       add_trace(x = WestDF$Winning.Competitor ,y = WestDF$cnt,showlegend=FALSE,text=round(WestDF$cnt,0), mode="text",hoverinfo='none',textposition = "top right") %>%
       layout(yaxis=list(title = "Unit Price (INR)"))
      
     
   })
   output$priceComparison2<- renderPlotly({
     
     #Dt$Topcomp=vapply(strsplit(Dt$Topcomp, ","), function(x) paste(unique(x), collapse = ","), character(1L))
 
     Filter_ClosedOpp=reactPriceAnalysis()
     
     if(!is.null(input$SelLoad_Pr1)){
       Filter_ClosedOpp = subset(Filter_ClosedOpp,Filter_ClosedOpp$Load %in% input$SelLoad_Pr1)
     }
     if(!is.null(input$SelSpeed_Pr1)){
       Filter_ClosedOpp = subset(Filter_ClosedOpp,Filter_ClosedOpp$Speed %in% input$SelSpeed_Pr1)
     }
     if(!is.null(input$SelRegion_Pr1)){
       Filter_ClosedOpp = subset(Filter_ClosedOpp,Filter_ClosedOpp$Region %in% input$SelRegion_Pr1)
     }
     plot_ly(data = Filter_ClosedOpp, y = Filter_ClosedOpp$PerUnitPrice,x=Filter_ClosedOpp$Speed, color = Filter_ClosedOpp$Winning.Competitor, type = "box") %>%
       layout(boxmode = "group") %>%
       layout(xaxis=list(title = "Speed"),yaxis=list(title = "Unit Price (INR)"))
   })
   
   output$tabPriceAnalysis = renderDataTable({
     Dt=reacttabMarkettop5()
     Dt=Dt %>% 
       group_by(Region,Load,Speed)%>%
       arrange(desc(cnt))%>%
       summarise(TopSeller = paste(Winning.Competitor[max(cnt) == cnt], collapse = ","))
     Dt$TopSeller=vapply(strsplit(Dt$TopSeller, ","), function(x) paste(unique(x), collapse = ","), character(1L))
        #   
       if(!is.null(input$SelLoad_Pr1)){
         Dt = subset(Dt,Dt$Load %in% input$SelLoad_Pr1)
       }
     if(!is.null(input$SelSpeed_Pr1)){
       Dt = subset(Dt,Dt$Speed %in% input$SelSpeed_Pr1)
     }
     # if(!is.null(input$SelRegion_Pr1)){
     #   Dt = subset(Dt,Dt$Region %in% input$SelRegion_Pr1)
     # }
      #Dt$`Winning.Competitor's.Bid` = paste(Dt$`Winning.Competitor's.Bid`, Dt$`Winning.Competitor's.Bid.Currency`, sep="")
     columns=c("Region","Load","Speed","TopSeller")
     datatable(Dt[,columns,drop=FALSE],filter="bottom",class = 'cell-border stripe',rownames = FALSE,
               colnames = c('REGION', 'LOAD', 'SPEED','TOP SELLER'))
   })
})
