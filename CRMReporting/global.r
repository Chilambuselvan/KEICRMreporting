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

if (dir.exists("D:/Official/09_Analytics/KEICRMreporting/CRMReporting/Data"))
{
  setwd("D:/Official/09_Analytics/KEICRMreporting/CRMReporting/Data")
}

if (dir.exists("D:/Reporting/KEICRMreporting/CRMReporting/Data"))
{
  setwd("D:/Reporting/KEICRMreporting/CRMReporting/Data")
}

#setwd("F:/Official/KEICRMreporting/CRMReporting/Data")
if (!exists("OppClosed"))
{
  OppClosed = read.xlsx("closed.xlsx",sheet = 1,startRow = 1, colNames = TRUE,detectDates = TRUE,rowNames = FALSE)
  OppOpen = read.xlsx("open.xlsx",sheet = 1,startRow = 1, colNames = TRUE,detectDates = TRUE,rowNames = FALSE)
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