library(shiny)
library(lubridate)
library(tvm)
require(ggplot2)
require(scales)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) 
{
  
  # Variables entered #
  output$outPlot <- renderPlot({
    
  SystemSize <- input$SystemSize
  OverPanel <- input$OverPanel
  HarvestEff <- input$HarvestEff
  SunnyDays <- input$SunnyDays
  UsageFactor <- input$UsageFactor
  ElecEscalation <- input$ElecEscalation
#  CPI <- input$CPI
  Interest <- input$Interest
  
  MountEquipment <- input$MountEquipment
  Labour <- input$Labour
  
  Inverter <- input$Inverter
  InverterInstall <- input$InverterInstall
  LVInstallation <- input$LVInstallation
  LVInstallation <- input$LVInstallation
  SwitchGear <- input$SwitchGear

  ##
  
  
  #Calculations #
  Panels <- SystemSize/250*(1+OverPanel)
  PanelPrice <- 8*250
  PanelPriceTotal <- Panels*PanelPrice
  
  MountEquipmentPrice <- PanelPriceTotal/5
  MountEquipmentPriceTotal <- MountEquipment*MountEquipmentPrice
  MountingLabourPrice <- 1.5*SystemSize
  MountingLabourPriceTotal <- MountingLabourPrice*Labour
  
  InverterPrice <- 96657*0.9
  InverterPriceTotal <- Inverter*InverterPrice
  InverterInstallPrice <- 10000*1.2
  InverterInstallPriceTotal <- InverterInstall*InverterInstallPrice
  
  PanelsPerString <- round(1000/38, digits =0)
  Strings <- Panels/PanelsPerString
  CablePerString <- 0 #in meters
  CableForString <- Strings*2*2*Strings/2 #in meters maar calculation maak nie sin nie
  
  StringCabling <- CableForString
  StringCablingPrice <- 17*1.3
  StringCablingPriceTotal <- StringCabling*StringCablingPrice
  
  LVCabling <- Inverter*80
  LVCablingPrice <- 100*1.2
  LVCablingPriceTotal <- LVCablingPrice*LVCabling
  LVInstallationPrice <- 60000*1.2
  LVInstallationPriceTotal <- LVInstallationPrice*LVInstallation
  
  SwitchGearPrice <- 15000*1.3
  SwithGearPriceTotal <- SwitchGear*SwitchGearPrice
  
  ##
  
  #Results of Panel Calculations
  kWhPerDay <- SystemSize/1000*(1+OverPanel)*HarvestEff*8
  SunnyToUsableDays <- 365*SunnyDays*UsageFactor
  kWhPerYear <- kWhPerDay*SunnyToUsableDays
  RandPerkWh <- 1.35
  RandPerYear <- kWhPerYear*RandPerkWh
  
  #Results of Financials
  TotalExpenditureYear1 <- PanelPriceTotal+MountEquipmentPriceTotal+MountingLabourPriceTotal+InverterPriceTotal+InverterInstallPriceTotal+StringCablingPriceTotal+LVCablingPriceTotal+LVInstallationPriceTotal+SwithGearPriceTotal
  TotalExpenditureYear1 <- -1*TotalExpenditureYear1
  
  CurrentYear <- year(here())
  YearsEntry <- c(CurrentYear,CurrentYear+1,CurrentYear+2,CurrentYear+3,CurrentYear+4,CurrentYear+5,CurrentYear+6,CurrentYear+7,CurrentYear+8,CurrentYear+9,CurrentYear+10,CurrentYear+11,CurrentYear+12,CurrentYear+13,CurrentYear+14)
  Income <- c(RandPerYear,RandPerYear*(1+ElecEscalation),RandPerYear*(1+ElecEscalation)^2,RandPerYear*(1+ElecEscalation)^3,RandPerYear*(1+ElecEscalation)^4,RandPerYear*(1+ElecEscalation)^5,RandPerYear*(1+ElecEscalation)^6,RandPerYear*(1+ElecEscalation)^7,RandPerYear*(1+ElecEscalation)^8,RandPerYear*(1+ElecEscalation)^9,RandPerYear*(1+ElecEscalation)^10,RandPerYear*(1+ElecEscalation)^11,RandPerYear*(1+ElecEscalation)^12,RandPerYear*(1+ElecEscalation)^13,RandPerYear*(1+ElecEscalation)^14)
  Expenses <- c(TotalExpenditureYear1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  CummIncome <- c(Income[1],Income[1]+Income[2],Income[2]+Income[3],Income[2]+Income[3]+Income[4],Income[2]+Income[3]+Income[4]+Income[5],Income[2]+Income[3]+Income[4]+Income[5]+Income[6],Income[2]+Income[3]+Income[4]+Income[5]+Income[6]+Income[7],Income[2]+Income[3]+Income[4]+Income[5]+Income[6]+Income[7]+Income[8],Income[2]+Income[3]+Income[4]+Income[5]+Income[6]+Income[7]+Income[8]+Income[9],Income[2]+Income[3]+Income[4]+Income[5]+Income[6]+Income[7]+Income[8]+Income[9]+Income[10],Income[2]+Income[3]+Income[4]+Income[5]+Income[6]+Income[7]+Income[8]+Income[9]+Income[10]+Income[11],Income[2]+Income[3]+Income[4]+Income[5]+Income[6]+Income[7]+Income[8]+Income[9]+Income[10]+Income[11]+Income[12],Income[2]+Income[3]+Income[4]+Income[5]+Income[6]+Income[7]+Income[8]+Income[9]+Income[10]+Income[11]+Income[12]+Income[13],Income[2]+Income[3]+Income[4]+Income[5]+Income[6]+Income[7]+Income[8]+Income[9]+Income[10]+Income[11]+Income[12]+Income[13]+Income[14],Income[2]+Income[3]+Income[4]+Income[5]+Income[6]+Income[7]+Income[8]+Income[9]+Income[10]+Income[11]+Income[12]+Income[13]+Income[14]+Income[15]) #Year 3 onwards het nie year 1 income in nie?
  CashFlow <- Expenses+Income
  TaxDeduction <- c((TotalExpenditureYear1/3*0.28),(TotalExpenditureYear1/3*0.28),(TotalExpenditureYear1/3*0.28),0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  
  #Building up the cummulative cash flows
  CummCashFlow1 <- TotalExpenditureYear1+Income[1]+TaxDeduction[1]
  CummCashFlow2 <- CummCashFlow1+Income[2]+TaxDeduction[2]
  CummCashFlow3 <- CummCashFlow2+Income[3]+TaxDeduction[3]
  CummCashFlow4 <- CummCashFlow3+Income[4]+TaxDeduction[4]
  CummCashFlow5 <- CummCashFlow4+Income[5]+TaxDeduction[5]
  CummCashFlow6 <- CummCashFlow5+Income[6]+TaxDeduction[6]
  CummCashFlow7 <- CummCashFlow6+Income[7]+TaxDeduction[7]
  CummCashFlow8 <- CummCashFlow7+Income[8]+TaxDeduction[8]
  CummCashFlow9 <- CummCashFlow8+Income[9]+TaxDeduction[9]
  CummCashFlow10 <- CummCashFlow9+Income[10]+TaxDeduction[10]
  CummCashFlow11 <- CummCashFlow10+Income[11]+TaxDeduction[11]
  CummCashFlow12 <- CummCashFlow11+Income[12]+TaxDeduction[12]
  CummCashFlow13 <- CummCashFlow12+Income[13]+TaxDeduction[13]
  CummCashFlow14 <- CummCashFlow13+Income[14]+TaxDeduction[14]
  CummCashFlow15 <- CummCashFlow14+Income[15]+TaxDeduction[15]
  CummCashFlow <- c(CummCashFlow1,CummCashFlow2,CummCashFlow3,CummCashFlow4,CummCashFlow5,CummCashFlow6,CummCashFlow7,CummCashFlow8,CummCashFlow9,CummCashFlow10,CummCashFlow11,CummCashFlow12,CummCashFlow13,CummCashFlow14,CummCashFlow15)
  #######
  
  #NPVs
  NPVResult1 <- xnpv(i=Interest,cf=CashFlow[1],d=YearsEntry[1])
  NPVResult2 <- xnpv(i=Interest,cf=CashFlow[1:2],d=YearsEntry[1:2])
  NPVResult3 <- xnpv(i=Interest,cf=CashFlow[1:3],d=YearsEntry[1:3])
  NPVResult4 <- xnpv(i=Interest,cf=CashFlow[1:4],d=YearsEntry[1:4])
  NPVResult5 <- xnpv(i=Interest,cf=CashFlow[1:5],d=YearsEntry[1:5])
  NPVResult6 <- xnpv(i=Interest,cf=CashFlow[1:6],d=YearsEntry[1:6])
  NPVResult7 <- xnpv(i=Interest,cf=CashFlow[1:7],d=YearsEntry[1:7])
  NPVResult8 <- xnpv(i=Interest,cf=CashFlow[1:8],d=YearsEntry[1:8])
  NPVResult9 <- xnpv(i=Interest,cf=CashFlow[1:9],d=YearsEntry[1:9])
  NPVResult10 <- xnpv(i=Interest,cf=CashFlow[1:10],d=YearsEntry[1:10])
  NPVResult11 <- xnpv(i=Interest,cf=CashFlow[1:11],d=YearsEntry[1:11])
  NPVResult12 <- xnpv(i=Interest,cf=CashFlow[1:12],d=YearsEntry[1:12])
  NPVResult13 <- xnpv(i=Interest,cf=CashFlow[1:13],d=YearsEntry[1:13])
  NPVResult14 <- xnpv(i=Interest,cf=CashFlow[1:14],d=YearsEntry[1:14])
  NPVResult15 <- xnpv(i=Interest,cf=CashFlow[1:15],d=YearsEntry[1:15])
  NPVResult <- c(NPVResult1,NPVResult2,NPVResult3,NPVResult4,NPVResult5,NPVResult6,NPVResult7,NPVResult8,NPVResult9,NPVResult10,NPVResult11,NPVResult12,NPVResult13,NPVResult14,NPVResult15)
  ###############
  
  # Generate a plot of the requested variable against 

  NPVData <- data.frame(matrix(unlist(NPVResult)),stringsAsFactors = FALSE)
  NPVData <- cbind(NPVData,YearsEntry)
  NPVData <- setNames(NPVData,c("NPVResult","Year"))
  outPlot <- ggplot(data=NPVData, aes(x=Year, y=NPVResult, group=1)) + geom_line()
  outPlot <- outPlot  + scale_y_continuous(labels = comma) + scale_x_continuous(breaks=YearsEntry) +labs(x="Year",y="Cash flow in Rands")
  outPlot <- outPlot + geom_hline(aes(yintercept=0), colour="#BB0000", linetype="dashed")
  outPlot
  
  })
})