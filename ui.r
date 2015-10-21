library(shiny)
require(rCharts)

# Define UI 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Solar Installation Payback Analysis"),
  
  # Sidebar with controls to select the variable to plot against 
  sidebarPanel(
    
    # Simple integer interval
    sliderInput("SystemSize", "VA System:",min=0, max=1000000, value=100000),
    sliderInput("MountEquipment", "Mounting Equipment:",min=0, max=10, value=1),
    sliderInput("Labour", "Mounting Labour:",min=0, max=10, value=1),
    sliderInput("Inverter", "Inverter:",min=0, max=10, value=4),
    sliderInput("InverterInstall", "Inverter Installation:",min=0, max=10, value=4),
    sliderInput("LVInstallation", "LV Installation:",min=0, max=10, value=1),
    sliderInput("SwitchGear", "Switch Gear:",min=0, max=10, value=4),
    
    
    # Decimal interval with step value
    sliderInput("OverPanel", "Overpannel Factor:", min = 0, max = 1, value = 0.2, step= 0.01),
    sliderInput("HarvestEff", "Harvest Efficiency:", min = 0, max = 1, value = 0.53, step= 0.01),
    sliderInput("SunnyDays", "Sunny Days:", min = 0, max = 1, value = 0.87, step= 0.01),
    sliderInput("UsageFactor", "Usage Factor:", min = 0, max = 1, value = 0.79, step= 0.01),
    sliderInput("ElecEscalation", "Electricity Escalation:", min = 0, max = 1, value = 0.12, step= 0.01),
   # sliderInput("CPI", "CPI:", min = 0, max = 1, value = 0.05, step= 0.01),
    sliderInput("Interest", "Interest:", min = 0, max = 1, value = 0.095, step= 0.01)
    
  ),
  
  # Show the caption and plot of the requested variable against 
  mainPanel(
    strong("This app takes a pre-designed solar installation and predicts the future break even point in South African Rands."),
    p("The main input on the left being the kVA rating of the system - with other measures, like labour, that also affects the cost."),
    
    plotOutput("outPlot")
  )
))