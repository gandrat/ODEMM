library(shiny)
library(DiagrammeR)
library(stringr)
library(readODS)
library(dplyr)

rm(list=ls()) #removing previous objects

packageVersion("base")
options(max.print=1000000000)

# Load df -----------
# Saved after 00_preprocess.R script
load("Data/SBS_v3.Rda")

data<-data%>%select(-ideco,-idsec, -idpressure)

data$Links = paste("'", data$Sector, "'", " -> ", "'", data$Pressure, "'", " -> ", "'", data$Ecological.Characteristic, "'")

data$key = paste0(data$Sector, data$Pressure, data$Ecological.Characteristic)


########################################################################
####   Function to produce plot
graph_obj <- function(data, InSector, InPressure, InEco, method, percent){
  
    ### Switch between different percentage linkages
  ### What number do we need to divide by to get that percentage of links
  PercentFactor =   if (percent == "All"){
    1
  }  else if (percent == "10%"){
    10
  } else if (percent == "20%"){
    5
  } else if (percent == "50%"){
    2
  }
  
  ### Function to perform the calculations of the linkage chains once the correct data set is selected
  RiskFilter = function (data){
    # AllRisk = sum (data[ , 'ImpactRisk'],na.rm=T)
    # data$relRisk = data[ , 'ImpactRisk']/AllRisk
    data = data[order (-data$ImpactRisk), ]
    data = data [1:(nrow(data)/PercentFactor), ]
    subset(data) 
  }
  
  data1 = data
  
  ### For the highlighted linkages (those that will be red), filter depending on selection boxes
  dataselected = if (InSector == "All Sectors" & InPressure == "All Pressures" & InEco == "All") {
    RiskFilter(data1)
  } else if (InSector != "All Sectors" & InPressure == "All Pressures" & InEco == "All") {
    data1 = subset (data1, data1$Sector == InSector)
    RiskFilter(data1)
  } else if (InSector != "All Sectors" & InPressure != "All Pressures" & InEco == "All") {
    data1 = subset (data1, data1$Sector == InSector & data1$Pressure == InPressure)
    RiskFilter(data1)
  } else if (InSector != "All Sectors" & InPressure != "All Pressures" & InEco != "All") {
    data1 = subset (data1, data1$Sector == InSector & data1$Pressure == InPressure & data1$Ecological.Characteristic == InEco)
    RiskFilter(data1)
  } else if (InSector != "All Sectors" & InPressure == "All Pressures" & InEco != "All") {
    data1 = subset (data1, data1$Sector == InSector & data1$Ecological.Characteristic == InEco)
    RiskFilter(data1)
  } else if (InSector == "All Sectors" & InPressure != "All Pressures" & InEco != "All") {
    data1 = subset (data1, data1$Pressure == InPressure & data1$Ecological.Characteristic == InEco)
    RiskFilter(data1)
  } else if (InSector == "All Sectors" & InPressure == "All Pressures" & InEco != "All") {
    data1 = subset (data1, data1$Ecological.Characteristic == InEco)
    RiskFilter(data1)
  } else if (InSector == "All Sectors" & InPressure != "All Pressures" & InEco == "All") {
    data1 = subset (data1, data1$Pressure == InPressure)
    RiskFilter(data1)
  }
  
  data$Selected = ifelse (data$key %in% dataselected$key, 1 , 2)
  data = data[order (data$Selected), ]
  
  
  data$Colour = ifelse (data$key %in% dataselected$key, 
                        paste0 (data$Links, " [penwidth = 20, color = red]; ", sep = " "),
                        paste0 (data$Links, " [penwidth = 2, color = black];", sep = " "))  
  
  Links = subset(data, select = Colour)
  LinksAll <- capture.output(print(Links, row.names = FALSE))[-1]
  LinksAll2 <- paste(LinksAll,"", collapse= " " )
  
  ######################
  #### Which nodes should be highlighted
  
  SelectedNodes = if (nrow(data) != nrow(dataselected)){
    dataselected$Sector2 = paste("'", dataselected$Sector, "';")
    dataselected$Pressure2 = paste("'", dataselected$Pressure, "';")
    dataselected$Ecological.Characteristic2 = paste("'", dataselected$Ecological.Characteristic, "';")
    
    
    NodesSector = unique(subset(dataselected, select = Sector2))
    NodesPressure = unique(subset(dataselected, select = Pressure2))
    NodesEco = unique(subset(dataselected, select = Ecological.Characteristic2))
    
    NodesSector2 <- capture.output(print(NodesSector, row.names = FALSE))[-1]
    NodesPressure2 <- capture.output(print(NodesPressure, row.names = FALSE))[-1]
    NodesEco2 <- capture.output(print(NodesEco, row.names = FALSE))[-1]

    NodesSector3 <- paste(NodesSector2,"", collapse= " " )
    NodesPressure3 <- paste(NodesPressure2,"", collapse= " " )
    NodesEco3 <- paste(NodesEco2,"", collapse= " " )
    
    paste(NodesSector3, NodesPressure3, NodesEco3)
  } else { " "}
  
  ########## create object to make plot
  obj <- paste0("digraph{ 
                graph [bgcolor='white'; 
                overlap=true;
                ratio=auto; 
                rankdir=LR;
                concentrate=true]
                
                node [fontname=Helvetica,shape=box, fontsize =150, style=bold, style = filled, color = black, penwidth = 10,fillcolor = yellow]                
                {" ,SelectedNodes, " }
                
                node [fontname=Helvetica,shape=box, fontsize =150, style=bold, style = empty, color = black, penwidth = 10]
                
                
                
                
                nodesep=1.5 // increases the separation between nodes
                ranksep= 25
                
                edge [arrowhead = none]
                ",LinksAll2,"
}", sep= " ")
  
}

#write (obj, "H:\\ODEMM\\Analysis\\Network Plot\\Horrendogram\\objline.dot" )


## ui.R ----
# Step 1:
# Define UI for app  ----
ui <- fluidPage(
  # App title ----
  titlePanel("South Brazillian Shelf ODEMM"),
  
  # Top Selection with input and output definitions ----
  # Top Selection with input and output definitions ----
  fluidRow(
    column(2,offset = .2,
           # Input: dropdown for each category ----
           selectInput(inputId = "Sector",
                       label = "Choose a 
                       sector:",
                       choices=append('All Sectors',sort(as.character(data$Sector)))
           )),
    
    column(2,offset = 0.2,
           # Input: dropdown for each category ----
           selectInput(inputId = "Pressure",
                       label = "Choose a 
                       pressure:",
                       choices=append('All Pressures',sort(as.character(data$Pressure)))
           )),
    
    column(2,offset = 0.2,
           # Input: dropdown for each category ----
           selectInput(inputId = "Ecological",
                       label = "Ecological characteristic:",
                       choices=append('All',sort(as.character(data$Ecological.Characteristic)))
           )),
    
 
    column(2, offset = 0.2,
           selectInput(inputId = "Percent",
                       label = "Top Percentage of Links:",
                       c('All' = "All",
                         '10%' = '10%',
                         '20%' = '20%',
                         '50%' = '50%'))),
    
    
    
    # Main panel for displaying outputs ----
    fluidRow(
      
      # Output: horrendogram ----
      grVizOutput(outputId = "horrendogram", width = "95%", height = "700px")
    )
  ))

### server.R ---
# Step 2:
# Define plot information (turn inputs into outputs)  ----
server <- function (input, output){
  
  output$horrendogram <- renderGrViz(
    grViz(
      graph_obj(data, input$Sector, input$Pressure, input$Ecological, input$Method, input$Percent)
    )
  )
  
}

# Step 3: 
# knit UI and server together

shinyApp (ui=ui, server = server)