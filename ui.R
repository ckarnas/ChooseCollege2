#
# Colleen Karnas-Haines
# ST 590-601
# 10/25/2018
# First Success
library(shiny)
library(shinydashboard)

library("readr")
library(dplyr)
library (tidyr)
library(ggplot2)
library(plotly)

#non reactive stuff here

newNames <-c("ID","Name","State","InstitutionType","Math75th","Math25th","Verbal75th",
             "Verbal25th","InStateTuition","OutStateTuition","GradRate6yrs")
#collegeData <- read_csv(file="NCState/ShinyApps/CollegeApp/CollegeApp/ipedsData/ipedsData.csv", col_names=newNames, skip=1)
collegeData <- read_csv(file="ipedsData/ipedsData.csv", col_names=newNames, skip=1)

#print(collegeData)
#make into a tble
collegeDF<-tbl_df(collegeData)

#rows with no tuition data are removed
collegeDF <-collegeDF %>% replace(is.na(.),0) %>% filter(OutStateTuition!=0)%>% filter(InStateTuition!=0)

#default to tuition being in-state tuition prices
collegeDF <- collegeDF %>% mutate(Tuition=InStateTuition)
collegeDF$InstitutionType <- as.factor(collegeDF$InstitutionType)

shinyUI(fluidPage(themes="sandstone",
                  #**
                  tabsetPanel(
                    tabPanel("Information", icon=icon("th"), fluid=TRUE,
                             h1("College Decision Tool", style="color:green; font-family:BentonSans Book"),
                             br(),
                             div("In this project we will explore data from the Integrated
                                 Postsecondary Education Data System. The data is collected by the
                                 U.S. Department of Education. Any higher education institution that
                                 wants to be eligible to receive federal student aid programs is required
                                 to participate in this yearly survey."),
                             br(),
                             div("The data available goes back many years and is collected from thousands of schools.
                                 For this project we took the most recent complete and confirmed data from the 2016-2017
                                 school year. We only looked at degree-granting, primarily 4-yr institutions within the US."),
                             br(),
                             div("The variables we collected were: "),
                             tags$ol(tags$li("Unique ID for each school"),tags$li ("Name,"), tags$li ("State (categorical),"), 
                                     tags$li("Institution control (categorical: public, private non-profit, or private for-profit),"), tags$li("Math SAT score 25th percentile,"),
                                     tags$li("Math SAT score 75th percentile,"), tags$li("Verbal SAT score 25th percentile,"), tags$li("Verbal SAT
                                 score 75th percentile,"), tags$li("In-state tuition,"), tags$li("Out-of-state tuition,"), tags$li("The percent of
                                 student who graduate 4 year programs within 6 years.")),
                             
                             #Link to data
                             h3(a("IPEDS data source", href="https://nces.ed.gov/ipeds/use-the-data")),
                             
                             
                             imageOutput("image1"),
                             div("Author: Colleen J. Karnas-Haines, PhD"),
                             div("To satisfy course requirement of NC State's St 590-601, 'R and Data Science'")
  
                             ),
                
                  
                  #**
                  tabPanel("College Search", icon=icon("institution"),fluid=TRUE,
                  
  
  
  # Application title
  #titlePanel("Investigation of Mammal Sleep Data"),
  titlePanel (uiOutput("dynamicTitle")),
  # Sidebar with options for the data set
  sidebarLayout(
    sidebarPanel(
      
      
      
      
      h3("Select the state you are interested in:"),
      selectInput("state", "State", selected = "NC", choices = levels(as.factor(collegeDF$State))),
      selectInput("inOrOut", "In-state or Out-of-state?", c("In-state"="in","Out-of-state"="out")),

      #Sidebar with a slider input for maximum yearly tuition
      if ("input.inOrOut"=="in"){
        sliderInput("max",
                    "Maximum amount of tuition:",
                    min = 1000,
                    max = max(collegeDF$InStateTuition),
                    value = 5000)
        
      }
      else {
        sliderInput("max",
                    "Maximum amount of tuition:",
                    min = 1000,
                    max = max(collegeDF$OutStateTuition),
                    value = 5000)
        
      },
      br(),
      checkboxInput("challenged","Would you like to be challenged academically?"),
      
      numericInput("math","Enter your Math SAT score:", value=400),
      numericInput("verbal","Enter your Verbal SAT score:", value=400)
    ),
    
    # Show outputs
    mainPanel(
      h3("Visual Display of Your Options:"),
      plotOutput("choicePlot"),
      downloadButton('buttonPlot', "Save this graph"),
      #actionButton("buttonPlot", "Save this graph"),
      downloadButton('buttonData', "Save the data"),
      h3("Advice:"),
      textOutput("info"),
      h3("Output Table:"),
      tableOutput("table")
    )
    
    #Closes sidebar layout
    )
  
  #closes the smaller tabPanel
  ),
  
  tabPanel("College Worth", icon=icon("money"),fluid=TRUE,
           
           
           
           #Title
           titlePanel ("Will a more expensive college help you graduate?"),
           # Sidebar with options for the data set
           sidebarLayout(
             sidebarPanel(
               
               
               
               
               h3("Select the state you are interested in:"),
               checkboxGroupInput("state2", "State",  choices = levels(as.factor(collegeDF$State))),
               
               selectInput("inOrOut2", "In-state or Out-of-state?", c("In-state"="in","Out-of-state"="out")),

               br()

             ),
             
             # Show outputs
             mainPanel(
               plotlyOutput("moneyPlot"),
               downloadButton('buttonMoneyPlot', "Save this graph"),
               h3("Averages", align="center"),
               #special symbol using MATHJAX
               h1(withMathJax(helpText("$$\\bar{x}$$"))),
               textOutput("info2"),
               tableOutput("moneytable")
             )
             
             #Closes sidebar layout
           )
           
           #closes the smaller tabPanel
  ),
  
  tabPanel("Data", icon=icon("list"), fluid=TRUE,
           
           downloadButton('buttonTotalData', "Save the data"),
           h1("The Data Used:"),
           tableOutput("datatable")
           
           )
  
  #Closes the larger tabsetPanel
  )
))