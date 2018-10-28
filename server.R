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

shinyServer(function(input, output, session) {
  
  output$image1 <- renderImage({

      return(list(
        src = "images/campus.jpg", 
        height = 150,
        contentType = "image/jpg",
        alt = "Campus View"
      ))
  }, deleteFile = FALSE)
  
  getData <- reactive({
    if (input$inOrOut=="in"){
      collegeDF<-collegeDF %>% mutate(Tuition=InStateTuition)
    }
    else{
      collegeDF<-collegeDF %>% mutate(Tuition=OutStateTuition)
    }
    collegeDF <- collegeDF %>% mutate(middleM = (Math75th+Math25th)/2)
    collegeDF <- collegeDF %>% mutate(middleV = (Verbal75th+Verbal25th)/2)
    
    if (input$challenged){
      collegeDF <-collegeDF %>%
        filter(Math75th+200>=input$math)%>% filter(Verbal75th+200>=input$verbal)
    }
    
    newData <- collegeDF %>% filter(State == input$state) %>% filter(Tuition <=input$max) %>%
      filter(middleM<=input$math) %>% filter(middleV<=input$verbal) 
  })
  
  #Data set for the "money" tab
  getData2 <- reactive({
    if (input$inOrOut2=="in"){
      collegeDF<-collegeDF %>% mutate(Tuition=InStateTuition)
    }
    else{
      collegeDF<-collegeDF %>% mutate(Tuition=OutStateTuition)
    }
    
    newData <- collegeDF %>% filter(State %in% input$state2) 

  })
  
  #Data set for the "Data" tab
  getData3 <- reactive({
    newData <- collegeDF %>% select(-Tuition)
  })
  
  
  output$dynamicTitle <- renderUI({
    totalTitle<-paste("Investigation of",input$state, "colleges", sep=" ")
    h1(totalTitle)
  })
  
  #create plot
  output$choicePlot <- renderPlot({
    #get filtered data
    newData <- getData()
    
    #create plot
    g <- ggplot(newData, aes(x = Name, y = GradRate6yrs, fill=InstitutionType))
    g+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(fill="Institution Type")+
      scale_fill_manual(labels = c("Public", "Private NonProfit", "Private forProfit"), values=c("blue","green","orange"))
  })
  
  choiceSavePlot <- reactive({
    #get filtered data
    newData <- getData()
    
    #create plot
    g <- ggplot(newData, aes(x = Name, y = GradRate6yrs, fill=InstitutionType))
    g+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(fill="Institution Type")+
      scale_fill_manual(labels = c("Public", "Private NonProfit", "Private forProfit"), values=c("blue","green","orange"))
  })
  
  #Click to save plot

    output$buttonPlot <- downloadHandler(
      filename = function() {'CollegeSearchPlot.png'},
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 300, units = "in")
        }
        ggsave(file, plot = choiceSavePlot(), device = device)
      })
    
    output$buttonData <- downloadHandler(
      filename = function(){'CollegeSearchData.csv'},
      content = function(file) {
          write.csv(getData(),file)
        })

  
  #create second "money" plot
  #output$moneyPlot <- renderPlot({
  output$moneyPlot <- renderPlotly({
    #get filtered data
    newData <- getData2()
    g <- ggplot(newData, aes(x = Tuition, y = GradRate6yrs, color=State, hoverinfo = Name))
    ggplotly(g+geom_point()+
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
               labs(color="College Location")  
    )
  })
  
  #plot png
  moneySavePlot <- reactive({
    #get filtered data
    newData <- getData2()
    g <- ggplot(newData, aes(x = Tuition, y = GradRate6yrs, color=State, hoverinfo = Name))
    g+geom_point()+
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
               labs(color="College Location")  
  })
  

  
  #Click to save plot
  
  output$buttonMoneyPlot <- downloadHandler(
    filename = function() {'CollegeValuePlot.png'},
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = moneySavePlot(), device = device)
    })
  
  
  
  #create text info
  output$info <- renderText({
  #   #get filtered data
     newData <- getData()
  #   
     paste("With a math SAT score of", input$math, "and a verbal SAT score of", input$verbal, 
      ", your best bet for colleges in", input$state, "under $", input$max, "tuition are listed below. Your SAT scores are higher or
      equal to the average SAT scores at these colleges. If you selected the 'challenged' checkbox, we elimintaed any schools whose
    75th percentile SAT scores were more than 200 points lower than your score. Your academic performance is too far above their student population.", sep = " ")
   })
  
  #create text info2
  output$info2 <- renderText({
    #   #get filtered data
    newData <- getData2()
    #create some variables with math
    medTuition <- round(mean(newData$Tuition))
    lowestData <- newData %>% filter(Tuition <= medTuition)
    highestData <- newData %>% filter(Tuition >=medTuition)
    lowestTGR <-round(mean(lowestData$GradRate6yrs))
    highestTGR <-round(mean(highestData$GradRate6yrs))
    
    #   
    paste0("The average yearly tuition for all the schools you are looking at is $",medTuition, ". The average
           6-yr graduation rate (bachelor's only) for the schools in the lowest 50% tuition bracket is ", lowestTGR, "%. The average 6-yr graduation rate
            (bachelor's only) for the schools in the highest 50% tuition bracket is ", highestTGR, "%. Is the higher price worth it?")
  })
  
  #create output of observations    
  output$table <- renderTable({
    getData()%>% rename("Avg Math SAT"="middleM", "Avg Verbal SAT"="middleV") %>% select("Name","State","Tuition","Avg Math SAT","Avg Verbal SAT")
  })
  
  #create second "worth" output of observations    
  output$moneytable <- renderTable({
    getData2()%>%  select("Name","State","Tuition","GradRate6yrs")
  })
  
  #create third "data" output of observations    
  output$datatable <- renderTable({
    getData3()
  })
  
  output$buttonTotalData <- downloadHandler(
    filename = function(){'CollegeTotalData.csv'},
    content = function(file) {
      write.csv(getData3(),file)
    })
  
})
