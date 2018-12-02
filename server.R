#
# Colleen Karnas-Haines
# ST 590-601
# 12/2/2018
# Project 3
library(shiny)
library(shinydashboard)

library("readr")
library(dplyr)
library (tidyr)
library(ggplot2)
library(plotly)
library(tree)
library(class)

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
#Latest nationwide 4-yr college graduation rate
gradRate = 60
collegeDF <- collegeDF %>% mutate(goodGradRate = ifelse(GradRate6yrs>=gradRate, 1,0))
collegeDF$goodGradRate <- as.factor(collegeDF$goodGradRate)
indVars <- c("State","InstitutionType","Math75th","Math25th","Verbal75th",
             "Verbal25th","InStateTuition","OutStateTuition")

numericChoices= c("Math75th","Math25th","Verbal75th",
                  "Verbal25th","InStateTuition","OutStateTuition")


## TEST HERE
train <-sample(1:nrow(collegeDF), size=nrow(collegeDF)*0.8)
test <- dplyr::setdiff(1:nrow(collegeDF), train)
colTrain <-collegeDF[train,]
colTest <- collegeDF[test,]

meanMath75th<-mean(colTrain$Math75th)
sdMath75th<-sd(colTrain$Math75th)
meanMath25th<-mean(colTrain$Math25th)
sdMath25th<-sd(colTrain$Math25th)
meanVerbal75th<-mean(colTrain$Verbal75th)
sdVerbal75th<-sd(colTrain$Verbal75th)
meanVerbal25th<-mean(colTrain$Verbal25th)
sdVerbal25th<-sd(colTrain$Verbal25th)
meanInStateTuition<-mean(colTrain$InStateTuition)
sdInStateTuition<-sd(colTrain$InStateTuition)
meanOutStateTuition<-mean(colTrain$OutStateTuition)
sdOutStateTuition<-sd(colTrain$OutStateTuition)

scaledTestData<-colTest
scaledTestData$Math75th <- (scaledTestData$Math75th-meanMath75th)/sdMath75th
scaledTestData$Math25th <- (scaledTestData$Math25th-meanMath25th)/sdMath25th
scaledTestData$Verbal75th <- (scaledTestData$Verbal75th-meanVerbal75th)/sdVerbal75th
scaledTestData$Verbal25th <- (scaledTestData$Verbal25th-meanVerbal25th)/sdVerbal25th
scaledTestData$InStateTuition <- (scaledTestData$InStateTuition-meanInStateTuition)/sdInStateTuition
scaledTestData$OutStateTuition <- (scaledTestData$OutStateTuition-meanOutStateTuition)/sdOutStateTuition

scaledTrainData<-colTrain
scaledTrainData$Math75th <- (scaledTrainData$Math75th-meanMath75th)/sdMath75th
scaledTrainData$Math25th <- (scaledTrainData$Math25th-meanMath25th)/sdMath25th
scaledTrainData$Verbal75th <- (scaledTrainData$Verbal75th-meanVerbal75th)/sdVerbal75th
scaledTrainData$Verbal25th <- (scaledTrainData$Verbal25th-meanVerbal25th)/sdVerbal25th
scaledTrainData$InStateTuition <- (scaledTrainData$InStateTuition-meanInStateTuition)/sdInStateTuition
scaledTrainData$OutStateTuition <- (scaledTrainData$OutStateTuition-meanOutStateTuition)/sdOutStateTuition
## TEST HERE

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
  
  #Grab some tree work
  getTrees <- reactive({
    set.seed(32)
    collegeDF2 <-collegeDF
    if (!is.element("Public",input$instType)){
      collegeDF2 <- collegeDF2 %>% filter(InstitutionType!=1)
    } 
    if (!is.element("Private non profit", input$instType)){
      collegeDF2 <-collegeDF2 %>% filter (InstitutionType!=2)
    }
    if (!is.element("Private for profit",input$instType)){
      collegeDF2 <-collegeDF2 %>% filter (InstitutionType!=3)
    }
    if (length(input$instType)==0){
      collegeDF2<-collegeDF
    }
    train <-sample(1:nrow(collegeDF2), size=nrow(collegeDF2)*0.8)
    test <- dplyr::setdiff(1:nrow(collegeDF2), train)
    colTrain <-collegeDF2[train,]
    colTest <- collegeDF2[test,]
    trainFit <-tree(goodGradRate~., data=dplyr::select(colTrain, -GradRate6yrs), split="deviance")
    summary(trainFit)
    
    prunedTree <-prune.misclass(trainFit, best=input$ntree)
    predPrune<-predict(prunedTree, newdata=dplyr::select(colTest, -goodGradRate), type="class")
    summary(predPrune)
    plotter <- plot(prunedTree);text(prunedTree)
    jpeg("tree.jpg")
    plot(prunedTree);text(prunedTree)
    dev.off()
    output$image2 <- renderImage({
      jpeg("tree.jpg")
      plot(prunedTree);text(prunedTree)
      dev.off()
      return(list(
        src = "tree.jpg",
        filetype = "image/png",
        alt = "This is our tree"
      ))
    })
    fullTbl <- table(data.frame(predPrune,colTest[,"goodGradRate"]))
    fullTbl
    output$tableTree <- renderDataTable(fullTbl)
    PGoodRate = round(sum(diag(fullTbl)/sum(fullTbl)),4)
    PMisRate <- round(1-PGoodRate,4)
    badword <- paste("Our prediction error is:", PMisRate)
    goodword <-paste("Our good prediciton rate is:", PGoodRate)
    allTheData <- list(plotter, fullTbl, badword, goodword)
    
  })
  
  knnData <- reactive({
    
    knnFit <-knn(train = select(scaledTrainData, input$varChoice),
                 test = select(scaledTestData, input$varChoice),
                 cl = scaledTrainData$goodGradRate,
                 k=5)
    #fitInfo <-tbl_df(data.frame(knnFit, select(scaledTestData, goodGradRate, Math75th, Math25th, Verbal75th, Verbal25th, InStateTuition, OutStateTuition)))
    fitInfo <-tbl_df(data.frame(knnFit, select(scaledTestData, goodGradRate, Name, input$varChoice)))
    fitInfo
    scaledTotal = rbind(scaledTrainData, scaledTestData)
    yourFit <-knn(train = select(scaledTrainData, input$varChoice),
                  test = select(scaledTotal, input$varChoice),
                  cl = scaledTrainData$goodGradRate,
                  k=5)
    yourFitInfo <- arrange(tbl_df(data.frame(yourFit, select(scaledTotal, goodGradRate, Name, input$varChoice))), Name)
    output$knnTable <- renderDataTable(fitInfo)
    output$totesTable <- renderDataTable(yourFitInfo)
    tblKnn <- table(fitInfo$knnFit,fitInfo$goodGradRate)
    tblKnn
    output$successTable <- renderDataTable(tblKnn)
    misClass <- 1-sum(diag(tblKnn))/sum(tblKnn)
    misClass
  })
  
  scaleData <- reactive({
    
    train <-sample(1:nrow(collegeDF), size=nrow(collegeDF)*0.8)
    test <- dplyr::setdiff(1:nrow(collegeDF), train)
    colTrain <-collegeDF[train,]
    colTest <- collegeDF[test,]
    
    meanMath75th<-mean(colTrain$Math75th)
    sdMath75th<-sd(colTrain$Math75th)
    meanMath25th<-mean(colTrain$Math25th)
    sdMath25th<-sd(colTrain$Math25th)
    meanVerbal75th<-mean(colTrain$Verbal75th)
    sdVerbal75th<-sd(colTrain$Verbal75th)
    meanVerbal25th<-mean(colTrain$Verbal25th)
    sdVerbal25th<-sd(colTrain$Verbal25th)
    meanInStateTuition<-mean(colTrain$InStateTuition)
    sdInStateTuition<-sd(colTrain$InStateTuition)
    meanOutStateTuition<-mean(colTrain$OutStateTuition)
    sdOutStateTuition<-sd(colTrain$OutStateTuition)
    
    scaledTestData<-colTest
    scaledTestData$Math75th <- (scaledTestData$Math75th-meanMath75th)/sdMath75th
    scaledTestData$Math25th <- (scaledTestData$Math25th-meanMath25th)/sdMath25th
    scaledTestData$Verbal75th <- (scaledTestData$Verbal75th-meanVerbal75th)/sdVerbal75th
    scaledTestData$Verbal25th <- (scaledTestData$Verbal25th-meanVerbal25th)/sdVerbal25th
    scaledTestData$InStateTuition <- (scaledTestData$InStateTuition-meanInStateTuition)/sdInStateTuition
    scaledTestData$OutStateTuition <- (scaledTestData$OutStateTuition-meanOutStateTuition)/sdOutStateTuition
    
    scaledTrainData<-colTrain
    scaledTrainData$Math75th <- (scaledTrainData$Math75th-meanMath75th)/sdMath75th
    scaledTrainData$Math25th <- (scaledTrainData$Math25th-meanMath25th)/sdMath25th
    scaledTrainData$Verbal75th <- (scaledTrainData$Verbal75th-meanVerbal75th)/sdVerbal75th
    scaledTrainData$Verbal25th <- (scaledTrainData$Verbal25th-meanVerbal25th)/sdVerbal25th
    scaledTrainData$InStateTuition <- (scaledTrainData$InStateTuition-meanInStateTuition)/sdInStateTuition
    scaledTrainData$OutStateTuition <- (scaledTrainData$OutStateTuition-meanOutStateTuition)/sdOutStateTuition
  })
  
  #Working with user entered info
  output$dynamicUserOutputs <- renderUI({
    scaleData()  
    sampleData <- data.frame(Name<-input$userInstName, 
                             Math75th<-input$userMathHigh, Math25th<-input$userMathLow, 
                             Verbal75th<-input$userVerbalHigh, Verbal25th<-input$userVerbalLow,
                             InStateTuition<-input$userInStateTuition,OutStateTuition<-input$userOutStateTuition,
                             goodGradRate<-"")
    sampleData$Math25th<-(sampleData$Math25th-meanMath25th)/sdMath25th
    sampleData$Math75th<-(sampleData$Math75th-meanMath75th)/sdMath75th
    sampleData$Verbal25th<-(sampleData$Verbal25th-meanVerbal25th)/sdVerbal25th
    sampleData$Verbal75th<-(sampleData$Verbal75th-meanVerbal75th)/sdVerbal75th
    sampleData$InStateTuition<-(sampleData$InStateTuition-meanInStateTuition)/sdInStateTuition
    sampleData$OutStateTuition<-(sampleData$OutStateTuition-meanOutStateTuition)/sdOutStateTuition
    
    indFit <-knn(train = select(scaledTrainData, Math75th, Math25th, Verbal75th, Verbal25th, InStateTuition, OutStateTuition),
                 test = select(sampleData, Math75th, Math25th, Verbal75th, Verbal25th, InStateTuition, OutStateTuition),
                 cl = scaledTrainData$goodGradRate,
                 k=2)
    if (indFit[[1]]==0){
      verdict <-("Poor graduation rate")
      output$face <- renderImage({
        
        return(list(
          src = "images/eyebrow-raised.png", 
          height = 150,
          contentType = "image/png",
          alt = "Face with raised eyebrows"
        ))
      },
      deleteFile=FALSE)
    } else{
      verdict<-("Average or better graduation rate")
      output$face <- renderImage({
        
        return(list(
          src = "images/smiling-face.png", 
          height = 150,
          contentType = "image/png",
          alt = "Smiling Face"
        ))
      },
      deleteFile=FALSE)
    }
    tagList(
      h3("User Institution: ", input$userInstName),
      h4(verdict),
      imageOutput("face")
    )
  })
  
  #Working with supervised learning
  
  output$dynamicSLInputs <- renderUI({
    if (input$sl=="tree"){
      titleType="Classification Tree"
      tagList(
        h1(paste("Results of",titleType, sep=" ")),
        sliderInput("ntree",
                    "How many tree nodes would you like?",
                    min = 2,
                    max = 6,
                    value = 2)
      )
      
    }
    else {
      titleType="kNN Analysis"
      numericChoices= c("Math75th","Math25th","Verbal75th",
                        "Verbal25th","InStateTuition","OutStateTuition")
      tagList(
        
        h1(paste("Results of",titleType, sep=" ")),
        h3("Select the variables you are interested in:"),
        h5("(This is a good way to explore which variables are good predictors. As you change which variables are included in the kNN analysis,
           you will see the prediction error rate change.)"),
        checkboxGroupInput("varChoice", "Variable Choice",  choices = (numericChoices), selected="Math75th")
      )
    }
    
  })
  
  getClusters <- reactive({
    #clusterWork <- reactive({
    ##Get the user X and Y and put it here somehow
    
    
    
    if (input$SAT){
      clusterData <- collegeDF %>% filter(Math75th!=0)%>% filter(Verbal75th!=0)%>% filter(Math25th!=0)%>% 
        filter(Verbal25th!=0) %>% select(input$xVar,input$yVar)
      
    } else{
      clusterData <- collegeDF %>% select(input$xVar,input$yVar)
    }
    
    clusFit <- kmeans(clusterData, centers=input$userClusters, iter.max = 10, nstart = 1)
    #,
    #       algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
    #                     "MacQueen"), trace=FALSE)
    
    #})
    library(cluster) 
    #clusplot(clusterData, clusFit$cluster, color=TRUE, shade=TRUE, 
    #         labels=2, lines=0)
    library(fpc)
    #plotcluster(clusterData, clusFit$cluster)
    
    # output$image3 <- renderImage({
    #   jpeg("clusters.jpg")
    #   #attach(clusterData)
    #   plot(clusterData, main="Clusters and Their Centers",
    #        xlab=input$xVar, ylab=input$yVar, col=clusFit$cluster)
    # 
    #   points(clusFit$centers, pch=8, col="blue", cex=10)
    #   dev.off()
    #   return(list(
    #     src = "clusters.jpg",
    #     filetype = "image/jpg",
    #     alt = "These are our clusters"
    #   ))
    # })
    output$clusterPlot <-renderPlot({
      
      attach(clusterData)
      plot(clusterData, main="Clusters and Their Centers", 
           xlab=input$xVar, ylab=input$yVar, col=clusFit$cluster)
      points(clusFit$centers, pch=8, col="blue", cex=10)
    })
    
  })
  
  output$dynamicUSLOutputs <-renderUI({
    getClusters()
    tagList(
      h3("Cluster plots"),
      #imageOutput('image3'),
      plotOutput('clusterPlot')
    )
  })
  
  output$dynamicSLOutputs <- renderUI({
    if (input$sl=="tree"){
      treeStuff <-getTrees()
      
      tagList(
        imageOutput('image2'),
        br(),
        hr(),
        fluidRow(
          column(6,h3(treeStuff[3])),
          
          column(6,
                 h3(treeStuff[4]))
        ),
        hr(),
        
        fluidRow (
          column(6,h4("This table's first row contains the underperforming predictions (worse graduation rate than average). Category 0."),
                 h4("The second row contains the average or higher predictions. Category 1."),
                 h4("The columns are the actual performance categories")),
          column(6,dataTableOutput('tableTree'))
        )
      )
      
    }
    else {
      scaleData()
      misses <-knnData()
      tagList(
        h3("Our prediction error is: ", misses),
        checkboxInput("seeSum", h4("View Predicition Table?")),
        conditionalPanel(
          condition = "input.seeSum",
          hr(),
          h3("This is a summary of hits and misses"),
          
          fluidRow (
            column(6,h4("This table's first row contains the underperforming predictions (worse graduation rate than average). Category 0."),
                   h4("The second row contains the average or higher predictions. Category 1."),
                   h4("The columns are the actual performance categories")),
            column(6,dataTableOutput('successTable'))
          )
          #dataTableOutput('successTable')
        ),
        checkboxInput("seeTestSet",h4("See all the predictions in test group?")),
        conditionalPanel(
          condition="input.seeTestSet",
          h3("This is the complete list of how well the knn predicted for the schools in our test group."),
          dataTableOutput('knnTable')
        ),
        checkboxInput("seeAll", h4("Investigate all schools in dataset?")),
        conditionalPanel(
          condition ="input.seeAll",
          h3("Have fun searching for how your school performed vs. how we predicted it would perform."),
          dataTableOutput('totesTable')
        )
      )
    }
    
  })
  
  
  
  
  })
