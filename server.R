source("helpers.R")

library(shiny)
library(DT)
library(caret)
library(tidyverse)
library(plotly)
library(corrplot)
library(imager)
library(shinydashboard)
library(summarytools)
library(rpart.plot)
library(randomForest)


# Define server logic 
shinyServer(function(input, output, session) {

    #creates a reactive context for the data set
    getApartmentData <- reactive({
        ApartmentData1 <- apartmentData %>% filter(between(sqft_size, input$sqft_size1[1], input$sqft_size1[2]) & 
                                                     between(YearBuilt, input$YearBuilt1[1], input$YearBuilt1[2]))
    })

    # create plots and summary tables
    
    # Histogram - Apartment price
    histogram <- function(){

        h <- plot_ly(getApartmentData(), x= ~sale_price, nbinsx = input$numberofbins ) %>% 
                add_histogram() 
        }
    
    output$histogram <- renderPlotly({
        histogram()
    })
    
    # Box plot - sale history
    boxPlot <- function(){
        p <- plot_ly(getApartmentData(), y= ~sale_price, color = I("blue"), alpha = 0.1, boxpoints = "suspectedoutliers")
             boxPlot1 <- if (input$sales == 'YearSold'){
                p %>% add_boxplot(x = ~YearSold)
             } else {
                p %>% add_boxplot(x = ~MonthSold)
             }
        }
    output$boxPlot <- renderPlotly({
          boxPlot()
    })
    
    # renderUI() and uiOutput("infoSale")
    output$infoSale <- renderUI({
      text1 <- paste0("You have selected to veiw box plots showing how apartment prices change by ", input$sales)
      h3(text1)
    })
    
    # Create text info
    output$info <- renderText({
      paste("First, looking at the graph by year the apartment was sold, the apartment prices have changed very slowly but overal have increased over 10 years (2007 to 2017).
            Interestingly, in 2009 and 2010, the overall housing price went down in Korea due to the global economic instability and interest rate increases. 
            Therefore, we can infer that the apartment prices in Daegu area were also affected by the Korean housing market.
            Second, the month of sale graph shows that apartment prices don't show much of a pattern.
            The lowest median price of apartments was in January, and the higest median price was in July.")
    })
    
    # Create summary table
    output$saleSummaryTable <- renderDataTable({
      
      getApartmentData() %>% 
        group_by_at(input$sales) %>%
        summarise(
          Min = min(sale_price), 
          Max = max(sale_price),
          Median = median(sale_price),
          Average = round(mean(sale_price),2))
    })
    
    # Bar plot - Access to subway station
    barPlot <- function(){
        p1 <- getApartmentData() %>% count(accessToSubwaySTN) %>% plot_ly(x = ~accessToSubwaySTN, y = ~n ) %>%
        add_bars()
        } 
    output$barPlot = renderPlotly({
        barPlot()
    })

    #create summary table
    output$summarytable <- renderDataTable({
      t <- getApartmentData() %>%
        group_by_at(input$transportation) %>%
        summarise(Min = min(sale_price), 
                  Max = max(sale_price),
                  Median = median(sale_price),
                  Average = round(mean(sale_price),2)
                  )
      DT::datatable(t, caption = "Summary statistics of apartment price by access of subway station")
    })
    
    # Select variables
    scatterData <- reactive({
      data <- getApartmentData() %>% select(input$numericalVarNames)
    }) 
    
    # Scatter plot - Apartment features and facts
    output$scatterPlot = renderPlotly({
      # Geom_smooth option
      if(!input$geomline){
        scatterPlot = ggplot(scatterData(),aes_string(x=input$featuresX, y=input$featuresY))+
          geom_point(color="blue")
        ggplotly(scatterPlot)
      }else{
        scatterPlot = ggplot(scatterData(),aes_string(x=input$featuresX, y=input$featuresY))+
          geom_point(color="orange")+geom_smooth(se=TRUE, color="blue")
        ggplotly(scatterPlot)
      }
    })
    
    # renderUI() for uiOutput("infoNew")
    output$infoNew <- renderUI({
      text <- paste0("You have selected to view a scatter plot of ", input$featuresX, " feature on ", input$featuresY)
      h3(text)
    })

    # Create reactive data for numerical summary
    summaryData <- reactive({
      data <- getApartmentData() %>% select(input$numericalVarNames)
    }) 
    
    # Numerical summary tables for numerical variables for Apartment features option
    output$numSummaryTable <- renderDataTable({
      m <- descr(scatterData(), stats = c("mean", "sd", "min", "q1", "med", "q3", "max", "iqr"), round.digits = st_options("round.digits"), order = "preserve",
                 transpose = FALSE)
      class(m) <- "matrix"
      m %>% as_tibble(rownames="Statistic")
      DT::datatable(m, caption = "Summary statistics of apartment features",options = list(scrollX = 't')) %>% formatRound(columns = c(1:11), digits = 2) 
    })
        
    # Correlation plot-  the correlation matrix for the numerical variables   
    output$corrplot = renderPlot({
      df_tmp <- getApartmentData() %>% dplyr::select(all_of(numericalVarNames))
      
      corrplot(cor(df_tmp), type = 'lower', diag = FALSE)
    })
    
    # Create text info
    output$info1 <- renderText({
      paste("We use this correlation plot to find the variables that are highly correlated and avoid including the variables in our model building phase.")
    })
    
    # Modeling tab
    
    # This data will be used for running the models. Three numeric variables were selected based on the correlation plots.
    modelData <- reactive({
      newmodelData <- apartmentData %>% select(sale_price, input$predictors)
    })
    
    # Split data into train and test
    trainsplit <- reactive({
      set.seed(123) # For reproducibility
      train = sample(1:nrow(modelData()), size=nrow(modelData())*input$proportion)
    })
    traindata <- reactive({
      train_data = modelData()[trainsplit(), ]
    })
    
    # Test data
    test <- reactive({
      set.seed(123)
      test = dplyr::setdiff(1:nrow(modelData()), trainsplit())
    })
    testdata <- reactive({
      test_data = modelData()[test(), ]
    })

    # Display predict results on train set
    
    # Train multiple regression model
    mlrFit <- reactive({
      set.seed(123)
      mlr.fit <- train(if(!input$interaction){
                           sale_price ~ .
                         }else{
                            sale_price ~ .*.
                          },
                      data = traindata(),
                      method = 'lm',
                      preProcess = c("center", "scale"), 
                      trControl = trainControl(method = "repeatedcv", number = as.numeric(input$numcv), repeats = 3)) 
    }) 
    
    # Multiple regression model outcome
    observeEvent(input$reportTrain,
                 output$mlrmodelfit <- renderTable({
                   withProgress(message = "In progress- Multiple Linear Regression", value = NULL,{
                   print(mlrFit()$results[2:7])
                   })
                 })
    )
    
    # ANOVA table for multiple linear regression outcome
    observeEvent(input$reportTrain,
                 output$summaryMLRTable <- renderTable(
                  
                 as.data.frame(anova(lm(if(!input$interaction){
                                            sale_price ~ .
                                         }else{
                                             sale_price ~ .*.
                                          }, 
                                        data = traindata()
                                        )
                                     )
                 )
    ))
    
    # Regression tree model
    rtmFit <- reactive({      
      set.seed(123)
      rtmFit1 <- train(sale_price ~., 
                          data = traindata(),
                          method = 'rpart', 
                          preProcess = c("center", "scale"), 
                          trControl = trainControl(method = "repeatedcv", number = as.numeric(input$numcv), repeats = 3))
    })
    
    # Regression Tree - model fit on train
    observeEvent(input$reportTrain,
                 output$rtmodelfit <- renderPrint({
                   withProgress(message = "In progress- Regression Tree", value = NULL,{
                   print(rtmFit())
                   })
                 })
    )
 
    # Regression tree plot
    observeEvent(input$reportTrain,
                 output$rtplot <- renderPlot({
                   rtp <- rpart(sale_price ~., data  = traindata())
                 rpart.plot(rtp)
                 }) 
                   )
    
    # Random forest model
    rfmFit <- reactive({
      set.seed(123)
      rfmFit1 <-   train(sale_price ~.,
                          data = traindata(),
                          method = 'rf',
                          preProcess = c("center", "scale"),
                          trControl = trainControl("cv", number = as.numeric(input$numcv)),
                          tuneGrid = data.frame(mtry = (1:4))
                         ) 
    })
    
    # Random forest fit on train set
    observeEvent(input$reportTrain,
                 output$rfmodelfit <- renderTable({
                   withProgress(message = "In progress- Random Forest", value = NULL,{
                   print(rfmFit()$results)
                   })
                 })
    )
    
    # Variable importance plot
    observeEvent(input$reportTrain,
                 output$rfplot <- renderPlot({
                      Variable_Importance <- randomForest(sale_price ~., data = traindata(), ntree=1000, importance=TRUE)
                 varImpPlot(Variable_Importance)
                 })
    )
    
    # Display predict results on test set
    # Multiple linear regression model on test set
    observeEvent(input$reportTest,
                 output$mlrmodelTest <- renderTable(
                   print(t(postResample(predict(mlrFit(), newdata = testdata()), obs = testdata()$sale_price)))
                 )
    )
    # Regression tree model on test set       
    observeEvent(input$reportTest,
                 output$rtmodelTest <- renderTable(
                   print(t(postResample(predict(rtmFit(), newdata = testdata()), obs = testdata()$sale_price)))
                 )
    )
    
    # Random forest on test set   
    observeEvent(input$reportTest,
                 output$rfmodelTest <- renderTable(
                   print(t(postResample(predict(rfmFit(), newdata = testdata()), obs = testdata()$sale_price)))
                 )
    )
    
    # Train data for prediction tab
    predictData <- reactive({
      predictData <- apartmentData %>% select(sale_price, sqft_size, floor, N_FacilitiesInApt, accessToSubwaySTN)
    })
    
    # Split data into train 
    trainsplit1 <- reactive({
      set.seed(123) 
      train = sample(1:nrow(predictData()), size=nrow(predictData())*input$proportion)
    })
    
    # Reactive train data
    traindata1 <- reactive({
      train_data = predictData()[trainsplit1(), ]
    })

    # Make prediction on the four variables
    observeEvent(input$prediction,
                output$PredictClick <- renderText({
                  withProgress(message = "In progress", value = NULL,{
                  rfmodelFit <- train(sale_price ~ sqft_size + floor + N_FacilitiesInApt, data = traindata1(),
                                      method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "cv", number = 5),tuneGrid = data.frame(mtry = (1:4)) )  
                  
                  predict(rfmodelFit, newdata = data.frame(
                      sqft_size = isolate(input$sqft_sizeinput),
                      floor = isolate(input$floorinput),
                      N_FacilitiesInApt = isolate(input$N_FacilitiesInAptinput), 
                      accessToSubwaySTN = isolate(input$subwaySTNinput)
                  ))
                  }) 
                })
    )
    
    # Create reactive data for data tab
    aptData <- reactive({
        data <- apartmentData %>% filter(between(sqft_size, input$sqft_size2[1], input$sqft_size2[2]) & 
                                           between(YearBuilt, input$YearBuilt2[1], input$YearBuilt2[2])) %>%
          select(YearSold, MonthSold, accessToSubway, accessToSubwaySTN, accessToSubway, input$numericalVarNames)
    })

    # Data tab
    output$Data <- renderDataTable({aptData()}, options = list(scrollX = '200px'))
    # Download
    output$downloadData <- downloadHandler(
                                  filename = "apartmentData.csv",
                                  content = function(file){
                                                write.csv(aptData(), file)
                                  }
                            )
}) 



    


