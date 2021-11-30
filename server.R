
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

daegu_real_estate_data <- read.csv("Daegu_Real_Estate_data.csv")

# Column list that are not be used.
columns_not_used <- c('HallwayType', 'HeatingType', 'AptManageType', 'TimeToBusStop',
                      'SubwayStation', 'N_FacilitiesNearBy.PublicOffice.','N_FacilitiesNearBy.Hospital.',
                      'N_FacilitiesNearBy.Dpartmentstore.', 'N_FacilitiesNearBy.Mall.', 'N_FacilitiesNearBy.ETC.',
                      'N_FacilitiesNearBy.Park.', 'N_SchoolNearBy.Elementary.', 'N_SchoolNearBy.Middle.',
                      'N_SchoolNearBy.High.', 'N_SchoolNearBy.University.')

# Data manipulation
daegu_real_estate_data_update <- daegu_real_estate_data %>% select(-columns_not_used) %>% dplyr::transmute(
    YearSold = as.factor(YrSold),
    MonthSold =  month.abb[MonthSold],
    accessToSubway = as.factor(TimeToSubway),
    YearBuilt = YearBuilt,
    sqft_size = Size.sqf.,
    floor = Floor,
    N_Parkinglot.Ground = N_Parkinglot.Ground.,
    N_Parkinglot.Basement = N_Parkinglot.Basement.,
    N_APT = N_APT,
    N_manager = N_manager,
    N_elevators = N_elevators,
    N_FacilitiesInApt = N_FacilitiesInApt,
    N_FacilitiesNearBy = N_FacilitiesNearBy.Total.,
    N_SchoolNearBy = N_SchoolNearBy.Total.,
    sale_price = SalePrice
)

# Function to create the accessToSubwaySTN column. 
# AccessToSubwaySTN rating was created.
accessToSubSt <- function(dataset){
    dataset <- dataset %>% 
        mutate("N_Parkinglot"= N_Parkinglot.Ground + N_Parkinglot.Basement, 
               "accessToSubwaySTN" = if_else(accessToSubway == '0-5min', "Very near",
                                             if_else(accessToSubway == '5min~10min', "Near", 
                                                     if_else(accessToSubway == '10min~15min', "Moderate",
                                                             if_else(accessToSubway == '15min~20min', "Far", "Not available"))))
        )            
    return(dataset)
}

# Data set using accessToSubSt function.
apartmentData <- accessToSubSt(daegu_real_estate_data_update) %>% as_tibble()

# Overwrite columns with factor version
apartmentData$accessToSubwaySTN <- as.factor(apartmentData$accessToSubwaySTN)
apartmentData$MonthSold <- as.factor(apartmentData$MonthSold)

# Use ordered function on a factor to order the levels
apartmentData$accessToSubwaySTN <- ordered(apartmentData$accessToSubwaySTN, 
                                           levels = c("Very near", "Near", "Moderate", "Far", "Not available"))
apartmentData$MonthSold <- ordered(apartmentData$MonthSold, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Subset variables
numericalVarNames = names(apartmentData %>% dplyr::select(YearBuilt, sqft_size, floor, N_Parkinglot, N_APT, N_manager, 
                                                          N_elevators, N_FacilitiesInApt, N_FacilitiesNearBy, N_SchoolNearBy, sale_price))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    #creates a reactive context for the data set
    getApartmentData <- reactive({
        ApartmentData1 <- apartmentData
    })

    
    # creates a reactive context for the data set including only selected variables for the modeling
    newmodelApartmentData <- reactive({
        getmodelapartmentData1 <- apartmentData %>%select(sqft_size,floor,N_FacilitiesInApt,sale_price)
    })
    
    # create plot
    
    # Summaries(Graphical and Numerical)   
    # Histogram - apartment price
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
      text1 <- paste0("You have selected to veiw bar plots showing how apartment prices change by ", input$sales)
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
    
    # Scatter plot - Apartment features and facts
    output$scatterPlot = renderPlotly({
        # Geom_smooth option
        if(!input$geomline){
            scatterPlot = ggplot(getApartmentData(),aes_string(x=input$featuresX, y=input$featuresY))+
                geom_point(color="blue")
            ggplotly(scatterPlot)
        } else {
            scatterPlot = ggplot(getApartmentData(),aes_string(x=input$featuresX, y=input$featuresY))+
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
      m <- descr(summaryData(), stats = c("mean", "sd", "min", "q1", "med", "q3", "max", "iqr"), round.digits = st_options("round.digits"), order = "preserve",
                 transpose = FALSE)
      class(m) <- "matrix"
      m %>% as_tibble(rownames="Statistic")
      DT::datatable(m, caption = "Summary statistics of apartment features",options = list(scrollX = 't')) %>% formatRound(columns = c(1:11), digits = 2) 
    })
        
    # Correlation plot-  the correlation matrix for the numerical variables   
    output$corrplot = renderPlot({
      df_tmp <- apartmentData %>% dplyr::select(all_of(numericalVarNames))
      
      corrplot(cor(df_tmp), type = 'lower', diag = FALSE)
    })
    
    # Create text info
    output$info1 <- renderText({
      
      paste("We use this correlation plot to find the variables that are highly correlated and avoid including the variables in our model building phase.")
    })
    
    
    # This data will be used for running the models. Three numeric variables were selected based on the correlation plots.
    modelData <- reactive({
      newmodelData <- getApartmentData() %>% select(sale_price, input$predictors)
    })
    
    # Split data into train and test
    trainsplit <- reactive({
      set.seed(123) # For reproducibility
      train = sample(1:nrow(modelData()), size=nrow(modelData())*input$proportion)
    })
    traindata <- reactive({
      train_data = modelData()[trainsplit(), ]
    })
    
    test <- reactive({
      test = dplyr::setdiff(1:nrow(modelData()), trainsplit())
    })
    testdata <- reactive({
      test_data = modelData()[test(), ]
    })

    # Display predict results on train set
    # Multiple regression model
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
    
    observeEvent(input$reportTrain,
                 output$mlrmodelfit <- renderTable({
                   print(mlrFit()$results[2:7])
                 })
                 )

    observeEvent(input$reportTrain,
                 output$summaryMLRTable <- renderTable(
                  
                 as.data.frame(anova(lm(if(!input$interaction){
                   sale_price ~ .
                 }else{
                   sale_price ~ .*.
                 },
                 data = traindata()))
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
                   print(rtmFit())
                 })
    )
 
    rtp <- reactive({
           rtp1 <- rpart(sale_price ~., data  = traindata())
    })
    observeEvent(input$reportTrain,
                 output$rtplot <- renderPlot(

                   rpart.plot(rtp())
                 ) 
                   )
    
    # Random forest model
    rfmFit <- reactive({
      set.seed(123)
      rfmFit1 <-   train(sale_price ~.,
                          data = traindata(),
                          method = 'rf',
                          preProcess = c("center", "scale"),
                          trControl = trainControl("cv", number = as.numeric(input$numcv))
                          #tuneGrid = data.frame(mtry = seq(1, as.numeric(input$mtry), by = 1))
                         ) 
    })
    
    # Random forest fit on train set
    observeEvent(input$reportTrain,
                 output$rfmodelfit <- renderTable({
                   print(rfmFit()$results)
                 })
    )
    
    # Variable importance plot for random forest model    
    rfvar <- reactive({
        rfvar1 <- randomForest(sale_price ~., data = traindata(), ntree=1000, importance=TRUE)
    
    })

    observeEvent(input$reportTrain,
                 output$rfplot <- renderPlot({
 
                 varImpPlot(rfvar())
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
    
    # Make a prediction based on the values of the predictors
    rfmodelTrain <- reactive({
      rfmodelFit <- train(sale_price ~ sqft_size + floor + N_FacilitiesInApt + accessToSubwaySTN, data = traindata(),
                          method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "cv", number = as.numeric(input$numcv)))
      
    })
    
    # Make prediction on the four variables
    observeEvent(input$prediction,
                output$PredictClick <- renderText({
                  predict(rfmodelTrain(), newdata = data.frame(
                      sqft_size = isolate(input$sqft_sizeinput),
                      floor = isolate(input$floorinput),
                      N_FacilitiesInApt = isolate(input$N_FacilitiesInAptinput),
                      accessToSubwaySTN = isolate(input$subwaySTNinput)
                  ))
                })
    )
    
   
    
    # Create reactive data for data tab
    aptData <- reactive({
        data <- select(apartmentData, YearSold, MonthSold, accessToSubway, accessToSubwaySTN, accessToSubway, input$numericalVarNames)
    })

    # Data tab
    output$Data <- renderDataTable(datatable(aptData(), options = list(scrollX = T)))

    output$downloadData <- downloadHandler(
            filename = "apartmentData.csv",
            content = function(file){
                write.csv(aptData(), file)
            })
    
})



    


