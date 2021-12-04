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


# Define UI 
shinyUI(dashboardPage(
    # Title
    dashboardHeader(title = "Apartment prices dashboard"),
    
    # Sidebar menu set up
    dashboardSidebar(width = 130,
        sidebarMenu(
            menuItem("About", tabName = "About"),
            menuItem("Data Exploration", tabName = "DataExploration"),
            menuItem("Modeling", tabName = "Modeling"),
            menuItem("Data", tabName = "Data")
        )
    ), 

    # Dashboard Body set up
    dashboardBody(
        tabItems(
            tabItem(tabName = "About",
                    h2("Apartment Data"),
                    fluidRow(
                        box(
                            h3("Purpose of the app"),
                            "The purpose of the project is to navigate various factors influencing apartment price changes in Daegu, South Korea.
                                In Korea, apartments are one of the most popular housing types and are mostly located in city areas.
                                After exploring the data, I will set up a multiple linear regression model, regression tree model, and a random forest model and 
                                make predictions on apartment prices by factors that may influence the housing price (i.e. year built, size)."
                        ),
                        box(
                            h3("The data and its source"),
                            "This data provide the traded apartment information in one district of South Korea over 10 years and contains 5391 instances with 30 variables.
                            Particulary, the data provide sales history of apartments, accessibility of public transporation, and apartment facts and features. ", 
                            br(),
                            "In this project, I selected a subset of variables from the data and manipulated them for the EDA and modeling.",
                            br(),
                            "The apartment data was obtained from Kaggle.",
                            br(),
                            a("Click here to go to the Kaggle website to view the apartment data", href="https://www.kaggle.com/gunhee/koreahousedata")
                        ),
                        box(
                            h3("The purpose of each tab(page)"),
                            br(),
                            h4("About"),
                            "The About page provide brief introduction of the app, data, and each tab (pages).",
                            br(),
                            h4("Data Exploration"),
                            "This page, I will be able to make summary charts and plots from the data.",
                            br(),
                            h4("Modeling"),
                            "The Modelling page will let me explore three different predictive models.",
                             tags$ul(
                                 tags$li("The Modeling Info tab will show a description of the three modeling approaches"),
                                 tags$li("The Model Fitting tab will enable me to train the data and choose model settings. Also, models will be compared on the test set."),
                                 tags$li("The Prediction tab will allow me to predict the price of apartment for selected predictors.")
                                 ),
                            h4("Data"),
                            "In the Data page, I will be able to view and download the dataset."
                            ),
                        box(
                            img(src="Daegu_apartment.jpg", height = 350, width=500, align="center")
                        )
                    )
            ),
            
            tabItem(tabName = "DataExploration",
                   h2("Data Exploration Page"),
                   fluidPage(
                      titlePanel("Housing Market: Apartment prices & trends"),
                        sidebarLayout(
                            sidebarPanel(
                                selectInput(
                                   inputId = 'graphType',
                                   label = 'Select graph',
                                   choices = list('Histogram','Box plot', 'Bar plot', 'Scatter plot', 'Correlation plot')
                                ),
                            
                            conditionalPanel(condition = "input.graphType == 'Histogram'",
                                selectInput(
                                    inputId = 'price',
                                    label = 'Histogram of apartment prices',
                                    choices = 'sale_price'
                                ),
                                sliderInput("numberofbins","Select number of bins for this histogram",
                                            min=0, max=30, step = 1, value = 0)
                              ),
                            conditionalPanel(condition = "input.graphType == 'Box plot'",
                                selectInput(
                                    inputId = 'sales', 
                                    label = 'APT was sold in',
                                    choices = list('YearSold', 'MonthSold'))
                                ),
                            conditionalPanel(condition = "input.graphType == 'Bar plot'",
                                selectInput(
                                    inputId = 'transportation', 
                                    label = 'closeness to the subway station',
                                    choices = 'accessToSubwaySTN')
                                ),
                            conditionalPanel(condition = "input.graphType == 'Scatter plot'",
                                selectInput(
                                    inputId = 'featuresX', 
                                    label = 'Choose a variable for your X-axis',
                                    choices = numericalVarNames,
                                    selected = 'sqft_size'
                                    ),  
                                selectInput(
                                    inputId = 'featuresY',
                                    label = 'Choose a variable for your Y-axis',
                                    choices = numericalVarNames,
                                    selected = 'sale_price'
                                    ),
                                checkboxInput('geomline', 'Add a scatter line to the plot.')
                            ),
                            sliderInput(
                                inputId = 'YearBuilt1',
                                label = 'Filter the data by YearBuilt',
                                min = min(apartmentData$YearBuilt),
                                max = max(apartmentData$YearBuilt),
                                value = c(min(apartmentData$YearBuilt),
                                          max(apartmentData$YearBuilt))
                            ),
                            sliderInput(
                                inputId = 'sqft_size1',
                                label = 'Filter the data by sqft_size',
                                min = min(apartmentData$sqft_size),
                                max = max(apartmentData$sqft_size),
                                value = c(min(apartmentData$sqft_size),
                                          max(apartmentData$sqft_size))
                            )),
                         
                            mainPanel(
                                conditionalPanel(condition = "input.graphType == 'Histogram'",
                                                 plotlyOutput("histogram")
                                ),
                                conditionalPanel(condition = "input.graphType == 'Box plot'",
                                              uiOutput("infoSale"),
                                              plotlyOutput("boxPlot"),
                                              textOutput("info"),
                                              dataTableOutput("saleSummaryTable")
                                ),
                                conditionalPanel(condition = "input.graphType == 'Bar plot'",
                                              plotlyOutput("barPlot"),
                                              dataTableOutput('summarytable')
                                ),
                                conditionalPanel(condition = "input.graphType == 'Scatter plot'",
                                              uiOutput("infoNew"),
                                              plotlyOutput("scatterPlot"),
                                              dataTableOutput("numSummaryTable")
                                ),
                                conditionalPanel(condition = "input.graphType == 'Correlation plot'",
                                              plotOutput("corrplot"),
                                              textOutput("info1")
                                )
                            )
                         ))
                   ),
           
            tabItem(tabName = 'Modeling',
                    fluidPage(
                        tabsetPanel(
                            tabPanel("Modeling Info",
                                     fluidRow(
                                         box(
                                             strong("Multiple Linear Regression: "),
                                             "Linear regression is one of the commonly used methods for modeling and a useful tool for predicting a quantitative response on the basis of a single or multiple predictor variables.", 
                                             "The idea of linear regression is that the model finds the best fit line between predictor variables and response variable, minimizing sum of squared errors.",  
                                             "Particularly, multiple linear regression can include many explanatory variables, higher order terms, and/or interactions, so we can see the effect of the combinations on prediction. ",
                                             br(),
                                             "Here is an example of the multiple linear regression model:",
                                             withMathJax(
                                                 helpText('\\(Y_i= \\beta_0\\ + \\beta_1X_{1i}\\ + \\beta_2X_{2i}\\ + \\beta_3X_{1i}X_{2i}+E_i\\) ')
                                             ),
                                             "One benefit of a multiple linear regression model is that not only numerical but categorical predictors can be included.", 
                                             "In so doing, we may see fits that differ by the level of a variable.", 
                                             "Yet, this model is limited in terms of violation of assumptions and multicollinearity among predictors."
                                         ),
                                         box(
                                             strong("Regression Trees: "),
                                             "Tree based methods take the predictors’ spaces and split up them into regions and then develop different predictors for each region.", 
                                             "Regression trees use means of observations as prediction for a given region and try to minimize the residual sum of squares for every possible value of each predictor.", 
                                             br(),
                                             "Regression tree is easy to understand and interpret output, and no statistical assumptions are necessary.", 
                                             "However, trees are changed vastly even there is a small changes in the data. Therefore, pruning process is necessary."
                                         )
                                     ),
                                     fluidRow(
                                         box(
                                             strong("Random Forest: "),
                                             "Random Forest method, which shares the idea of bagging,", 
                                             "but extends the idea and only includes a random subset of predictors for each bootstrap sample/tree fit instead of including every predictor in each of the trees.", 
                                             "In doing so, one or two good predictors won’t dominate the tree fit. Also, by choosing a randomly selected subset of predictors in each tree,", 
                                             "we will possibly reduce the correlation and gain stronger prediction. Moreover, random forest decreases variance over an individual tree fit and can average across many fitted trees.", 
                                             "However, the disadvantages include losing interpretability and a long computation time."
                                         )
                                     )
                            ),
                            
                            tabPanel("Modeling Fitting",
                                     sidebarLayout(
                                         sidebarPanel(
                                             sliderInput("proportion", "Proportion of data you want to use for training",
                                                         min = 0.1, max = 0.9, value = 0.7, step = 0.1),
                                             checkboxGroupInput('predictors', h4('Select variables'),
                                                                choices = list('sqft_size', 
                                                                               'floor',
                                                                               'N_FacilitiesInApt',
                                                                               'accessToSubwaySTN'),
                                                                selected = 'sqft_size'),
                                             selectInput(
                                                 inputId = 'numcv', 
                                                 label = 'Choose cross-validation number',
                                                 choices = c('5','10'),
                                                 selected = '5'),  
                                             checkboxInput('interaction', strong('Option to add interactions for Multiple linear regression model')),
                                             actionButton("reportTrain","Fit models on training data"),
                                             conditionalPanel(condition = "input.reportTrain ==1",
                                                              actionButton("reportTest","Fit models on test data")
                                             )
                                         ),

                                         mainPanel(
                                             h4(strong("Multiple Linear Regression model")),
                                             tableOutput("mlrmodelfit"),
                                             h5("ANOVA table from Multiple Linear Regression model"),
                                             tableOutput("summaryMLRTable"),
                                             br(),
                                             h4(strong("Regression Tree model")),
                                             verbatimTextOutput("rtmodelfit"),
                                             h5("Regression tree"),
                                             plotOutput("rtplot"),
                                             br(),
                                             h4(strong("Random Forest model")),
                                             tableOutput("rfmodelfit"),
                                             h5("Variable Importance plot"),
                                             plotOutput("rfplot"),
                                             br(),
                                             h4(strong("Multiple Linear Regression model on the test set")),
                                             tableOutput("mlrmodelTest"),
                                             br(),
                                             h4(strong("Regression tree model on the test set")),
                                             tableOutput("rtmodelTest"),
                                             br(),
                                             h4(strong("Random Forest model on the test set")),
                                             tableOutput("rfmodelTest")
                                         )
                                    )),
                            
                            tabPanel("Prediction",
                                     sidebarLayout(
                                         sidebarPanel(
                                             sliderInput("sqft_sizeinput", "Select a value for sqft_size", 
                                                         min = 100, max=2500, step = 10, value = 1000),
                                             sliderInput("floorinput", "Select a value for floor", 
                                                         min = 1.0, max = 43, step =1, value = 30),
                                             sliderInput("N_FacilitiesInAptinput", "Select a value for N_FacilitiesInApt",
                                                         min=1, max=10, step = 1, value = 3),
                                             selectInput("subwaySTNinput", "Select a value for access to subway station",
                                                        choices =c("Very near","Near", "Moderate", "Far", "Not available")),
                                             actionButton("prediction","Predict")  
                                         ),
                                         
                                         mainPanel(
                                             h3('Make a prediction using the Random forest model.'),
                                             br(),
                                             h3("The predicted apartment sale price is "), box(textOutput("PredictClick"))
                                         )
                                     )
                            )
                        
                   ))
            ),
                      
            tabItem(tabName = 'Data',
                    fluidPage(
                         titlePanel("Data"),
                         h4('Apartment dataset'),
                         sidebarLayout(
                           sidebarPanel(
                             checkboxGroupInput(
                                 inputId = 'numericalVarNames', 
                                 label = 'Option to choose columns to view', 
                                 choices = numericalVarNames, 
                                 selected = numericalVarNames
                             ),
                             sliderInput(
                                 inputId = 'YearBuilt2',
                                 label = 'Option to subset rows by YearBuilt',
                                 min = min(apartmentData$YearBuilt),
                                 max = max(apartmentData$YearBuilt),
                                 value = c(min(apartmentData$YearBuilt),
                                           max(apartmentData$YearBuilt))
                             ),
                             sliderInput(
                                 inputId = 'sqft_size2',
                                 label = 'Option to subset rows by sqft_size',
                                 min = min(apartmentData$sqft_size),
                                 max = max(apartmentData$sqft_size),
                                 value = c(min(apartmentData$sqft_size),
                                           max(apartmentData$sqft_size))
                             ),
                             downloadButton('downloadData', 'Download data')
                             ),
                    
                         mainPanel(
                             dataTableOutput(outputId = 'Data')
                    )
                )
            )) 
    )) 
)) 


    
