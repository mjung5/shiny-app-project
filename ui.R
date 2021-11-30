
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
    MonthSold = month.abb[MonthSold],
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

# Overwrite accessToSubwaySTN column with factor version
apartmentData$accessToSubwaySTN <- as.factor(apartmentData$accessToSubwaySTN)

apartmentData$MonthSold <- as.factor(apartmentData$MonthSold)
# Use ordered function on a factor to order the levels
apartmentData$accessToSubwaySTN <- ordered(apartmentData$accessToSubwaySTN, 
                                           levels = c("Very near", "Near", "Moderate", "Far", "Not available"))

apartmentData$MonthSold <- ordered(apartmentData$MonthSold, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

numericalVarNames = names(apartmentData %>% dplyr::select(YearBuilt, sqft_size, floor, N_Parkinglot, N_APT, N_manager, 
                                                          N_elevators, N_FacilitiesInApt, N_FacilitiesNearBy, N_SchoolNearBy, sale_price))

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    # title
    dashboardHeader(title = "Apartment prices dashboard"),
    
    # sidebar menu set up
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
                                In Korea, apartment is one of the most popular housing type and is mostly located in city areas.
                                After exploring the data, I will set up a multiple linear regression model, regression tree model, and a random forest model and 
                                make predictions on apartment prices by factors may influence the housing price (i.e. year built, size)."
                        ),
                        box(
                            h3("The data and its source"),
                            "This data provide the traded apartment information in one district of South Korea over 10 years and contains 5391 instances with 30 variables.
                            Particulary, the data provide sales history of house, accessibility of public transporation, and apartment facts and features. ", 
                            br(),
                            "The apartment data was obtained from Kaggle.",
                            br(),
                            a("Click here to go to the Kaggle website to view he apartment data", href="https://www.kaggle.com/gunhee/koreahousedata")
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
                            #br(),
                            h4("Data"),
                            "In the Data page, I will be able to view the dataset."
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
                                   choices = list('Apartment prices','Sales history', 'Access to subway station', 'Apartment features', 'Correlation plot')
                              ),
                              
                            conditionalPanel(condition = "input.graphType == 'Apartment prices'",
                                selectInput(
                                    inputId = 'price',
                                    label = 'Histogram of apartment prices',
                                    choices = 'sale_price'
                                ),
                                sliderInput("numberofbins","Select number of bins for this histogram",
                                            min=0, max=30, step = 1, value = 0)
                              ),

                            conditionalPanel(condition = "input.graphType == 'Sales history'",
                                selectInput(
                                    inputId = 'sales', 
                                    label = 'APT was sold in',
                                    choices = list('YearSold', 'MonthSold'))
                                ),
                            conditionalPanel(condition = "input.graphType == 'Access to subway station'",
                                selectInput(
                                    inputId = 'transportation', 
                                    label = 'closeness to the subway station',
                                    choices = 'accessToSubwaySTN')
                                ),
                            conditionalPanel(condition = "input.graphType == 'Apartment features'",
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
                                             
                                    )

                            ),
                         
                            mainPanel(
                                conditionalPanel(condition = "input.graphType == 'Apartment prices'",
                                                 plotlyOutput("histogram")
                                ),
                                conditionalPanel(condition = "input.graphType == 'Sales history'",
                                              uiOutput("infoSale"),
                                              plotlyOutput("boxPlot"),
                                              textOutput("info"),
                                              dataTableOutput("saleSummaryTable")

                                ),
                                conditionalPanel(condition = "input.graphType == 'Access to subway station'",
                                              plotlyOutput("barPlot"),
                                              dataTableOutput('summarytable')

                                ),
                                conditionalPanel(condition = "input.graphType == 'Apartment features'",
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
                                             "Linear regression is one of the commonly used methods for modeling and useful tool for predicting a quantitative response on the basis of a single or multiple predictor variables.", 
                                             "The idea of linear regression is that the model finds the best fit line between predictor variables and response variable, minimizing sum of squared errors.",  
                                             "Particularly, multiple linear regression can include many explanatory variables, higher order terms, and/or interaction, so we can see the effect the combinations on prediction. ",
                                             br(),
                                             "Here is the example of the multiple linear regression model:",
                                             withMathJax(
                                                 helpText('\\(Y_i= \\beta_0\\ + \\beta_1X_{1i}\\ + \\beta_2X_{2i}\\ + \\beta_3X_{1i}X_{2i}+E_i\\) ')
                                             ),
                                             "One benefit of a multiple linear regression model is that not only numerical but categorical predictors can be included.", 
                                             "In so doing, we may see fits that differ by the level of a variable.", 
                                             "Yet, this model is limited in terms of violation of assumptions and multicollinearity among predictors."
                                         ),
                                         box(
                                             strong("Regression Trees: "),
                                             "Tree based methods take the predictors’ spaces and split up them into regions and then develop different predictors for each regions.", 
                                             "Regression trees use mean of observations as prediction for a given region and try to minimize the residual sum of squares for every possible value of each predictor.", 
                                             br(),
                                             "Regression tree is easy to understand and interpret output and no statistical assumptions are necessary.", 
                                             "However, trees are changed vastly even there is a small changes in the data. Therefore, pruning process is necessary."
                                         )
                                     ),
                                     fluidRow(
                                         box(
                                             strong("Random Forest: "),
                                             "Random Forest method, which shares the idea of bagging,", 
                                             "but extends the idea and only include random subset of predictors for each bootstrap sample/tree fit instead of including every predictor in each of the tree.", 
                                             "In doing so, one or two good predictors won’t dominate the tree fit.Also, by choosing randomly selected subset of predictors in each tree,", 
                                             "we will possibly reduce the correlation and gain stronger prediction. Moreover, random forest decreases variance over an individual tree fit, and can average across many fitted trees.", 
                                             "However, the disadvantages are losing interpretability and the computation time is long."
                                         )
                                     )
                            ),
                            tabPanel("Modeling Fitting",
                                     sidebarLayout(
                                         sidebarPanel(
                                             sliderInput("proportion", "Proportion of data you want to use for training",
                                                         min = 0.1, max = 0.9, value = 0.5, step = 0.1),
                                           
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
                                             #sliderInput('mtry', strong('Choose the number of mtry for Random forest model'),
                                              #           min = 1, max = 3, value = 2, step = 1),
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
                                             plotOutput("rtplot"),
                                             br(),
                                             h4(strong("Random Forest model")),
                                             tableOutput("rfmodelfit"),
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
                                             h4('Before click the', strong('predict'), 'button, make sure that you fit the Random Forest model in the modeling fitting tab with all 4 predictor variables.'),
                                             br(),
                                             h3("The predicted apartment sale price is "), textOutput("PredictClick")
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
                               
                             downloadButton('downloadData', 'Download data')
                             ),
                    
                           mainPanel(
                        dataTableOutput(outputId = 'Data')
                    )
                )
            ))
))

))


    
