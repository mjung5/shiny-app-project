# shiny-app-project
Min-Jung Jung

## Overview
This project is to create a Shiny app to explore the Apartment data in Daegu, South Korea.  
Through the shiny app, you can view and manipulate several summary tables and plots and fit three different models via several tabs. Also, you will view and download the data . 

## Requirements
This project requires the following packages:
`shiny`
`DT`
`caret`
`tidyverse`
`plotly`
`corrplot`
`imager`
`shinydashboard`
`summarytools`
`rpart.plot`
`randomForest`.

To install the packages, run the following code chunk:

```
install.packages("shiny")
install.packages("DT")
install.packages("caret")
install.packages("tidyverse")
install.packages("plotly")
install.packages("corrplot")
install.packages("imager")
install.packages("shinydashboard")
install.packages("summarytools")
install.packages("rpart.plot")
install.packages("randomForest")
```

## Code to run shiny app

```
shiny::runGitHub("mjung5/shiny-app-project", ref="main")
```