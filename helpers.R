library(tidyverse)

daegu_real_estate_data <- read.csv("Daegu_Real_Estate_data.csv")

# Column list that are not being used. 
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



