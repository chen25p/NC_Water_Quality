# Functions for WQ Package 
# Peggy Chen 
# July 4, 2024 

##### Data Functions ##### 
# WQ data - NOT WORKING ! 
# wq_data <- function(x, y){
# library(tidyverse) 
# library(remotes)
# library(HASP)
# library(dataRetrieval)
# library(dplyr)
# site_data <- readWQPqw(paste0("USGS-", x), c(y))}
# Test 
# wq_data("02105524", "Specific conductance")

# Site sample data - NOT WORKING ! 
# site_data <- function(x, y){ 
# library(tidyverse) 
# library(remotes)
# library(HASP)
# library(dataRetrieval)
# library(dplyr)
# site_locations <- whatNWISsites(stateCd = x, parameterCd = y)}
# Test 
# site_data("NC", "00094") 

##### Graph Functions ##### 
# Plot with only points - ADD LABELS 
plotwq <- function(a, b, c, d, e){
  library(tidyverse) 
  library(remotes)
  library(HASP)
  library(dataRetrieval)
  library(dplyr)
  library(ggplot2)
  qw_data <- readWQPqw(paste0("USGS-", a),
                       c(b))
  plot <- ggplot(data = qw_data, aes(x = ActivityStartDate, y = ResultMeasureValue)) + 
    geom_point() + 
    labs(title = c, 
         x= d, 
         y = e, 
         caption = "USGS Water Quality Data, 2024.") + 
    theme_bw() 
  plot
}
# Test 
# plotwq("02091500", 
#       "Specific conductance", 
#       "Specific Conductance over Time at Pee Dee River in Rockingham, NC", 
#       "Year", 
#       "Specific conductance in µS/cm at 25 °C")

# Plot with regression and points 
regressionwq <- function(a, b, c, d, e){
  library(tidyverse) 
  library(remotes)
  library(HASP)
  library(dataRetrieval)
  library(dplyr)
  library(ggplot2)
  qw_data <- readWQPqw(paste0("USGS-", a),
                       c(b))
  regression_1 <- ggplot(qw_data, aes(x = ActivityStartDate, y = ResultMeasureValue)) +
    geom_point() + 
    geom_smooth(method = "lm",  fill="#69b3a2", se = FALSE) + 
    labs(title = c, 
         x= d, 
         y = e, 
         caption = "USGS Water Quality Data, 2024.") + 
    theme_bw() 
  regression_1 
} 
# Test 
# regressionwq("02129000", 
#             "Specific conductance", 
#             "Specific Conductance over Time at Pee Dee River in Rockingham, NC", 
#             "Year", 
#             "Specific conductance in µS/cm at 25 °C") 

# Plot with regression, points, confidence interval 
regressionwq <- function(a, b, c, d, e){
  library(tidyverse) 
  library(remotes)
  library(HASP)
  library(dataRetrieval)
  library(dplyr)
  library(ggplot2)
  qw_data <- readWQPqw(paste0("USGS-", a),
                       c(b))
  regression_1 <- ggplot(qw_data, aes(x = ActivityStartDate, y = ResultMeasureValue)) +
    geom_point() + 
    geom_smooth(method = "lm",  fill="#69b3a2", se = TRUE) + 
    labs(title = c, 
         x= d, 
         y = e, 
         caption = "USGS Water Quality Data, 2024.") + 
    theme_bw() 
  regression_1 
} 
# Test 
# regressionwq("02129000", "Specific conductance", 
#             "Specific Conductance over Time at Pee Dee River in Rockingham, NC", 
#             "Year", 
#             "Specific conductance in µS/cm at 25 °C") 

# OLS regression 
regression_test <- function(w, z){
  library(olsrr)
  library(remotes)
  library(HASP)
  library(dataRetrieval) 
wq_data <- readWQPqw(paste0("USGS-", w),
                       c(z)) 
y <- wq_data$ResultMeasureValue 
x <- wq_data$ActivityStartDate
ols <- lm(y ~ x, data = wq_data) 
ols
} 
# Test 
# regression_test("02129000", "Specific conductance")

# Mapping locations where a parameter is tested 
map_sites <- function(x, y, z, w){
  library(tidyverse) 
  library(remotes)
  library(HASP)
  library(dataRetrieval)
  library(dplyr)
  library(ggplot2)
  library(sf)
  # What sites measured nitrogen in North Carolina? 
  parameter_sites <- whatNWISsites(stateCd = z, 
                                   parameterCd = x)
  # Plot the locations of these sites 
  usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE),
                  crs = 4269)
  sf_parameter <- st_as_sf(parameter_sites, 
                           coords = c("dec_long_va", "dec_lat_va"),
                           crs = 4269)
  # Map 
  ggplot() +
    geom_sf(data = usa[ usa$ID == y ,]) +
    geom_sf(data = sf_parameter) + 
    xlab(NULL)+
    ylab(NULL)+
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) + 
    labs(title = w, 
         caption = "USGS Water Quality Data, 2024") 
} 
# Test 
# map_sites("00600", 
#          "north carolina", 
#          "NC", 
#          "USGS Locations that Meausure Total Nitrogen in Water across North Carolina") # y MUST be all lowercase 