# Nitrate Since 2010 
# Peggy Chen 
# July 10, 2024 

# Set wd 
setwd("/Users/xuejingyan/Desktop/R/Practice/NC_Water_Quality")

##### Installing the USGS HASP package ##### 
# First install the remotes package 
# install.packages("remotes") 

# Install HASP package 
# remotes::install_gitlab("water/stats/hasp", 
# host = "code.usgs.gov", 
# build_opts = c("--no-resave-data","--no-manual"), 
# build_vignettes = TRUE, 
# dependencies = TRUE) 

##### Load packages ##### 
library(tidyverse) 
library(remotes)
library(HASP)
library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(sf)
library(spData)
library(maps)

##### Preparation ##### 
# Assign codes and pull data 
pCode <- c("00618", "00620", "51185", "51290", "64832", "71850", "71851", "91003",
           "91006", "99121", "99124", "99130", "99136", "99137", "99411")

nNC <- whatNWISdata(stateCd="NC", 
                     parameterCd=pCode) 

# Tidy data 
nNC <- nNC %>%
  separate(begin_date, into = c("Year_start"), sep = "-", remove = FALSE) %>% 
  separate(end_date, into = c("Year_end"), sep = "-", remove = FALSE) 

# Filter data acc. to start date of measurements 
nNC_1 <- nNC %>% 
  mutate(period = as.Date(end_date) - as.Date(begin_date)) %>%
  filter(Year_start > 2010)

##### Mapping sites ##### 
# Preparing data to map 
sf_nNC_1 <- st_as_sf(nNC_1, 
                      coords = c("dec_long_va", "dec_lat_va"),
                      crs = 4269)
usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE),
                crs = 4269)
map_nNC <- ggplot() +
  geom_sf(data = usa[ usa$ID == "north carolina" ,]) +
  geom_sf(data = sf_nNC_1) + 
  xlab(NULL)+
  ylab(NULL)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  labs(title = "Sites that Measure Nitrate in Water in NC since 2010", 
       caption = "United States Geological Survey, 2024.") 
map_nNC

# Let’s look at the maximum measured value, and number of samples 
n_NC_data <- readNWISqw(siteNumbers = nNC_1$site_no,
                         parameterCd = pCode)

nNC_1_summary <- n_NC_data %>% 
  group_by(site_no) %>% 
  summarize(max = max(result_va, na.rm = TRUE),
            count = n()) %>% 
  ungroup() %>% 
  left_join(attr(n_NC_data, "siteInfo"), 
            by = "site_no")

##### Temporal graphing ##### 
# Get nitrate measurements 
n_sites <- nNC_1$site_no

n_all_data <- readWQPqw(paste0("USGS-", n_sites),
                         c("Nitrate"))

# Tidy data 
n_all_data <- n_all_data %>%
  separate(ActivityStartDate, into = c("Year_start"), sep = "-", remove = FALSE) %>% 
  filter(Year_start > 2010) 

##### Initial time graph ##### 
plot_n <- ggplot(data = n_all_data, aes(x = ActivityStartDate, y = ResultMeasureValue)) + 
  geom_point() + 
  labs(title = "Nitrate levels in North Carolina Waters since 2010", 
       x = "Year", 
       y = "Nitrate in mg/L", 
       caption = "1306 measurements since 2010.") +
  geom_smooth(method = "lm",  fill="#69b3a2", se = TRUE) 
plot_n 

# Remove outliers 
n_clean_data <- n_all_data %>%
  filter(ResultMeasureValue < 60) 

# Plot again 
plot_n_1 <- ggplot(data = n_clean_data, aes(x = ActivityStartDate, y = ResultMeasureValue)) + 
  geom_point() + 
  labs(title = "Nitrate levels in North Carolina Waters since 2010", 
       x = "Year", 
       y = "Nitrate in mg/L", 
       caption = "1306 measurements since 2010.") +
  geom_smooth(method = "lm",  fill="#69b3a2", se = TRUE) 
plot_n_1 

##### Separating by ground or surface water ##### 
nitrate_groundwater <- subset(n_all_data, ActivityMediaSubdivisionName 
                              %in% c("Groundwater"))

nitrate_surfacewater <- subset(n_all_data, ActivityMediaSubdivisionName 
                              %in% c("Surface Water")) 

# Plot groundwater data 
plot_groundwater <- ggplot(data = nitrate_groundwater, aes(x = ActivityStartDate, y = ResultMeasureValue)) + 
  geom_point() + 
  labs(title = "Nitrate levels in North Carolina Groundwater since 2010", 
       x = "Year", 
       y = "Nitrate in mg/L", 
       caption = "United States Geologiccal Survey, 2024.")
plot_groundwater 

# Plot surface water data 
plot_surfacewater <- ggplot(data = nitrate_surfacewater, aes(x = ActivityStartDate, y = ResultMeasureValue)) + 
  geom_point() + 
  labs(title = "Nitrate levels in North Carolina Surface Water since 2010", 
       x = "Year", 
       y = "Nitrate in mg/L", 
       caption = "United States Geologiccal Survey, 2024.")
plot_surfacewater 

##### Separating Based On Chemical ##### 
# Tidy data from groundwater 
nitrate_NO3_gw <- subset(nitrate_groundwater, ResultMeasure.MeasureUnitCode 
                              %in% c("mg/l asNO3"))

nitrate_N_gw <- subset(nitrate_groundwater, ResultMeasure.MeasureUnitCode 
                               %in% c("mg/l as N")) 

# Tidy data from surface water 
nitrate_NO3_sw <- subset(nitrate_surfacewater, ResultMeasure.MeasureUnitCode 
                         %in% c("mg/l asNO3"))

nitrate_N_sw <- subset(nitrate_surfacewater, ResultMeasure.MeasureUnitCode 
                        %in% c("mg/l as N")) 

# Chart it all 
plot_NO3_gw <- ggplot(data = nitrate_NO3_gw, aes(x = ActivityStartDate, y = ResultMeasureValue)) + 
  geom_point() + 
  labs(title = "NO3 levels in North Carolina Ground Water since 2010", 
       x = "Year", 
       y = "NO3 in mg/L", 
       caption = "United States Geologiccal Survey, 2024.")
plot_NO3_gw 

plot_NO3_sw <- ggplot(data = nitrate_NO3_sw, aes(x = ActivityStartDate, y = ResultMeasureValue)) + 
  geom_point() + 
  labs(title = "NO3 levels in North Carolina Surface Water since 2010", 
       x = "Year", 
       y = "NO3 in mg/L", 
       caption = "United States Geologiccal Survey, 2024.")
plot_NO3_sw 

plot_N_gw <- ggplot(data = nitrate_N_gw, aes(x = ActivityStartDate, y = ResultMeasureValue)) + 
  geom_point() + 
  labs(title = "N levels in North Carolina Ground Water since 2010", 
       x = "Year", 
       y = "N in mg/L", 
       caption = "United States Geologiccal Survey, 2024.")
plot_N_gw 

plot_N_sw <- ggplot(data = nitrate_N_sw, aes(x = ActivityStartDate, y = ResultMeasureValue)) + 
  geom_point() + 
  labs(title = "N levels in North Carolina Surface Water since 2010", 
       x = "Year", 
       y = "N in mg/L", 
       caption = "United States Geologiccal Survey, 2024.")
plot_N_sw 

### Salinity Data 

salt <- read.csv("salinity.csv")

# Rename 
salt <- salt %>% 
  rename("Date" = Date_)

# Tidy data 
salt$date_formatted <- sprintf("%04d-%02d-%02d", year(mdy(salt$Date)), month(mdy(salt$Date)), day(mdy(salt$Date)))
  
plot <- ggplot(data = salt, aes(x = date_formatted, y = Salinity)) + 
  geom_point() 
plot 

library(wql)
sfb <- wqData(salt, c(1, 3:4), 5:12, site.order = TRUE,
              type = "wide", time.format = "%m/%d/%Y")
