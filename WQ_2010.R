# Specific Conductance Since 2010 
# Peggy Chen 
# July 10, 2024 

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

##### Filtered Data Example ##### 

# This example filters the points so that rather than displaying all locations where 
# specific conductance is tested, it only finds sites with data after 2010 

##### Specific Conductance Data Filter ##### 
# Assign codes and pull data 
pCode <- c("00094", "00095", "00402", "70386", "72430", "90094", "90095", 
           "90096", "99974", "99978", "99982")
scNC <- whatNWISdata(stateCd="NC", 
                     parameterCd=pCode)

# Tidy data 
scNC <- scNC %>%
  separate(begin_date, into = c("Year_start"), sep = "-", remove = FALSE) %>% 
  separate(end_date, into = c("Year_end"), sep = "-", remove = FALSE) 

# Filter data acc. to start date of measurements 
scNC_1 <- scNC %>% 
  mutate(period = as.Date(end_date) - as.Date(begin_date)) %>%
  filter(Year_start > 2010)

# Preparing data to map 
sf_scNC_1 <- st_as_sf(scNC_1, 
                        coords = c("dec_long_va", "dec_lat_va"),
                        crs = 4269)
usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE),
                crs = 4269) 

##### Map of site locations ##### 
plot_scNC <- ggplot() +
  geom_sf(data = usa[ usa$ID == "north carolina" ,]) +
  geom_sf(data = sf_scNC_1) + 
  xlab(NULL)+
  ylab(NULL)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  labs(title = "Sites that Measure Specific Conductance in Water in NC since 2010", 
       caption = "United States Geological Survey, 2024.") 
plot_scNC

# Let’s look at the maximum measured value, and number of samples 
sc_NC_data <- readNWISqw(siteNumbers = scNC_1$site_no,
                               parameterCd = pCode)

scNC_1_summary <- sc_NC_data %>% 
  group_by(site_no) %>% 
  summarize(max = max(result_va, na.rm = TRUE),
            count = n()) %>% 
  ungroup() %>% 
  left_join(attr(sc_NC_data, "siteInfo"), 
            by = "site_no")

##### Simple over time graph ##### 
# Get specific conductance measurements 
sc_sites <- scNC_1$site_no 
sc_all_data <- readWQPqw(paste0("USGS-", sc_sites),
                     c("Specific conductance"))

# Tidy data 
sc_all_data <- sc_all_data %>%
  separate(ActivityStartDate, into = c("Year_start"), sep = "-", remove = FALSE) %>% 
  filter(Year_start > 2010)

# Plot specific conductance data 
plot_sc <- ggplot(data = sc_all_data, aes(x = ActivityStartDate, y = ResultMeasureValue)) + 
  geom_point() + 
  labs(title = "Specific Conductance in North Carolina Waters since 2010", 
       x = "Years", 
       y = "Specific conductance in us/cm @ 25 degrees C", 
       caption = "United States Geological Survey, 2024.") 
plot_sc 

##### Adding counties ##### 
# Create county list 
startm <- Sys.time()
county<-map.where(database="county", 
                  scNC_1$dec_long_va, scNC_1$dec_lat_va)
endm <- Sys.time()

# Extract string into new column 
values <- unlist(strsplit(county, ' '))

# New column added to existing dataframe 
# new_column <- c(county)
# scNC_1$county <- new_column 

# Separate state from county name 
# scNC_coast <- data.frame(
#  location = scNC_1$county,
#  stringsAsFactors = FALSE  # Ensure strings are treated as characters, not factors)

# Split data 
# split_data <- strsplit(as.character(scNC_coast$location), ",") 

# Separate into state and county 
# state <- sapply(split_data, `[`, 1)
# counties <- sapply(split_data, `[`, 2)

# Compile into new data frame 
# separated_data <- data.frame(state = state, counties = counties)

# New columns added to existing dataframe 
# new_column_1 <- c(counties)
# scNC_1$county <- new_column_1

# Delete unwanted columns 
# scNC_1 <- subset(scNC_1, select = -c(counties, new_column_name, loc_web_ds, stat_cd))

# Export data 
# write.csv(scNC_1, "scNC_1.csv")

# Import fixed data 
new_counties <- read_csv("county.csv")

# Add new column 
scNC_1$counties <- new_counties$county

##### Filtering by coastal county ##### 
# Isolate all CAMA counties 
counties_of_interest <- c("beaufort", "bertie", "brunswick", 
                          "camden", "carteret", "chowan", "craven", "currituck", 
                          "dare", "gates", "hertford", "hyde", "new hanover", 
                          "onslow", "pamlico", "pasquotank", "pender", "perquimans", 
                          "tyrrell", "washington")

scNC_coastal_counties <- subset(scNC_1, counties %in% counties_of_interest)

# Call data from USGS 
sc_coastal_sites <- scNC_coastal_counties$site_no

sc_all_coastal_data <- readWQPqw(paste0("USGS-", sc_coastal_sites),
                                 c("Specific conductance")) 

# Plot specific conductance data 
plot_sc_coastal <- ggplot(data = sc_all_coastal_data, aes(x = ActivityStartDate, y = ResultMeasureValue)) + 
  geom_point() + 
  labs(title = "Specific Condutance Measurements in CAMA Counties since 2010", 
       x = "Year", 
       y = "Specific conductance in uS/cm at 25 degrees C", 
       caption = "USGS, 2024.") + 
  geom_smooth(method = "lm",  fill="#69b3a2", se = TRUE) 
plot_sc_coastal

##### Removing outliers ##### 
# Call data from USGS 
sc_coastal_sites <- scNC_coastal_counties$site_no

sc_clean_coastal_data <- readWQPqw(paste0("USGS-", sc_coastal_sites),
                                 c("Specific conductance")) 

# Clear data before 2010 
sc_clean_coastal_data <- sc_clean_coastal_data %>%
  separate(ActivityStartDate, into = c("Year_start"), sep = "-", remove = FALSE) %>% 
  filter(Year_start > 2010) %>% 
  filter(Year_start < 2020)

# Plot specific conductance data 
plot_sc_coastal_1 <- ggplot(data = sc_clean_coastal_data, aes(x = ActivityStartDate, y = ResultMeasureValue)) + 
  geom_point() + 
  labs(title = "Specific Condutance Measurements in CAMA Counties from 2010 - 2020", 
       x = "Year", 
       y = "Specific conductance in uS/cm at 25 degrees C", 
       caption = "USGS, 2024.") + 
  geom_smooth(method = "lm",  fill="#69b3a2", se = TRUE) 
plot_sc_coastal_1

##### Investigating Max Values ##### 
sc_max_data <- sc_all_coastal_data %>% 
  filter(ResultMeasureValue > 40000)
# Max days: 2017-05-18 and 2012-09-05 

##### Isolating Carteret County ##### 
counties_of_interest <- c("carteret")

scNC_carteret <- subset(scNC_1, counties %in% counties_of_interest)

# Call data from USGS 
sc_coastal_sites_carteret <- scNC_carteret$site_no

carteret_data <- readWQPqw(paste0("USGS-", sc_coastal_sites_carteret),
                                 c("Specific conductance")) 

# Plot specific conductance data 
carteret <- ggplot(data = carteret_data, aes(x = ActivityStartDate, y = ResultMeasureValue)) + 
  geom_point() + 
  labs(title = "Specific Condutance Measurements in Carteret Counties since 2010", 
       x = "Year", 
       y = "Specific conductance in uS/cm at 25 degrees C", 
       caption = "USGS, 2024.") + 
  geom_smooth(method = "lm",  fill="#69b3a2", se = TRUE) 
carteret 

##### Separating based on ground or surface water ##### 
sc_groundwater <- subset(sc_all_coastal_data, ActivityMediaSubdivisionName 
                              %in% c("Groundwater"))

sc_surfacewater <- subset(sc_all_coastal_data, ActivityMediaSubdivisionName 
                               %in% c("Surface Water")) 

# Plot respectively 
sc_gw <- ggplot(data = sc_groundwater, aes(x = ActivityStartDate, y = ResultMeasureValue)) + 
  geom_point() + 
  labs(title = "Specific Condutance Measurements in Groundwater in North Carolina since 2010", 
       x = "Year", 
       y = "Specific conductance in uS/cm at 25 degrees C", 
       caption = "USGS, 2024.") + 
  geom_smooth(method = "lm",  fill="#69b3a2", se = TRUE) 
sc_gw 

sc_sw <- ggplot(data = sc_surfacewater, aes(x = ActivityStartDate, y = ResultMeasureValue)) + 
  geom_point() + 
  labs(title = "Specific Condutance Measurements in Surface Water in North Carolina since 2010", 
       x = "Year", 
       y = "Specific conductance in uS/cm at 25 degrees C", 
       caption = "USGS, 2024.") + 
  geom_smooth(method = "lm",  fill="#69b3a2", se = TRUE) 
sc_sw 

