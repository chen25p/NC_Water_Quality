# wql package experimentation 
# Peggy Chen 
# July 12, 2024 

# Load packages 
library(wql)
library(magrittr)
library(scales) 

# Add dataset 
deq <- read.csv("deq_wq.csv")
dates <- read.csv("deq_dates.csv")

# Add dates in the actual format needed 
deq$Date <- dates$Sample.Date

# Create Date objects using base R
deq$parsed <- strptime(deq$Date, "%m/%d/%Y")

# Format them to spec
deq$Date <- format(deq$parsed, "%Y-%m-%d") 

# Fix start year 
actual_dates <- as.Date(deq$Date,format="%Y-%m-%d") %>% format("20%y-%m-%d") 

new_column <- c(actual_dates) 
deq$Actual.Dates <- new_column 

# Clean up columns 
deq <- deq %>% 
  subset(select = -c(parsed, Date)) %>% 
  rename("Dates" = Actual.Dates) 

# Turn character values into actual date class 
deq$Actual.Dates <- as.Date(deq$Actual.Dates, format = "%Y-%m-%d")

# Subset data 
deq_sc <- deq %>% 
  subset(Parameter %in% c("Specific Conductivity")) 

deq_nitrate <- deq %>% 
  subset(Parameter %in% c("Nitrate (NO3 as N)")) 

# Boxplot 
ggplot(deq_sc, aes(x = Actual.Dates, y = Result)) + 
  geom_point() +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "10 months") +
  labs(x = "Date", y = "Result") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
