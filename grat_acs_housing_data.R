install.packages("tidycensus")
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
census_api_key("63f5a137f3ade1ea93d2ecd54a8601fd5ba2773d", install = TRUE)
readRenviron("~/.Renviron")

v22 <- load_variables(2022, "acs5", cache = TRUE)
view(v22)

# Get ACS data for Alachua County (GEOID 12001)

# Function to get each variable: Median Gross Rent: 2022, 2018, and 2013
get_med_gross_rent <- function(year) {
  data <- get_acs(geography = "tract", 
                  variables = "B25064_001",  
                  state = "FL", 
                  county = "Alachua", 
                  year = year, 
                  survey = "acs5")
  
  View(data)  
  return(data)  
}

med_gross_rent_22 <- get_med_gross_rent(2022)
med_gross_rent_18 <- get_med_gross_rent(2018)
med_gross_rent_13 <- get_med_gross_rent(2013)

# Clean up tract name, plot estimates, account for MOE, add pts for the estimates, set titles and axes

med_gross_rent_22 %>%
  mutate(NAME = gsub("Census Tract ", "", NAME),
         NAME = gsub(", Alachua County, Florida", "", NAME)) %>%  
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) + 
  geom_point(color = "red", size = 3) +  
  labs(title = "Median Gross Rent by Census Tract in Alachua County, FL",  
       subtitle = "2022 American Community Survey 5-Year Estimates",  
       y = "Census Tract",  
       x = "ACS Median Gross Rent Estimate")  
