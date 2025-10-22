
# 1a. Load the necessary packages
library(tidyverse)
library(lubridate)
library(here)

# 1b. Check ny working directory
getwd() 

# 1c. Read in all four raw data files using the consistent variable names (O3 for Ozone)
# If you are still using the "Data/Raw" file structure:
EPAAir_O3_NC2018 <- read.csv(
  file = here("Data/Raw/EPAair_O3_NC2018_raw.csv"),
  stringsAsFactors = TRUE)

EPAAir_O3_NC2019 <- read.csv(
  file = here("Data/Raw/EPAair_O3_NC2019_raw.csv"),
  stringsAsFactors = TRUE)

EPAAir_PM25_NC2018 <- read.csv(
  file = here("Data/Raw/EPAair_PM25_NC2018_raw.csv"),
  stringsAsFactors = TRUE)

EPAAir_PM25_NC2019 <- read.csv(
  file = here("Data/Raw/EPAair_PM25_NC2019_raw.csv"),
  stringsAsFactors = TRUE)

# 2. Add the appropriate code to reveal the dimensions
dim(EPAAir_O3_NC2018)
dim(EPAAir_O3_NC2019)
dim(EPAAir_PM25_NC2018)
dim(EPAAir_PM25_NC2019)

## Wrangle individual datasets to create processed files.

# --- Ozone 2018 Processing ---
EPAAir_O3_NC2018_P <- EPAAir_O3_NC2018 %>%
  # Q3: Convert Date to a Date object
  mutate(Date = lubridate::mdy(Date)) %>%
  # Q4: Select the required columns
  select(Date, DAILY_AQI_VALUE, Site.Name, AQS_PARAMETER_DESC, COUNTY, SITE_LATITUDE, SITE_LONGITUDE)

# --- Ozone 2019 Processing ---
EPAAir_O3_NC2019_P <- EPAAir_O3_NC2019 %>%
  mutate(Date = lubridate::mdy(Date)) %>% # Q3
  select(Date, DAILY_AQI_VALUE, Site.Name, AQS_PARAMETER_DESC, COUNTY, SITE_LATITUDE, SITE_LONGITUDE) # Q4

# --- PM2.5 2018 Processing ---
EPAAir_PM25_NC2018_P <- EPAAir_PM25_NC2018 %>%
  mutate(Date = lubridate::mdy(Date)) %>% # Q3
  select(Date, DAILY_AQI_VALUE, Site.Name, AQS_PARAMETER_DESC, COUNTY, SITE_LATITUDE, SITE_LONGITUDE) %>% # Q4
  # Q5: Fill all cells in AQS_PARAMETER_DESC with "PM2.5"
  mutate(AQS_PARAMETER_DESC = factor("PM2.5")) 

# --- PM2.5 2019 Processing ---
EPAAir_PM25_NC2019_P <- EPAAir_PM25_NC2019 %>%
  mutate(Date = lubridate::mdy(Date)) %>% # Q3
  select(Date, DAILY_AQI_VALUE, Site.Name, AQS_PARAMETER_DESC, COUNTY, SITE_LATITUDE, SITE_LONGITUDE) %>% # Q4
  # Q5: Fill all cells in AQS_PARAMETER_DESC with "PM2.5"
  mutate(AQS_PARAMETER_DESC = factor("PM2.5")) 

# --- Q6: Save Processed Files (Code for Rmd) ---
# NOTE: This code is commented out but is the correct structure for saving the files.
file_path_processed <- here::here("Data", "Processed")
write_csv(EPAAir_O3_NC2018_P, file.path(file_path_processed, "EPAair_O3_NC2018_processed.csv"))
write_csv(EPAAir_O3_NC2019_P, file.path(file_path_processed, "EPAair_O3_NC2019_processed.csv"))
write_csv(EPAAir_PM25_NC2018_P, file.path(file_path_processed, "EPAair_PM25_NC2018_processed.csv"))
write_csv(EPAAir_PM25_NC2019_P, file.path(file_path_processed, "EPAair_PM25_NC2019_processed.csv"))


## Combine datasets

# Q7. Combine the four processed datasets with rbind.
combined_data <- rbind(EPAAir_O3_NC2018_P, EPAAir_O3_NC2019_P, EPAAir_PM25_NC2018_P, EPAAir_PM25_NC2019_P)

# Define the list of common sites for the filter step
common_sites <- c("Linville Falls", "Durham Armory", "Leggett", "Hattie Avenue",
                  "Clemmons Middle", "Mendenhall School", "Frying Pan Mountain",
                  "West Johnston Co.", "Garinger High School", "Castle Hayne",
                  "Pitt Agri. Center", "Bryson City", "Millbrook School")

# Q8. Wrangle your new dataset with a pipe function (%>%)
tidy_combined_data <- combined_data %>%
  # 1. Filter: Include only the sites in the common_sites list
  filter(Site.Name %in% common_sites) %>%
  
  # 2. Split-Apply-Combine: Group and summarise to get daily means
  group_by(Date, Site.Name, AQS_PARAMETER_DESC, COUNTY) %>%
  summarise(
    DAILY_AQI_VALUE = mean(DAILY_AQI_VALUE, na.rm = TRUE),
    SITE_LATITUDE = mean(SITE_LATITUDE, na.rm = TRUE),
    SITE_LONGITUDE = mean(SITE_LONGITUDE, na.rm = TRUE),
    .groups = 'drop' 
  ) %>%
  
  # 3. Mutate: Add Month and Year columns
  mutate(
    Month = lubridate::month(Date),
    Year = lubridate::year(Date)
  )

# Q9. Spread your datasets (pivot_wider)
tidy_final_data <- tidy_combined_data %>%
  pivot_wider(
    names_from = AQS_PARAMETER_DESC,       # Creates the 'Ozone' and 'PM2.5' columns
    values_from = DAILY_AQI_VALUE         # Fills them with the AQI values
  )

# Q10. Call up the dimensions of your new tidy dataset.
dim(tidy_final_data)

# Expected Dimensions: 14,752 rows and 9 columns

# Q11. Save your processed dataset (Code for Rmd)
write_csv(tidy_final_data, file.path(here::here("Data", "Processed"), "EPAair_O3_PM25_NC1819_Processed.csv"))

## Generate summary tables

# Q12. Generate a summary data frame.
summary_data <- tidy_final_data %>%
  # Group by site, month, and year
  group_by(Site.Name, Month, Year) %>%
  
  # Generate the mean AQI values
  summarise(
    Mean_Ozone_AQI = mean(Ozone, na.rm = TRUE),
    Mean_PM25_AQI = mean(`PM2.5`, na.rm = TRUE), # Use backticks for the 'PM2.5' column name
    .groups = 'drop'
  ) %>%
  
  # Remove instances where mean ozone values are not available
  tidyr::drop_na(Mean_Ozone_AQI)

# Q13. Call up the dimensions of the summary dataset.
dim(summary_data)

# Expected Dimensions: Approximately 295 rows and 5 columns



