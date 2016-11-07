# Program: 00_data-processing-census.R
# Author: Max Rubinstein
# Purpose: Clean and process drug deaths data, merge to census data
# Date modified: October 5, 2016

# Load libraries ----------------------------------------------------------------------
libraries <- c('purrr', 'tigris', 'dplyr', 'tidyr', 'stringr', 'zoo')
lapply(libraries, library, character.only = T)

# Define processing funs ---------------------------------------------------------
process_data <- . %>%
  readr::read_csv() %>%
  set_names(gsub(' ', '_', tolower(names(.)))) %>%
  set_names(gsub('gender', 'sex', names(.))) %>%
  dmap_at('year', as.character) %>%
  dmap_if(is.character, tolower) %>%
  filter(town != 'connecticut',
         age == 'total',
         race %in% 'total' | ethnicity %in% 'total' | sex %in% 'total') %>%
  mutate(county = substr(fips, 3, 5)) %>%
  select(-town, -fips) %>%
  group_by(year, county, ethnicity, race, sex, drug_type) %>%
  mutate(value = sum(value)) %>%
  as.data.frame() %>%
  distinct()

merge_data   <- . %>%
  right_join(census_data %>% readRDS,
            by = c('county', 'race', 'sex', 'ethnicity', 'year'))

create_rates <- . %>%
  mutate(death_rate = value/population * 100000) %>%
  dmap_at('year', ~ lubridate::year(as.Date(.x, '%Y')))

# Read and process data ----------------------------------------------------------
drugs_file  <- '1_RawData/DrugDeaths/accidentaldrugrelateddeathsbydrugtype.csv'
census_data <- '2_AnalyticFiles/censusdata.RDS' 

processed_data <- drugs_file %>% 
                  process_data() %>%
                  merge_data() %>%
                  create_rates()

# Write file to disk -------------------------------------------------------------
saveRDS(processed_data, '2_AnalyticFiles/analyticfile.RDS')
