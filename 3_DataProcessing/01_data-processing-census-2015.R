# Program: 00_data-processing-census.R
# Author: Max Rubinstein
# Purpose: Clean and process census 2012-2015 population data
# Date modified: October 5, 2016

# Load libraries ----------------------------------------------------------------------
libraries <- c('purrr', 'tigris', 'dplyr', 'tidyr', 'stringr', 'zoo')
lapply(libraries, library, character.only = T)
source('3_RPgrms/helper-funs.R')

# Data processing ---------------------------------------------------------------------
filter_data <- . %>%
  read.fwf(widths = c(4, 2, 3, 2, 1, 1, 8, 8, 8, 8, 8, 8, 8),
           header = FALSE,
           colClasses = var_types) %>%
  set_names(var_names) %>%
  select(-matches('_r$')) %>%
  filter(state == '09') 

group_data <- . %>%
  mutate(sex  = ifelse(race_sex %% 2 == 1, 'male', 'female'),
         race = ifelse(race_sex %in% c(1:2), 'white',
                ifelse(race_sex %in% c(3:4), 'black', 'other'))) %>%
  select(-race_sex, -state) %>%
  group_by(county, race, sex, ethnicity) %>%
  summarise(est_2012 = sum(est_2012),
            est_2013 = sum(est_2013),
            est_2014 = sum(est_2014),
            est_2015 = sum(est_2015)) %>%
  mutate(ethnicity   = ifelse(ethnicity == 1, 'not hispanic', 'hispanic'))

melt_years <- . %>%
  group_by(county, race) %>%
  gather(key = year, 
         value = population,
         est_2012, est_2013, est_2014, est_2015,
         -county, -race, -sex, -ethnicity) %>%
  as.data.frame() %>%
  dmap_at('year', str_replace_all, 'est_', '')  

total_sums <- function(df) {
  df %>%
    slice_sums('race') %>%
  rbind(
    slice_sums(df, 'ethnicity')
  ) %>%
  rbind(
    slice_sums(df, 'sex')
  ) %>%
  rbind(
    slice_sums(df, 'race', all = TRUE)
  ) %>%
  rbind(
    df
  )
}
# Read and process data ---------------------------------------------------------------
file        <- '1_RawData/CensusData/pcen_v2015_y1015_txt.txt'
vars        <- '2_AnalyticFiles/varnames-census.yaml'
  var_names <- vars %>% yaml_process('col_name')
  var_types <- vars %>% yaml_process('col_type')

data        <- file %>% 
               filter_data() %>%
               group_data() %>%
               melt_years() %>%
               total_sums()

saveRDS(data, '2_AnalyticFiles/censusdata.RDS')

