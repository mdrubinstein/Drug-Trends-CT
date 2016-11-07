#yaml_process <- . %>%
#  yaml::yaml.load_file() %>%
#  strsplit(split = ' ') %>% 
#  unlist()

yaml_process <- function(file, type = 'col_name') {
  file <- file %>%
    yaml::yaml.load_file() %>%
    unlist()
  if(type == 'col_type')      file <- file %>% as.vector()
  else if(type == 'col_name') file <- file %>% names()
  else (print('Type must be equal to col_type or col_name'))
  return(file)
}

slice_sums <- function(df, var, all = FALSE) {
  subvars    <- c('ethnicity', 'race', 'sex')
  if(all == F) subvars <- subvars[-grep('TRUE', subvars %in% var)]

  pop_sum    <- lazyeval::interp(~ sum(a), a = as.name('population'))
  grp_var    <- lazyeval::interp(~ b, b = as.name(var))
  county     <- lazyeval::interp(~ c, c = as.name('county'))
  year       <- lazyeval::interp(~ d, d = as.name('year'))

  if(all == F) {
  new_rows <- df %>% 
    group_by_(.dots = grp_var, county, year) %>%
    summarise_(.dots  = setNames(list(pop_sum), 'population')) %>%
    as.data.frame()
  } else if(all == T) {
  new_rows <- df %>% 
    group_by_(.dots = county, year) %>%
    summarise_(.dots  = setNames(list(pop_sum), 'population')) %>%
    as.data.frame()
  }
  
  a <- rep('total', nrow(new_rows))
  for(i in 1:length(subvars)) {
    new_rows[[subvars[i]]] <- a
  }
  return(new_rows)
}

var_totals   <- function(df, var) {
  pop_sum    <- lazyeval::interp(~ sum(a), a = as.name('population'))
  grp_var    <- lazyeval::interp(~ b, b = as.name(var))
  county     <- lazyeval::interp(~ c, c = as.name('county'))
  year       <- lazyeval::interp(~ d, d = as.name('year'))
  
  new_rows <- df %>% 
    group_by_(.dots = year, county, grp_var) %>%
    summarise_(.dots  = setNames(list(pop_sum), 'population')) %>%
    as.data.frame()
  
  a <- rep('total', nrow(new_rows))
  new_rows[[var]] <- a

  #df <- rbind(df, new_rows)
  return(new_rows)
}


