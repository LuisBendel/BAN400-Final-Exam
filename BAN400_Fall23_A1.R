library(tidyverse)
library(readr)

# read data and convert to numeric
bacedata <- read_csv("BACEDATA.csv", skip = 2) %>% 
  select(-OBS, -CODE, -COUNTRY) %>% 
  map_df(., as.numeric)

# we now have many NAs in the data, however, in the original dataset, this is
# missing data as indicated by the dot ".". All columns containing NAs:
bacedata %>% 
  map_lgl(., anyNA) %>% 
  which()

# function to return distinct values from original data that cause NAs during conversion
NA_produced_check <- function(column) {
  converted <- as.numeric(column)
  
  col_vector <- as.vector(column)
  
  NA_origins <- col_vector[is.na(converted)] %>% unique()
  
  return(ifelse(length(NA_origins) == 0, "No NAs in Column", NA_origins))
}

# check for values that cause NAs during conversion
NA_originated_values <- read_csv("BACEDATA.csv", skip = 2) %>% 
  select(-OBS, -CODE, -COUNTRY) %>% 
  map_chr(., NA_produced_check) %>% 
  unique()

# display distinct values that cause NAs during conversion
# we see it's only dots, so we do not have to worry about them now
print(paste0("The following values in the original data cause NAs: '",
             NA_originated_values[NA_originated_values != "No NAs in Column"], "'"))

