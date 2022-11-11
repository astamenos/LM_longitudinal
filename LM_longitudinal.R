# Import libraries
library(tidyverse)

# Function to 
get_data <- function(identifier, limit = 50000, offset = 0, time_var) {
  require(jsonlite)
  
  URL <- sprintf('https://healthdata.gov/resource/%s.json', identifier)
  parameters <- sprintf('?limit=%s&offset=%s', limit, offset, time_var)
  
  return(data)
}

lm_df <- get_data('aitj-yx37', limit = 90000, offset = 0, time_var = 'week')

for(i in 1:6) {
  batch <- get_data('aitj-yx37', limit = 90000, offset = 90000*i, time_var = 'week')
}