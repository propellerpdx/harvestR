# harvestR <img src="man/figures/logo.png" align="right" height="125" width="125" />

harvestR is provides tools for extracting data from the Harvest API v2. harvestR supports get statements only and provides a set of tools for parallel execution. 

## Installation
harvestR will not be submitted to CRAN given the numerous dependencies including the Harvest API. The library can be installed from github. 
``` r
devtools::install_github("propellerpdx/harvestR")
```

## Get Tables
The `harvestR::get_table()` function gets tables from the [Harvest API](https://help.getharvest.com/api-v2/) by name and coerces the data into a data frame. A number of optional parameters are in place including query parameters, parallel processing, and messaging. 

``` r 
# Credentials are required
user_id <- 'your_user_id'
api_key <- paste0('Bearer ','you_api_key')

# Request the projects table
get_table(table = 'projects', user = user_id,key = api_key)

# Request the time_entries table for 3 months
get_table(table = 'time_entries', user = user_id, key = api_key, email = 'your_email', query = list(from = '2018-01-01', to = '2018-03-31'))

# Request the time_entries table for 3 months in parallel mode (i.e. speed up performance)
get_table(table = 'time_entries', user = user_id, key = api_key, email = 'your_email', query = list(from = '2018-01-01', to = '2018-03-31', strategy = 'multiprocess'))

# Request the time_entries table for 3 months in parallel mode with auto retrying (set to make 3 attempts) and messaging
get_table(table = 'time_entries', user = user_id, key = api_key, email = 'your_email', query = list(from = '2018-01-01', to = '2018-03-31',  auto_retry = T, strategy = 'multiprocess', verbose = T))
```
