# harvestR <img src="man/figures/logo.png" align="right" height="125" width="125" />

harvestR is provides tools for extracting data from the Harvest API v2. harvestR supports get statements only and provides a set of tools for parallel execution. 

## Installation
harvestR will not be submitted to CRAN given the numerous dependencies including the Harvest API. The library can be installed from github. 
``` r
devtools::install_github("propellerpdx/harvestR")
```

## Credentials
Harvest requires an account ID and personal access token to make API calls which can be found [here](https://id.getharvest.com/developers) when logged in. Once the user has obtained those credentials, they can manage them using any credential management tool of their choice and pass them into harvestR functions with the parameters `user` and `key`. For example, the user can bypass the defaults in `get_table()` by running the below code.

``` r
projects <- get_table(table = 'projects', user = 'not_a_real_account', key 'Bearer fake_personal_access_token')
```

Users can also use harvestR to manage their credentials using the [keyring](https://github.com/r-lib/keyring) package. To do so run the below code and add the account ID and personal access tokens in the prompts:
  
``` r
harvestR::create_harvest_creds()
```  
  
**Note: you must add `Bearer ` including the space to your personal access token in order for it to work. This is a [Harvest requirement](https://help.getharvest.com/api-v2/authentication-api/authentication/authentication/).**  

Once the credentials are created, they are saved on the system keyring so they can be accessed in future interactive R sessions. By running the below code harvestR will automatically find your credentials when running `get_table()`.  

``` r
harvestR::set_harvest_creds()
```  

If at any point you need to update your token, you can call `create_harvest_creds()` again - it will delete the old credentials once you have provided the new ones. If you want to delete the credentials from your keyring and not add new ones, run the below code.  

``` r
harvestR::delete_harvest_creds()
```  
  
### Non-Interactive Sessions
For non-interactive users such as Rscript, harvestR will still attempt to retrieve your account ID and personal access token from the environment variables `HARVEST_ACCOUNT_ID` and `HARVEST_PAT`. You can use other means of adding the credentials to the system environment variables, we use the Renviron.site file for our servers and the withr package for other situations. 
  
## Get Tables
The `get_table()` function gets tables from the [Harvest API](https://help.getharvest.com/api-v2/) by name and coerces the data into a data frame. A number of optional parameters are in place to add query parameters, configure parallel processing, and provide messaging for logs. A few examples are provided below.

``` r
# Request the projects table
projects <- get_table(table = 'projects')

# Request the time_entries table for 3 months
time_entries <- get_table(table = 'time_entries', email = 'your_email', query = list(from = '2018-01-01', to = '2018-03-31'))

# Request the time_entries table for 3 months in with parallel requests
time_entries <- get_table(table = 'time_entries', email = 'your_email', query = list(from = '2018-01-01', to = '2018-03-31'), plan_options = list(strategy = "multicore"))

# Request the time_entries table for 3 months in parallel mode with auto retrying (set to make 3 attempts) and messaging
time_entries <- get_table(table = 'time_entries', email = 'your_email', query = list(from = '2018-01-01', to = '2018-03-31'), plan_options = list(strategy = "multicore"), auto_retry = T, quiet = F)
```
