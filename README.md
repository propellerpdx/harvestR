# harvestR <img src="man/figures/logo.png" align="right" height="125" width="125" />

harvestR is a get wrapper to extract tables from the Harvest API v2. 

## Installation
To install harvestR run the following:
``` r
devtools::install_github("propellerpdx/harvestR")
```

## Credentials
Harvest requires credentials to make any API requests. Harvest supports personal access tokens and oauth2, for more info refer to [Harvest](https://help.getharvest.com/api-v2/authentication-api/authentication/authentication/).

### PAT  
To authenticate using a personal access token, you need an account ID and personal access token which can be obtained [here](https://id.getharvest.com/developers). The user account is shared by all users in that account and personal access token is specific to the user. These credentials can be used in `get_table()` by passing them to the `user` and `key` parameters. 
``` r
projects <- get_table(table = 'projects', user = 'not_a_real_account', key 'Bearer fake_personal_access_token')
```
**Note:** you must add `Bearer ` including the space to your personal access token in order for it to work. This is a [Harvest requirement](https://help.getharvest.com/api-v2/authentication-api/authentication/authentication/).

harvestR provides functions for users to manage their credentials using the [keyring](https://github.com/r-lib/keyring) package. This allows users to avoid storing secrets in their code and allows them to keep from retyping them every time they make a call. To do so run the below code and add the account ID and personal access tokens in the prompts:
``` r
create_harvest_creds()
```  
**Note:** you must add `Bearer ` including the space to your personal access token in order for it to work. This is a [Harvest requirement](https://help.getharvest.com/api-v2/authentication-api/authentication/authentication/).

Once the credentials are created, they are saved on the system keyring so they can be accessed in future interactive R sessions. By running the below code harvestR will automatically find your credentials when running `get_table()`.  

``` r
set_harvest_creds()
```  

If at any point you need to update your token, you can call `create_harvest_creds()` again - it will delete the old credentials once you have provided the new ones. If you want to delete the credentials from your keyring and not add new ones, run `harvestR::delete_harvest_creds()`.

### Oauth  
If users require oath support, tokens can passed `get_table()` as a parameter `token`. Tokens are forwarded to `httr::config`. This functionality has not been tested as of this release.
``` r
get_table(table = 'projects', token = my_token)
```  
  
### Non-Interactive Sessions  
For non-interactive users such as Rscript, harvestR will still attempt to retrieve your account ID and personal access token from the environment variables `HARVEST_ACCOUNT_ID` and `HARVEST_PAT`. For additional information for storing credentials refer to [usethis](https://usethis.r-lib.org/articles/articles/git-credentials.html).
  
## Get Tables
The `get_table()` function gets tables from the [Harvest API](https://help.getharvest.com/api-v2/) by name and coerces the data into a data frame. A number of optional parameters are in place to add query parameters, configure parallel processing, and provide messaging for logs. A few examples are provided below.

``` r
# Request the projects table
projects <- get_table(table = 'projects')

# Request the time_entries table for 3 months
time_entries <- get_table(table = 'time_entries', query = list(from = '2018-01-01', to = '2018-03-31'))

# Request the time_entries table for 3 months in with parallel requests
time_entries <- get_table(table = 'time_entries', query = list(from = '2018-01-01', to = '2018-03-31'), plan_options = list(strategy = "multicore"))

# Request the time_entries table for 3 months in parallel mode with automatic retries lowered to 1 (times) and messaging on (quiet)
# Windows does not support multicore plans, users must use multisession
time_entries <- get_table(table = 'time_entries', query = list(from = '2018-01-01', to = '2018-03-31'), plan_options = list(strategy = "multisession"), times = 1, quiet = F)

# Request using an httr option (see `httr::httr_options()`) and an oauth token
projects <- get_table(table = 'projects', http_version = 2, token = my_token)
```
