# harvestR 1.1.0

## General features

* Added messaging to `get_table()` to address verbose output issue.
* The strategy, auto_retry, and verbose parameters are deprecated and replaced by plan_options, times, and quiet, respectively.
* Added credential management workflow, special thanks to [joethorley](https://github.com/joethorley) for suggesting the idea - sorry for the long wait!

## Messaging
The `get_table()` function had a `verbose` parameter was passed to `httr::verbose()`. Implementing parallel requests in `get_table()` broke the verbose output. The future package has a [known limitation](https://cran.r-project.org/web/packages/future/vignettes/future-2-output.html) with messaging across parallel threads. We expect many users will want to take advantage of parallel requests so we added custom messaging inside the `get_table()` which provides information for logging. The messaging can be turned on by setting `quiet = FALSE` in the `get_table()` call. To divert output to a log file you can wrap calls with logging functions. 

## Parameter changes
* The `verbose` parameter is deprecated because it does not work on parallel threads (refer to Messaging above for more details). The `quiet` parameter is the suggested replacement. Verbose will still be passed to `httr::config()`, but a warning will be issued for the time being to make sure users are aware of the behavior changes. 
* The `strategy` parameter is deprecated in favor of plan_options. The `strategy` parameter was passed to `future::plan()`, but did not allow users to modify any other parameters inside `future::plan()`. The new `plan_options` parameter allows users to pass any parameters into `future::plan()` in a named list.  
* The `auto_retry` parameter is deprecated because it was used to retry get requests that failed in custom logic. We moved to the `httr::RETRY()` function in order to simplify. The `times` parameter is the suggested replacement. 
* Additional arguments in ... were specifically forwarded to httr functions, but only a few were supported. `get_table()` now forwards listed arguments to `httr::RETRY` and attempts to forward all other arguments to `httr::config()` including token (for oath requests). 

## Credential Management
Joe Thorley made the suggestion of adding credential management functions to make fetching keys easier within the library. We added a few functions that are intended to provide a credential management workflow for harvestR. They enable interactive users to securely save credentials on their system using the [keyring](https://github.com/r-lib/keyring) package. Scheduled jobs can still take advantage of the system with system environment variables. Refer to the harvestR package readme for more info or refer to the `create_harvest_creds()` function to get started. 

# harvestR 1.0.0

## General features

* `get_table()` gains new arguments to allow auto retries, parallel get requests, and more verbose messaging.

## Executing get_table requests in parallel

* The `strategy` parameter is passed to `future::plan()` which sets a plan for how future executions are resolved. The parameter does not provide full access to all `future::plan()` parameters and functionality because the current use cases do not call for it. 

## Automatic retries

* The `auto_retry` parameter will automatically retry get requests that fail (i.e. non-200 responses) up to 3 times. It does not have intelligence only perform retries depending on the response code. It is primarily useful if the user is running batches over a poor internet connection. 
