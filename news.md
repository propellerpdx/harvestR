# harvestR 1.1.0

## General features

* Added messaging to `get_table()` to address verbose output issue.
* The strategy & verbose parameters are deprecated and replaced by plan_options and quiet, respectively.
* Added credential management workflow, special thanks to [joethorley](https://github.com/joethorley) for suggesting the idea - sorry for the long wait!

## Messaging

* Implementing parallel requests in `get_table()` broke verbose output. The future package has a [known limitation](https://cran.r-project.org/web/packages/future/vignettes/future-2-output.html) with messaging across parallel threads. 
* The `verbose` parameter was passed to httr to provide verbose messaging on each get request, but messages in any parallel threads are dropped even when using `sink()`. 
* To enable logging for data pipelines, we added custom messages inside the `get_table()` function which can be triggered by setting `quiet = FALSE`. 

## Parameter changes

* The `verbose` parameter is deprecated because it does not work on parallel threads. The new `quiet` is a non-standard replacement. 
* The `strategy` parameter is deprecated in favor of plan_options. The `strategy` parameter was passed to `future::plan()`, but did not allow users to modify any other parameters inside `future::plan()`. The new `plan_options` parameter allows users to pass any parameters into `future::plan()` in a named list.  
* A few parameters are now listed inside `...`  This change does not impact users outside of the vignettes, but it allows us to make changes in the future with less risk of breaking changes.

## Credential Management

* New functions that allow interactive users to securely save credentials on their system using the [keyring](https://github.com/r-lib/keyring) package.
* Refer to package readme for more information or refer to the `create_harvest_creds()` function to get started. 

# harvestR 1.0.0

## General features

* `get_table()` gains new arguments to allow auto retries, parallel get requests, and more verbose messaging.

## Executing get_table requests in parallel

* The `strategy` parameter is passed to `future::plan()` which sets a plan for how future executions are resolved. The parameter does not provide full access to all `future::plan()` parameters and functionality because the current use cases do not call for it. 

## Automatic retries

* The `auto_retry` parameter will automatically retry get requests that fail (i.e. non-200 responses) up to 3 times. It does not have intelligence only perform retries depending on the response code. It is primarily useful if the user is running batches over a poor internet connection. 
