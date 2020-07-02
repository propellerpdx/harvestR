# harvesR 1.0.0

## General features

* `get_table()` gains new arguments to allow auto retries, parallel get requests, and more verbose messaging.

## Executing get_table requests in parallel

* The `strategy` parameter is passed to `future::plan()` which sets a plan for how future executions are resolved. The parameter does not provide full access to all `future::plan()` parameters and functionality because the current use cases do not call for it. 

## Automatic retries

* The `auto_retry` parameter will automatically retry get requests that fail (i.e. non-200 responses) up to 3 times. It does not have intelligence only perform retries depending on the response code. It is primarily useful if the user is running batches over a poor internet connection. 
