#' Harvest API v2 get request wrapper
#'
#' Submits get requests to the Harvest API v2 with retry functionality.
#' Errors are messaged to the execution environment and set to NULL allowing user-facing function returns to provide data frame output.
#'
#' @param url Character url of the get request.
#' @param user Character account number (e.g. user = '12345') for authentication. Register at \href{https://id.getharvest.com/developers}{Harvest Developers}.
#' @param key  Character key (e.g. key = 'Bearer <secret key>') for API authentication. Register at \href{https://id.getharvest.com/developers}{Harvest Developers}.
#' @param email Character email address for API call. (Optional)
#' @param verbose Logical scalar. Should the function provide verbose messaging back on each step?
#' @param ... Internally used to indicated that the call is a retry.
#'
#' @return response object
#'
#' @examples
#'
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#' @references
#' \url{https://id.getharvest.com/developers}
#' \url{https://help.getharvest.com/api-v2}
#' \url{https://github.com/r-lib/httr}
#' \url{https://curl.haxx.se/libcurl/c/CURLOPT_HTTP_VERSION.html}
#'
#' @importFrom magrittr %>%
#'

get_request <- function(url = NULL,
                        user = NULL,
                        key = NULL,
                        email = NULL,
                        auto_retry = F,
                        verbose = F,
                        ...){


  # Setup Vars --------------------------------------------------------------

  # If retry is not provided, it's first try, function is recursive.
  input_params <- list(...)
  if(is.null(input_params$retry)){
    input_params$retry <- 0
  }

  # Get Request -------------------------------------------------------------

  response <- httr::with_config(config = httr::config(verbose = verbose, http_version = 2),
                                httr::GET(url,
                                          httr::add_headers("Harvest-Account-ID" = user,
                                                            Authorization = key,
                                                            'User-Agent= R harvestR',
                                                            'From' = email)))
  if(verbose==T){
    message(glue::glue('{url} returned a status of {response$status_code}'))
  }
  if(response$status_code==200){
    response_extract <- httr::content(response, as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(., flatten = T)
    return(response_extract) # Do we need this?
  } else if (input_params$retry <= 2 & auto_retry == T) {

    # Error Handling ----------------------------------------------------------

    input_params$retry <- input_params$retry + 1
    message(glue::glue('Attempting retry {input_params$retry} for {response$url} in 15 seconds'))
    Sys.sleep(15)
    get_request(url = url,
                user = user,
                key = key,
                email = email,
                auto_retry = auto_retry,
                verbose = verbose,
                retry = input_params$retry)
  } else {
    warning(glue::glue('No more retries left, get_request() failed for {response$url}.'))
    response_extract <- NULL
  }
  return(response_extract)
}
