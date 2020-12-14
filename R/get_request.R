#' Harvest API v2 get request wrapper
#'
#' Submits get requests to the Harvest API v2 with retry functionality.
#' Errors are messaged to the execution environment and set to NULL allowing user-facing function returns to provide data frame output.
#'
#' @param url Character url of the get request.
#' @param user Character account number (e.g. user = '12345') for authentication. Register at \href{https://id.getharvest.com/developers}{Harvest Developers}.
#' @param key  Character key (e.g. key = 'Bearer <secret key>') for API authentication. Register at \href{https://id.getharvest.com/developers}{Harvest Developers}.
#' @param email Character email address for API call. (Optional)
#' @param ... Additional arguments, potentially passed to other functions - see details in \link[harvestR]{get_table}
#'
#' @return contents of the API response
#'
#' @author Mark Druffel, \email{mdruffel@mark.druffel@gmail.com}
#'
#' @references
#' Set up your \href{https://id.getharvest.com/developers}{API key}
#' Read the Harvest API v2 \href{https://help.getharvest.com/api-v2}{documentation}
#' Read the httr \href{https://github.com/r-lib/httr}{documentation}
#' Refer to curl \href{https://github.com/curl/curl/blob/master/include/curl/curl.h}{documentation} for more http_version options
#'
#' @examples
#' get_request(url = 'https://api.harvestapp.com/v2/projects',
#'             user = user_id,
#'             key = api_key,
#'             email = 'your_email'
#'             auto_retry = F,
#'             quiet = T)
#'
#' @importFrom magrittr %>%
#'

get_request <- function(url = NULL,
                        user = NULL,
                        key = NULL,
                        email = NULL,
                        ...){

  # Setup Vars --------------------------------------------------------------

  # Default ... params
  input_params <- list(...)
  if(is.null(input_params$auto_retry)){ input_params$auto_retry <- FALSE }
  if(is.null(input_params$quiet)){ input_params$quiet <- TRUE }
  if(is.null(input_params$http_version)){ input_params$http_version <- 2 }

  # If retry is not provided, it's first try, function is recursive.
  if(is.null(input_params$retry)){ input_params$retry <- 0 }

  # Get Request -------------------------------------------------------------
  # The curl version installed on EC2 / Lambda is slightly different from local and does not fall back to HTTP/1.1., http_version=2 forces HTTP/1.1.
  response <- httr::with_config(config = httr::config(http_version = input_params$http_version),
                                httr::GET(url,
                                          httr::add_headers("Harvest-Account-ID" = user,
                                                            Authorization = key,
                                                            'User-Agent= R harvestR',
                                                            'From' = email)))

  if(!input_params$quiet) message(glue::glue('request_url: {url} returned a status of {response$status_code}'))

  if(httr::http_error(response)){
    if(input_params$retry <= 2 & input_params$auto_retry == T){

      # Retry ---------------------------------------------------------------
      input_params$retry <- input_params$retry + 1
      if(!input_params$quiet) message(glue::glue('Attempting retry {input_params$retry} for {response$url} in 15 seconds'))

      # Wait to avoid Harvest rate limiting
      Sys.sleep(15)

      harvestR:::get_request(url = url,
                             user = user,
                             key = key,
                             email = email,
                             auto_retry = input_params$auto_retry,
                             quiet = input_params$quiet,
                             retry = input_params$retry,
                             http_version = input_params$http_version)

    } else {

      # Out of retries ------------------------------------------------------
      content <- NULL
      if(!input_params$quiet) warning(glue::glue('No more retries left, get_request() failed for {response$url}'))

      return(content)
    }

  } else {

    # Good response ---------------------------------------------------------
    content <- httr::content(response, as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(., flatten = T)

    # send messages
    if(!input_params$quiet) harvestR:::get_response_messages(response, content)

    return(content)
  }
}
