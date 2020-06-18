#' Harvest API v2 get wrapper
#'
#' Submits get requests to the Harvest API v2. The function is a wrapper around the httr::GET function and safely returns the httr result as result or error.
#'
#' @param url tbd
#' @inheritParams get_table
#'
#' @return named list
#'
#' @examples
#'
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#' @references
#' \url{https://id.getharvest.com/developers},
#' \url{https://help.getharvest.com/api-v2},
#' \url{https://github.com/r-lib/httr}
#'
#' @importFrom magrittr %>%
#' @import purrr
#' @import httr
#' @import furrr
#' @import glue
#'
get_request <- function(url = NULL,
                        user = NULL,
                        key = NULL,
                        email = NULL,
                        verbose = F,
                        auto_retry = F,
                        ...){
  input_params <- list(...)
  if(is.null(input_params$retry)){
    input_params$retry <- 0
  }
  response <- httr::with_config(config = httr::config(verbose = verbose, http_version = 2),
                                httr::GET(url,
                                          httr::add_headers("Harvest-Account-ID" = user,
                                                            Authorization = key,
                                                            'User-Agent=Propeller R API Helper (mdruffel@propellerpdx.com)',
                                                            'From' = email)))
  if(response$status_code==200){
    response_extract <- httr::content(response, as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(., flatten = T)
    return(response_extract)
  } else if (input_params$retry <= 2 & auto_retry == T) {
    input_params$retry <- input_params$retry + 1
    message(glue::glue('Requested {response$url} and received {response$headers$status}. Attempting retry {input_params$retry} in 15 seconds'))
    Sys.sleep(15)
    get_request(url = url,
                user = user,
                key = key,
                email = email,
                verbose = verbose,
                auto_retry = auto_retry,
                retry = input_params$retry)
  } else {
    warning(glue::glue('Requested {response$url} and received {response$headers$status}. No more retries left, get_request() failed.'))
    response_extract <- NULL
  }
  return(response_exract)
}

