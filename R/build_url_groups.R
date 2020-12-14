#' Build url groups for get_requests
#'
#' Harvest API v2 only accepts 100 urls every 15 seconds. build_url_groups breaks urls into groups of 100 naming the last group, "Last_Group". Groups run requests and then R sleeps for 15 seconds get requests to the Harvest API v2. The function is a wrapper around the httr::GET function and safely returns the httr result as result or error.
#'
#' @param url Character vector of urls of the get request
#' @inheritParams get_request
#'
#' @return named list
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#'

build_url_groups <- function(urls = NULL,
                             ...){

  input_params <- list(...)
  if(is.null(input_params$quiet)) input_params$quiet <- TRUE

  request_groups <- NULL
  for(i in 1:ceiling(length(urls) / 100)){
    # Calculates the position of the first url in the group
    start_url <- (i * 100) - 99
    # Calculates the position of the last url in the group
    end_url <- min(i * 100 , length(urls))
    # Last group needs a specific name to avoid rate limit timeout in get_requests()
    group_name <- dplyr::if_else(i*100 > length(urls), 'Last_Group', paste0('Group_',i))
    # Adds vector of urls into list
    request_groups[[group_name]] <- urls[start_url:end_url]
    if(!input_params$quiet) message(glue::glue('Group {group_name} starts with page {start_url + 1} and ends with page {end_url + 1}'))
  }
  return(request_groups)
}
