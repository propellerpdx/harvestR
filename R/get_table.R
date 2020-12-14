#' Harvest API v2 get request wrapper
#'
#' Submits get requests to the Harvest API v2. The function is a wrapper around the httr::GET function and returns the Harvest table.
#'
#' @param table Character table name to be submitted to the Harvest API v2. Refer to \href{https://help.getharvest.com/api-v2}{Harvest APIv2}.
#' @param ... Additional arguments, potentially passed to other functions - see details.
#' @inheritParams get_request
#' @inheritParams get_requests
#'
#' @details
#' Description of ... parameters
#' \itemize{
#'    \item \code{auto_retry} Logical scalar. Control whether to retry GET requests upon failure. T results 2 retries (3 attempts total). F results in no retries (1 attempt total).
#'    \item \code{quiet} Logical scalar. Controls messaging. T will result in displaying only errors in the console. F will results in displaying messages, warnings, and errors.
#'    Depending on strategy, messages from httr::GET may not be surfaced to the console due to multiple R sessions running and stderror relaying issues. See references.
#'    \item \code{strategy} Character vector. Passed to \code{future::plan}. Examples include \code{sequential}, \code{multiprocess}, & \code{multicore}
#'    }
#'
#' @return dataframe with contents of requested Harvest table
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com},
#'         Kyle Crawford, \email{kcrawford@propellerpdx.com}
#'
#' @references
#' \url{https://id.getharvest.com/developers} \cr
#' \url{https://help.getharvest.com/api-v2} \cr
#' \url{https://cran.r-project.org/web/packages/future/vignettes/future-2-output.html} (Known Limitations)
#'
#' @examples
#'
#' user_id <- 'your_user_id'
#'
#' api_key <- paste0('Bearer ', 'you_api_key')
#'
#' get_table(table = 'projects',
#'           user = user_id,
#'           key = api_key)
#'
#' get_table(table = 'projects',
#'           user = user_id,
#'           key = api_key,
#'           email = 'your_email',
#'           auto_retry = F,
#'           quiet = T,
#'           strategy = 'sequential',
#'           query = list(from = '2018-01-01', to = '2018-03-31'))
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
get_table <- function(table = NULL,
                      user = NULL,
                      key = NULL,
                      email = '',
                      query=NULL,
                      ...){

  # Check internet connection -----------------------------------------------
  if(!curl::has_internet()) stop("Please check your internet connection.")

  # Setup Vars --------------------------------------------------------------

  # Default ... params
  input_params <- list(...)
  if(is.null(input_params$auto_retry)) input_params$auto_retry <- FALSE
  if(is.null(input_params$quiet) && is.null(input_params$verbose)){
    input_params$quiet <- TRUE
  } else if(is.null(input_params$quiet) && !is.null(input_params$verbose)) {
    input_params$quiet <- switch(input_params$verbose +1, T, F)
  }
  if(is.null(input_params$quiet)) input_params$quiet <- TRUE
  if(is.null(input_params$strategy)){
    input_params$strategy <- 'sequential'
    if(!input_params$quiet) message('You have not provided a strategy, so sequential will be used')
  }

  url <- paste0('v2/', table) %>%
    httr::modify_url(url="https://api.harvestapp.com",
                     path=.,
                     query=query)

  # Check Date Params Format ------------------------------------------------

  harvestR:::check_date_format(query = query)

  # Get Request -------------------------------------------------------------

  response <- harvestR:::get_request(url = url,
                                     user = user,
                                     key = key,
                                     email = email,
                                     auto_retry = input_params$auto_retry,
                                     quiet = input_params$quiet)

  # If request is bad response$total_pages will be NULL
  if(is.null(response)){
    stop('Initial request failed. See warning message for details:')
  }
  # If quiet is off and more than 1 page, provide message.
  if(!input_params$quiet & response$total_pages > 1){
    message(glue::glue('Initial request shows {response$total_entries} records. Initiating the remaining {response$total_pages-1} requests.'))
  }

  # Get requests (multi-page) -----------------------------------------------

  if(response$total_pages > 1){
    # Pull the dataframe from the initial get request
    return_df <- response[[table]]
    # Build urls for the remaining requests
    urls <- purrr::map(2:response$total_pages, function(x) httr::modify_url(url, query = list(page = x)))
    # Build url groups by sets of 100 to avoid rate limiting by Harvest
    url_groups <- harvestR:::build_url_groups(urls = urls,
                                              quiet = input_params$quiet)
    # Pass url and the name of the url group,
    # last group is named 'Last_Group' which keeps get_requests from pausing if the last group finishes in less than 15 seconds
    responses <- purrr::map2(url_groups, names(url_groups), function(x, y) harvestR:::get_requests(urls = x,
                                                                                                   group_name = y,
                                                                                                   user = user,
                                                                                                   key = key,
                                                                                                   email = email,
                                                                                                   auto_retry = input_params$auto_retry,
                                                                                                   strategy = input_params$strategy,
                                                                                                   quiet = input_params$quiet))
    # responses uses map2 around a future_map call so it creates another level of list.
    # Climb down the list and get the tables, then bind them all into a data frame
    return_dfs <-     purrr::map(responses, function(x)
      purrr::map(x, function(y) y[[table]]) %>% dplyr::bind_rows()) %>%
      dplyr::bind_rows()
    # Add responses to the initial response
    return_df <- dplyr::bind_rows(return_df, return_dfs)
  } else {
    return_df <- response[[table]]
  }
  return(return_df)
}
