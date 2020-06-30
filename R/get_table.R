#' Harvest API v2 get request wrapper
#'
#' Submits get requests to the Harvest API v2. The function is a wrapper around the httr::GET function and returns the Harvest table.
#'
#' @param table Character table name to be submitted to the Harvest API v2. Refer to \href{https://help.getharvest.com/api-v2}{Harvest APIv2}.
#' @inheritParams get_request
#' @inheritParams get_requests
#'
#' @examples
#'
#' user_id <- 'your_user_id'
#' api_key <- paste0('Bearer ','you_api_key')
#' get_table(table = 'projects', user = user_id,key = api_key)
#' get_table(table = 'time_entries' , user = user_id, key = api_key, email = 'your_email',query = list(from = '2018-01-01', to = '2018-03-31'))
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#' @references
#' \url{https://id.getharvest.com/developers}
#' \url{https://help.getharvest.com/api-v2}
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

get_table <- function(
  table = NULL,
  user = NULL,
  key = NULL,
  email = '',
  query=NULL,
  auto_retry = FALSE,
  strategy = 'sequential',
  verbose=FALSE){

  # Setup Vars --------------------------------------------------------------

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
                                     auto_retry = auto_retry,
                                     verbose=verbose)
  # If not a bad request and verbose is on, provide message. If request is bad response$total_pages will be NULL
  if(verbose == T & is.null(response$total_pages) == F){
    if(response$total_pages > 1){
      message(glue::glue('Initial request shows {response$total_entries} records. Initiating the remaining {response$total_pages-1} requests.'))
    }
  }
  if(is.null(response)){
    stop('Initial request failed. See warning message for details:')
  }

  # Get requests (multi-page) -----------------------------------------------

  if(response$total_pages > 1){
    # Pull the dataframe from the initial get request
    return_df <- response[[table]]
    # Build urls for the remaining requests
    urls <- purrr::map(2:response$total_pages, function(x) httr::modify_url(url, query = list(page = x)))
    # Build url groups by sets of 100 to avoid rate limiting by Harvest
    url_groups <- harvestR:::build_url_groups(urls = urls)
    # Pass url and the name of the url group,
    # last group is named 'Last_Group' which keeps get_requests from pausing if the last group finishes in less than 15 seconds
    responses <- purrr::map2(url_groups, names(url_groups), function(x, y) harvestR:::get_requests(urls = x,
                                                                                                   group_name = y,
                                                                                                   user = user,
                                                                                                   key = key,
                                                                                                   email = email,
                                                                                                   auto_retry = auto_retry,
                                                                                                   strategy = strategy,
                                                                                                   verbose=verbose))
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
