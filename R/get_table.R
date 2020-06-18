#' Harvest API v2 get request wrapper
#'
#' Submits get requests to the Harvest API v2. The function is a wrapper around the httr::GET function and returns the Harvest table.
#'
#' @param table Harvest API table that does not require specific ID's for the GET request. Refer to \href{https://help.getharvest.com/api-v2}{Harvest APIv2}. Possible values include the following as a string:  (\code{"clients"} or \code{"invoices"} or \code{"estimates"} or \code{"expenses"} or \code{"tasks"} or \code{"time_entries"} or \code{"projects"} or \code{"roles"} or \code{"users"})
#' @param user Harvest account number for authentication. Register at \href{https://id.getharvest.com/developers}{Harvest Developers}.
#' @param key Harvest API key for API authentication. Register at \href{https://id.getharvest.com/developers}{Harvest Developers}.
#' @param email Optional argument, requester email address for the "From" header in the get request.
#' @param query Optional argument, API query parameters to be provided in a list. Refer to \href{https://help.getharvest.com/api-v2}{Harvest APIv2} for acceptable parameters for each table. A few examples include:  (\code{from = "2018-1-1"} or \code{to = "2018-3-31"} or \code{project_id = "1234"} or \code{client_id = "1234"} or \code{user_id = "1234"})
#' @param verbose logical; passed to httr; A verbose connection provides much more information about the flow of information between the client and server. \href{https://github.com/r-lib/httr}{httr documentation}
#' @param auto_retry logical;
#' @param plan character
#' @param ... tbd
#' @return tbl_df
#'
#' @examples
#'
#' user_id <- 'your_user_id'
#' api_key <- paste0('Bearer ','you_api_key')
#' get_table(table = 'projects', user = user_id,key = api_key)
#' get_table(table = 'time_entries' , user = user_id, key = api_key, email = 'your_email',query = list(from = '2018-01-01', to = '2018-03-31'))
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#' @references \url{https://id.getharvest.com/developers}, \url{https://help.getharvest.com/api-v2}, \url{https://github.com/r-lib/httr}
#'
#' @importFrom magrittr %>%
#' @import httr
#'
#' @export
#
get_table <- function(
  table = NULL,
  user = NULL,
  key = NULL,
  email = '',
  query=NULL,
  verbose=FALSE,
  auto_retry = FALSE,
  plan = 'multiprocess',
  ...){
  url <- paste0('v2/', table) %>%
    httr::modify_url(url="https://api.harvestapp.com",
                     path=.,
                     query=query)

  response <- harvestR:::get_request(url = url,
                                     user = user,
                                     key = key,
                                     email = email,
                                     auto_retry = auto_retry)

  if(response$total_pages > 10){
    future::plan(plan)
    return_df <- response[[table]]
    urls <- purrr::map(2:response$total_pages, function(x) httr::modify_url(url, query = list(page = x)))
    if(length(urls)>100){
      message('More than 100 pages')
    }
    next_responses <- furrr::future_map(urls, function(x) harvestR:::get_request(url = x,
                                                                                 user = user,
                                                                                 key = key,
                                                                                 email = email,
                                                                                 auto_retry = auto_retry))
    return_df <- purrr::map(next_responses, table) %>%
      dplyr::bind_rows(return_df, .)
  } else {
    return_df <- response[[table]]
  }
  return(return_df)
}
