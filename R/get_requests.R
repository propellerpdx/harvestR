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
#' @import furrr
#'

get_requests <- function(urls = NULL,
                         group_name = NULL,
                         user = user,
                         key = key,
                         email = email,
                         verbose = verbose,
                         auto_retry = auto_retry,
                         plan = plan,
                         ...){
  future::plan(plan)
  responses <- furrr::future_map(urls, function(x) harvestR:::get_request(url = x,
                                                                          user = user,
                                                                          key = key,
                                                                          email = email,
                                                                          auto_retry = auto_retry))
  if(group_name != 'Last_Group'){
    Sys.sleep(15)
  }
  return(responses)
}
