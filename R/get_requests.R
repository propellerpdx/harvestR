#' Execute multiple get_request calls
#'
#' Submits multiple get requests to the Harvest API v2. Supports parallel execution through furrr & the future package.
#'
#' @param urls Named list of character vectors of urls in groups from build_url_groups function.
#' @param strategy Character vector passed to future::plan strategy parameter. Examples include 'sequential', 'multiprocess', & 'multicore'.
#' @inheritParams get_request
#'
#' @return named list
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#'
#' @seealso [future::plan()] for details regarding how to implement parallel api calls
#'
#' @references
#' \url{https://github.com/HenrikBengtsson/future}
#' \url{https://davisvaughan.github.io/furrr}
#' \url{https://rstudio.com/resources/rstudioconf-2020/parallel-computing-with-r-using-foreach-future-and-other-packages}
#' \url{https://help.getharvest.com/api-v2/introduction/overview/general}
#'
#' @importFrom lubridate seconds
#' @import furrr
#' @import future
#'

get_requests <- function(urls = NULL,
                         group_name = NULL,
                         user = NULL,
                         key = NULL,
                         email = NULL,
                         verbose = F,
                         auto_retry = NULL,
                         strategy = NULL){
  if(is.null(strategy)==T){
    strategy <- 'sequential'
  }
  future::plan(strategy = strategy)
  start_time <- Sys.time()
  responses <- furrr::future_map(urls, function(x) harvestR:::get_request(url = x,
                                                                          user = user,
                                                                          key = key,
                                                                          email = email,
                                                                          auto_retry = auto_retry,
                                                                          verbose = verbose))
  end_time <- Sys.time()
  time <- lubridate::seconds(end_time - start_time)
  if(time < 15 & group_name != 'Last_Group'){
    Sys.sleep(15 - time)
  }
  return(responses)
}
