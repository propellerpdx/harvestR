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

get_requests <- function(urls = NULL,
                         group_name = NULL,
                         user = NULL,
                         key = NULL,
                         email = NULL,
                         verbose = F,
                         auto_retry = FALSE,
                         strategy = 'sequential'){

  # Setup Vars --------------------------------------------------------------

  # If strategy is not provided we default to sequential (not parallel)
  if(is.null(strategy)==T){
    strategy <- 'sequential'
  }


  # Future Config -----------------------------------------------------------

  # Set plan for the execution
  future::plan(strategy = strategy)

  # Get Requests --------------------------------------------------------

  start_time <- Sys.time()
  # urls is a character vector no greater than 100 in length
  responses <- furrr::future_map(urls, function(x) harvestR:::get_request(url = x,
                                                                          user = user,
                                                                          key = key,
                                                                          email = email,
                                                                          auto_retry = auto_retry,
                                                                          verbose = verbose))
  # If responses are returned in less than 15 seconds, R waits to avoid Harvest rate limiting
  end_time <- Sys.time()
  time <- lubridate::seconds(end_time - start_time)
  if(time < 15 & group_name != 'Last_Group'){
    Sys.sleep(15 - time)
  }
  return(responses)
}
