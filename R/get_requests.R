#' Execute multiple get_request calls
#'
#' Submits multiple get requests to the Harvest API v2. Supports parallel execution through furrr & the future package.
#'
#' @param urls Named list of character vectors of urls in groups from build_url_groups function.
#' @inheritParams get_request
#' @param ... Additional arguments, potentially passed to other functions - see details in \link[harvestR]{get_table}
#'
#' @return named list of contents of API responses for specified group of URLs
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#'
#' @references
#' \url{https://github.com/HenrikBengtsson/future} \cr
#' \url{https://davisvaughan.github.io/furrr} \cr
#' \url{https://rstudio.com/resources/rstudioconf-2020/parallel-computing-with-r-using-foreach-future-and-other-packages} \cr
#' \url{https://help.getharvest.com/api-v2/introduction/overview/general}
#'
#' @seealso [future::plan()] for details regarding how to implement parallel api calls
#'
#' @examples
#' urls = list('url_1', 'url_2', 'etc')
#'
#' get_requests(url = 'https://api.harvestapp.com/v2/projects',
#'              user = user_id,
#'              key = api_key,
#'              email = 'your_email'
#'              auto_retry = F,
#'              quiet = T,
#'              strategy = 'sequential')
#'
get_requests <- function(urls = NULL,
                         group_name = NULL,
                         user = NULL,
                         key = NULL,
                         email = NULL,
                         ...){

  # Setup Vars --------------------------------------------------------------

  # Default ... params
  input_params <- list(...)
  if(is.null(input_params$auto_retry)) input_params$auto_retry <- FALSE
  if(is.null(input_params$quiet)) input_params$quiet <- TRUE
  if(is.null(input_params$strategy)){
    input_params$strategy <- 'sequential'
    if(!input_params$quiet) message('You have not provided a strategy, so sequential will be used')
  }

  # If retry is not provided, it's first try, function is recursive.
  if(is.null(input_params$retry)) input_params$retry <- 0

  # Future Config -----------------------------------------------------------

  # Set plan for the execution
  future::plan(strategy = input_params$strategy)

  # Get Requests --------------------------------------------------------

  start_time <- Sys.time()
  # urls is a character vector no greater than 100 in length
  responses <- furrr::future_map(urls, function(x) harvestR:::get_request(url = x,
                                                                          user = user,
                                                                          key = key,
                                                                          email = email,
                                                                          auto_retry = input_params$auto_retry,
                                                                          quiet = input_params$quiet))

  # If responses are returned in less than 15 seconds, R waits to avoid Harvest rate limiting
  end_time <- Sys.time()
  time <- lubridate::seconds(end_time - start_time)
  if(time < 15 & group_name != 'Last_Group'){
    Sys.sleep(15 - time)
  }
  return(responses)
}
