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
#' @references
#' Set up your \href{https://id.getharvest.com/developers}{API key}
#' Read the Harvest API v2 \href{https://help.getharvest.com/api-v2}{documentation}
#' Read the httr \href{https://github.com/r-lib/httr}{documentation}
#' Refer to curl \href{https://github.com/curl/curl/blob/master/include/curl/curl.h}{documentation} for more http_version options
#'
#' @examples
#' \dontrun{
#' get_request(url = 'https://api.harvestapp.com/v2/projects',
#'             user = user_id,
#'             key = api_key,
#'             email = 'your_email'
#'             auto_retry = F,
#'             quiet = T)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @import httr

get_request <- function(url = NULL,
                        user = NULL,
                        key = NULL,
                        email = NULL,
                        ...){

  # Setup Vars --------------------------------------------------------------

  # Default ... params
  input_params <- list(...)
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

  if(httr::http_error(response) && input_params$retry <= 2 && input_params$auto_retry == T){
    # Retry ---------------------------------------------------------------
    input_params$retry <- input_params$retry + 1
    if(!input_params$quiet) {
      message(glue::glue('request_url: {url} returned a status of {response$status_code}\nAttempting retry {input_params$retry} in 15 seconds'))
    }
    # Wait, 15 used to avoid Harvest rate limiting for parallel requests that may experience timeouts
    Sys.sleep(15)
    harvestR:::get_request(url = url,
                           user = user,
                           key = key,
                           email = email,
                           auto_retry = input_params$auto_retry,
                           quiet = input_params$quiet,
                           retry = input_params$retry,
                           http_version = input_params$http_version)
  }
  # Good response ---------------------------------------------------------
  if(!httr::http_error(response)){
    content <- httr::content(response, as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(., flatten = T)
    if(!input_params$quiet){
      harvestR:::parse_response_message(response = response, content = content, table = input_params$table)
    }
    return(content)
  } else {
    httr::stop_for_status(response,
                          task = glue::glue("return results for {response$url}"))
  }
}

#' Execute multiple get_request calls
#'
#' Submits multiple get requests to the Harvest API v2. Supports parallel execution through furrr & the future package.
#'
#' @param urls Named list of character vectors of urls in groups from build_url_groups function.
#' @inheritParams get_request
#' @inheritParams get_table
#' @param ... Additional arguments, potentially passed to other functions - see details in \link[harvestR]{get_table}
#'
#' @return named list of contents of API responses for specified group of URLs
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
#' \dontrun{
#' urls = list('url_1', 'url_2', 'etc')
#'
#' get_requests(url = 'https://api.harvestapp.com/v2/projects',
#'              user = user_id,
#'              key = api_key,
#'              email = 'your_email'
#'              auto_retry = F,
#'              quiet = T,
#'              strategy = 'sequential')
#' }
#' @importFrom furrr future_map
#' @importFrom furrr future_map_dfr
#'
get_requests <- function(urls,
                         group_name,
                         table,
                         user,
                         key,
                         email = NULL,
                         ...){
  # Get Requests --------------------------------------------------------
  input_params <- list(...)
  start_time <- Sys.time()
  # urls is a character vector no greater than 100 in length
  responses <- furrr::future_map(urls, function(x) harvestR:::get_request(url = x,
                                                                          user = user,
                                                                          key = key,
                                                                          email = email,
                                                                          auto_retry = input_params$auto_retry,
                                                                          quiet = input_params$quiet,
                                                                          table = table))
  contents <- purrr::map(table) %>%
    dplyr::bind_rows()
  end_time <- Sys.time()
  time <- lubridate::seconds(end_time - start_time)
  if(time < 15 & group_name != 'Last_Group'){
    Sys.sleep(15 - time)
  }
  return(contents)
}

#' Get Response Messages
#'
#' Sends messages to console regarding the response. This is implemented because,
#' depending on the selected strategy for the future::plan, messages from httr::GET may not be
#' surfaced to the console due to multiple R sessions running and stderror relaying issues.
#'
#' @param response harvest API response
#' @param content content from harvest API response
#'
#' importFrom glue glue

parse_response_message <- function(response, content, table){
  df <- content[[table]]
  message(glue::glue("response-url: {response$url}\n\tx-request-id: {response$headers$`x-request-id`}\n\tdate: {response$date}\n\tx-runtime: {response$headers$`x-runtime`}\n\tpages: {content$page}\n"))
  message(paste0("\t", capture.output(df[1, ]), collapse = "\n"))
  message(paste0("\t", capture.output(df[nrow(df), ]), collapse = "\n"))
}
