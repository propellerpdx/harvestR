#' Harvest API v2 get request wrapper
#'
#' Submits get requests to the Harvest API v2 with retry functionality.
#' Errors are messaged to the execution environment and set to NULL allowing user-facing function returns to provide data frame output.
#'
#' @param url Character url of the get request.
#' @param user Character account number (e.g. user = '12345') for authentication. Register at \href{https://id.getharvest.com/developers}{Harvest Developers}.
#' @param key  Character key (e.g. key = 'Bearer <secret key>') for API authentication. Register at \href{https://id.getharvest.com/developers}{Harvest Developers}.
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
#' @importFrom magrittr %>%
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @import httr

get_request <- function(url = NULL,
                        user = NULL,
                        key = NULL,
                        ...){

  # Setup Vars --------------------------------------------------------------

  # Default ... params
  input_params <- list(...)
  # Get Request -------------------------------------------------------------
  if(!input_params$quiet){
    message(glue::glue("Attempting {url}\n"))
  }
  # There is a quiet param, but it causes issues with 429 errors
  #https://github.com/r-lib/httr/issues/611
  response <- httr::RETRY(url,
                          verb = "GET",
                          config = httr::config(input_params$httr_config_opts, token = input_params$token),
                          httr::add_headers(.headers = c("Harvest-Account-ID" = user,
                                                         Authorization = key)),
                          times = input_params$times)

  if(!httr::http_error(response)){
    content <- httr::content(response, as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(., flatten = T)

    if(!input_params$quiet){
      harvestR:::parse_response_message(response = response, content = content, table = input_params$table)
    }
    return(content)
  } else{
    stop(httr::stop_for_status(reponse))
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
#' @importFrom furrr future_map
#' @importFrom furrr future_map_dfr
#'
get_requests <- function(urls,
                         table,
                         user,
                         key,
                         ...){
  # Get Requests --------------------------------------------------------
  input_params <- list(...)
  # urls is a character vector no greater than 99 in length
  responses <- furrr::future_map(urls, function(x) harvestR:::get_request(url = x,
                                                                          user = user,
                                                                          key = key,
                                                                          table = table,
                                                                          times = input_params$times,
                                                                          httr_config_opts = input_params$httr_config_opts,
                                                                          quiet = input_params$quiet,
                                                                          token = input_params$token),
                                 .options = input_params$furrr_opts)
  return(responses)
}


#' Execute multiple get_request calls with rate limiting
#'
#' @importFrom ratelimitr limit_rate
#' @importFrom ratelimitr rate

get_requests_lim <- ratelimitr::limit_rate(get_requests, ratelimitr::rate(n = 1, period = 15))



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
