#' \lifecycle{maturing}
#'
#' @title Harvest API v2 get request wrapper
#' @description Submits get requests to the Harvest API v2. The function is a wrapper around the httr::GET function and returns the Harvest table.
#' @param table Character table name to be submitted to the Harvest API v2. Refer to \href{https://help.getharvest.com/api-v2}{Harvest API v2}.
#' @param user Character account number (e.g. user = '12345') for authentication. Register at \href{https://id.getharvest.com/developers}{Harvest Developers}. Defaults to `get_harvest_account()`
#' @param key Character key (e.g. key = 'Bearer <secret key>') for API authentication. Register at \href{https://id.getharvest.com/developers}{Harvest Developers}. Defaults to `get_harvest_pat()`
#' @param ... Additional arguments, some are forwarded to other functions - see details.
#' @inheritParams get_request
#' @inheritParams get_requests
#'
#' @details
#' Description of ... parameters
#' \itemize{
#'    \item \code{auto_retry} Logical; control whether to retry GET requests upon failure. TRUE results 2 retries (3 attempts total). FALSE results in no retries (1 attempt total).
#'    \item \code{quiet} Logical; controls messaging. TRUE will result in displaying only errors in the console. FALSE will results in displaying messages, warnings, and errors. Depending on execution strategy, messages from httr::GET may not be surfaced to the console due to multiple R sessions running and stderror relaying issues. See \href{https://cran.r-project.org/web/packages/future/vignettes/future-2-output.html}{Future Package Output} for me details.
#'    \item \code{plan_options} List; list of parameters passed to \code{future::plan}. The strategy parameter is required to implement parallel requests, see \href{https://cran.r-project.org/web/packages/future/vignettes/future-1-overview.html}{Future Package Overview} for more details.
#'    }
#'
#' @return dataframe with contents of requested Harvest table
#'
#' @seealso `future::plan()` for more detail regarding how to implement parallel api calls
#'
#' @references
#' \href{https://help.getharvest.com/api-v2}{Harvest API v2}
#' \href{https://id.getharvest.com/developers}{Harvest Developers Page}
#' \href{https://cran.r-project.org/web/packages/future/vignettes/future-1-overview.html}{Future Pacakge Overview}
#' \href{https://cran.r-project.org/web/packages/future/vignettes/future-2-output.html}{Future Package Output}
#'
#' @examples
#' \dontrun{
#' create_harvest_creds()
#' set_harvest_creds()
#' get_table(table = 'projects')
#' get_table(table = 'time_entries',
#'           email = 'your_email',
#'           query = list(from = '2018-01-01', to = '2018-12-31'),
#'           plan_options = list(strategy = "multicore"),
#'           auto_retry = T,
#'           quiet = T)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom curl has_internet
#' @importFrom purrr map2
#' @importFrom purrr map
#' @importFrom glue glue
#' @import rlang
#' @import httr
#' @import dplyr
#'
#' @export

get_table <- function(table = NULL,
                      user = harvestR::get_harvest_account(),
                      key = harvestR::get_harvest_pat(),
                      email = '',
                      query = NULL,
                      ...,
                      verbose = lifecycle::deprecated(),
                      strategy = lifecycle::deprecated()){

  # Check internet connection -----------------------------------------------
  if(!curl::has_internet()) stop("Please check your internet connection.")

  # Setup Vars --------------------------------------------------------------
  # Default ... params
  input_params <- list(...)
  if(is.null(input_params$auto_retry)) input_params$auto_retry <- FALSE
  if (lifecycle::is_present(verbose)) {

    # Signal the deprecation to the user
    lifecycle::deprecate_soft(when = "1.1.0",
                              what = "harvestR::get_table(verbose = )",
                              with = "harvestR::get_table(quiet = )",
                              details = "A limitation of the future package is providing stdout across all parallel processes. httr currently ")
    # verbose goes to quiet which is a ... parameter
    input_params$quiet <- !verbose
  }
  if(is.null(input_params$quiet)){
    input_params$quiet <- TRUE
  }

  # Check Date Params Format ------------------------------------------------
  harvestR:::check_date_format(query = query)

  # Create URL --------------------------------------------------------------
  url <- paste0('v2/', table) %>% httr::modify_url(url="https://api.harvestapp.com",
                                                   path=.,
                                                   query=query)

  # Get Request -------------------------------------------------------------

  response <- harvestR:::get_request(url = url,
                                     user = user,
                                     key = key,
                                     email = email,
                                     auto_retry = input_params$auto_retry,
                                     quiet = input_params$quiet,
                                     table = table)

  # If quiet is off and more than 1 page, provide message.
  if(!input_params$quiet & response$total_pages > 1){
    message(glue::glue('Initial request shows {response$total_entries} records. Initiating the remaining {response$total_pages-1} requests.'))
  }

  # Get requests (multi-page) -----------------------------------------------

  if(response$total_pages > 1){
    # Pull the dataframe from the initial get request
    response_df <- response[[table]]
    # Build urls for the remaining requests
    urls <- purrr::map(2:response$total_pages, function(x) httr::modify_url(url, query = list(page = x)))
    # Build url groups by sets of 100 to avoid rate limiting by Harvest
    url_groups <- harvestR:::build_url_groups(urls = urls,
                                              quiet = input_params$quiet)
    # Future Config -----------------------------------------------------------
    if (lifecycle::is_present(strategy)) {

      # Signal the deprecation to the user
      lifecycle::deprecate_soft(when = "1.1.0",
                                what = "harvestR::get_table(strategy = )",
                                with = "harvestR::get_table(plan_options = )",
                                details = "\nThe strategy argument was passed to future::plan(), but provided no did not allow users to pass other arguments to future::plan(). The plan_options argument forwards all arguments to future::plan() for added flexibility.")
      # verbose goes to quiet which is a ... parameter
      input_params$plan_options <- list(strategy = strategy)
    }
    # Forward plan_options to the future::plan function to set up an execution plan, no need to default if one isn't provided because future::plan will default to sequential
    if(!is.null(input_params$plan_options)){
      rlang::call2('plan', !!!input_params$plan_options, .ns = "future") %>%
        rlang::eval_tidy()
    }
    # Get requests ------------------------------------------------------------
    # Pass url and the name of the url group,
    # last group is named 'Last_Group' which keeps get_requests from pausing if the last group finishes in less than 15 seconds
    responses_df <- purrr::map2(url_groups, names(url_groups), function(x, y) harvestR:::get_requests(urls = x,
                                                                                                      group_name = y,
                                                                                                      user = user,
                                                                                                      key = key,
                                                                                                      email = email,
                                                                                                      table = table,
                                                                                                      auto_retry = input_params$auto_retry,
                                                                                                      quiet = input_params$quiet)) %>%
      dplyr::bind_rows()
    # Reset execution plan to default
    future::plan("default")
    # Add responses to the initial response
    response_df <- dplyr::bind_rows(response_df, responses_df)
  } else {
    response_df <- response[[table]]
  }
  return(response_df)
}


#' Check date params for Harvest API v2 standards
#'
#' Harvest API v2 accepts a few date parameters which have strict requirements. Date parameters must
#' be in `yyyy-mm-dd` format. `yyyy/mm/dd`, `mm-dd-yyyy`, `yyyy-m-d`,  etc. will result in failure.
#' This function simply stops the user if they try an incorrect format.
#'
#' @inheritParams get_table
#'
#' @importFrom glue glue

check_date_format <- function(query = NULL) {
  if(is.null(query) == F){
    for(i in 1:length(query)){
      # Parms to & from  --------------------------------------------------------
      if(names(query[i]) %in% c('to','from')){
        if(grepl('^[1-9]{1}[0-9]{3}-[0-1]{1}[0-9]{1}-[0-3]{1}[0-9]{1}$', trimws(query[[i]][1]))==F){
          stop(
            glue::glue('{names(query[i])} = {query[i]} is not an accepted format for the parameter. Please refer to help file for more information.')
          )
        }
      }
    }
  }
}

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
#' @importFrom glue glue
#' @import dplyr

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
