#' \lifecycle{maturing}
#'
#' @title Harvest API v2 get request wrapper
#' @description Submits get requests to the Harvest API v2 with options configured for authentication, parallelism, and logging
#' @param table Character table name to be submitted to the Harvest API v2. Refer to \href{https://help.getharvest.com/api-v2}{Harvest API v2}.
#' @param user Character account number (e.g. user = '12345') for authentication. Register at \href{https://id.getharvest.com/developers}{Harvest Developers}. Defaults to `get_harvest_account()`
#' @param key Character key (e.g. key = 'Bearer <secret key>') for API authentication. Register at \href{https://id.getharvest.com/developers}{Harvest Developers}. Defaults to `get_harvest_pat()`
#' @param plan_options List; list of parameters passed to \code{future::plan}. The strategy parameter is required to implement parallel requests, see \href{https://cran.r-project.org/web/packages/future/vignettes/future-1-overview.html}{Future Package Overview} for more details.
#' @param quiet Logical; controls harvestR messaging, TRUE returns only errors in the console and FALSE returns messages, warnings, and errors. Does not impact deprecation warnings.
#' @param ... Additional arguments forwarded to `httr::RETRY()` and `httr::config()`. Refer to details for more information.
#' @inheritParams get_request
#' @inheritParams get_requests
#'
#' @details
#' Description of ... parameters. Additional arguments are forwarded to config.
#' \itemize{
#'    \item \code{times} integer; Forwarded to RETRY, maximum number of requests to attempt. Defaults to 3.
#'    \item \code{token} httr-token; Forwarded to config, an OAuth token (1.0 or 2.0).
#'    }
#'
#' @return dataframe with contents of requested Harvest table
#'
#' @seealso `future::plan()` for more detail regarding how to implement parallel api calls
#' @seealso `httr::httr_options()` for more detail regarding httr configurations
#'
#' @references
#' \href{https://help.getharvest.com/api-v2}{Harvest API v2}
#' \href{https://id.getharvest.com/developers}{Harvest Developers Page},
#' \href{https://cran.r-project.org/web/packages/future/vignettes/future-1-overview.html}{Future Pacakge Overview},
#' \href{https://cran.r-project.org/web/packages/future/vignettes/future-2-output.html}{Future Package Output}
#'
#' @examples
#' \dontrun{
#'
#' # Create a credentials record on the system keyring
#' create_harvest_creds()
#'
#' # Set the credentials in the environment for get_table
#' set_harvest_creds()
#'
#' # Barebones call to get_table
#' projects <- get_table(table = 'projects')
#'
#' # Parallel request with 2 retries and messaging
#' # Windows users should use multisession, multicore will not work
#' time_entries <- get_table(table = 'time_entries',
#' query = list(from = '2018-01-01', to = '2018-12-31'),
#' plan_options = list(strategy = "multicore"),
#' times = 2,
#' quiet = F)
#'
#' # Request using an httr option (see `httr::httr_options()`) and an oauth token
#' projects <- get_table(table = 'projects',
#' http_version = 2,
#' token = my_token)
#'
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom curl has_internet
#' @importFrom purrr map2
#' @importFrom purrr map
#' @importFrom glue glue
#' @importFrom dplyr bind_rows
#' @import rlang
#' @import httr
#'
#' @export

get_table <- function(table = NULL,
                      user = harvestR::get_harvest_account(),
                      key = harvestR::get_harvest_pat(),
                      query = NULL,
                      plan_options = NULL,
                      quiet = T,
                      ...,
                      auto_retry = lifecycle::deprecated(),
                      email = lifecycle::deprecated(),
                      verbose = lifecycle::deprecated(),
                      strategy = lifecycle::deprecated()){

  if(!curl::has_internet()) stop("Please check your internet connection.")
  input_params <- list(...)
  # Match ... params with httr_options to pass to httr::config
  httr_config_opts <- input_params[names(input_params) %in% httr::httr_options()$httr]
  # Pull oath token if one is provided to pass to httr::config
  token <- switch(is.null(input_params$token), NULL, input_params$token)

  # Deprecated param warnings -----------------------------------------------
  # If no auto_retry set, defaults to 0, if T defaults to 3, if FALSE 0, if nothing it passes
  if (lifecycle::is_present(auto_retry)) {
    # Signal the deprecation to the user
    lifecycle::deprecate_soft(when = "1.1.0",
                              what = "harvestR::get_table(auto_retry = )",
                              with = "harvestR::get_table(times = )",
                              details = "Please see https://propellerpdx.github.io/harvestR/news/index.html for more details.")
    # verbose goes to quiet which is a ... parameter
    input_params$times <- dplyr::if_else(auto_retry == T, 3, 0)
  }
  if(is.null(input_params$times)){ input_params$times <- 3 }
  if (lifecycle::is_present(verbose)) {
    lifecycle::deprecate_soft(when = "1.1.0",
                              what = "harvestR::get_table(verbose = )",
                              details = "harvestR no longer sets Please see https://propellerpdx.github.io/harvestR/news/index.html for more details.")
    quiet <- !verbose
  }
  if (lifecycle::is_present(email)) {
    lifecycle::deprecate_soft(when = "1.1.0",
                              what = "harvestR::get_table(email = )",
                              details = "Please see https://propellerpdx.github.io/harvestR/news/index.html for more details.")
  }
  if (lifecycle::is_present(strategy)) {
    lifecycle::deprecate_soft(when = "1.1.0",
                              what = "harvestR::get_table(strategy = )",
                              with = "harvestR::get_table(plan_options = )",
                              details = "\nThe strategy argument was passed to future::plan(), but provided no did not allow users to pass other arguments to future::plan(). The plan_options argument forwards all arguments to future::plan() for added flexibility.")
    plan_options <- list(strategy = strategy)
  }

  # Create URL --------------------------------------------------------------
  harvestR:::check_date_format(query = query)
  url <- paste0('v2/', table) %>% httr::modify_url(url="https://api.harvestapp.com",
                                                   path=.,
                                                   query=query)

  # Get Request -------------------------------------------------------------
  response <- harvestR:::get_request(url = url,
                                     user = user,
                                     key = key,
                                     email = email,
                                     times = input_params$times,
                                     httr_config_opts = httr_config_opts,
                                     quiet = quiet,
                                     table = table,
                                     token = token)
  # Get requests (multi-page) -----------------------------------------------
  if(response$total_pages > 1){
    if(!quiet){
      message(glue::glue('Initial request shows {response$total_entries} records. Initiating the remaining {response$total_pages-1} requests.'))
    }
    urls <- purrr::map(2:response$total_pages, function(x) httr::modify_url(url, query = list(page = x)))
    url_groups <- harvestR:::build_url_groups(urls = urls,
                                              quiet = quiet)
    # Future Config -----------------------------------------------------------
    # Forward plan_options to the future::plan function to set up an execution plan, no need to default if one isn't provided because future::plan will default to sequential
    furrr_opts <- furrr::furrr_options()
    if(!is.null(plan_options)){
      rlang::call2('plan', !!!plan_options, .ns = "future") %>%
        rlang::eval_tidy()
      if(plan_options$strategy != "sequential"){
        furrr_opts <- furrr::furrr_options(seed = T)
      }
    }

    # Get requests ------------------------------------------------------------
    responses <- purrr::map(url_groups, function(x) harvestR:::get_requests_lim(urls = x,
                                                                                user = user,
                                                                                key = key,
                                                                                table = table,
                                                                                quiet = quiet,
                                                                                httr_config_opts = httr_config_opts,
                                                                                times = input_params$times,
                                                                                token = token,
                                                                                furrr_opts = furrr_opts))
    responses_df <- purrr::map(responses, function(x){ purrr::map_dfr(x, function(y) y[table]) }) %>%
      purrr::map(table) %>%
      dplyr::bind_rows()
    # Reset execution plan to default
    future::plan("default")
    # Pull the dataframe from the initial get request
    response_df <- response[[table]]
    # Add responses to the initial response
    df <- dplyr::bind_rows(response_df, responses_df)
  } else {
    df <- response[[table]]
  }
  return(df)
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
#' @importFrom dplyr if_else

build_url_groups <- function(urls = NULL,
                             ...){

  input_params <- list(...)
  if(is.null(input_params$quiet)) input_params$quiet <- TRUE

  request_groups <- NULL
  for(i in 1:ceiling(length(urls) / 99)){
    # Calculates the position of the first url in the group
    start_url <- (i * 99) - 98
    # Calculates the position of the last url in the group
    end_url <- min(i * 99 , length(urls))
    # Last group needs a specific name to avoid rate limit timeout in get_requests()
    group_name <- dplyr::if_else(i*99 > length(urls), 'Last_Group', paste0('Group_',i))
    # Adds vector of urls into list
    request_groups[[group_name]] <- urls[start_url:end_url]
    if(!input_params$quiet) message(glue::glue('Group {group_name} starts with page {start_url + 1} and ends with page {end_url + 1}'))
  }
  return(request_groups)
}
