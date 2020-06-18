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
#' @import dplyr
#'

build_url_groups <- function(urls = NULL,
                             verbose = verbose){
  request_groups <- NULL
  for(i in 1:ceiling(length(urls) / 100)){
    start_url <- (i * 100) - 99
    end_url <- min((((i + 1) * 100) - 100), length(urls))
    group_name <- dplyr::if_else(i*100 > length(urls), 'Last_Group', paste0('Group_',i))
    request_groups[[group_name]] <- urls[start_url:end_url]
  }
  return(request_groups)
}

