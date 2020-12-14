#' Get Response Messages
#'
#' Sends messages to console regarding the response. This is implemented because,
#' depending on the selected strategy for the future::plan, messages from httr::GET may not be
#' surfaced to the console due to multiple R sessions running and stderror relaying issues.
#'
#' @param response harvest API response
#' @param content content from harvest API response
#'
#'
get_response_messages <- function(response,
                                  content){
  message(glue::glue('response_URL: {response$url}'))
  message(glue::glue('x-request-id: {response$headers$`x-request-id`}'))
  message(glue::glue('date: {response$date}'))
  message(glue::glue('x-runtime: {response$headers$`x-runtime`}'))
  message(glue::glue('page: {content$page}'))

  data_frame <- names(Filter(isTRUE, purrr::map(content, is.data.frame)))

  message(paste0('first_row: ', paste(content[[data_frame]][1,], collapse = " | ")))
  message(paste0('last_row: ', paste(content[[data_frame]][nrow(content[[data_frame]]),], collapse = " | ")))
}
