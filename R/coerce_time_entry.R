#' @title R-native time_entry object
#'
#' @description Coerces the Harvest time_entry object to R-native data types
#'
#' @details Coerces the data types of the elements of the time_entry object from
#'   Harvest. Coercions are based on the Harvest API docs
#'   \href{https://help.getharvest.com/api-v2/timesheets-api/timesheets/time-entries/}
#'    as follows (Harvest Data Type > R Data type): integer > integer, date >
#'   ___, object > list, decimal > numeric, string > character, boolean >
#'   logical, datetime > datetime, time > ---
#'
#'   This function only does coercion for the time_entry object - other objects
#'   in the time_entries payload are coerced by their respective functions.
#'
#'   Functions similar to this may eventually be available for other API calls.
#'
#' @param time_entry a list; *required*; a list (can be partial) of the
#'   time_entry attributes; defaults to NULL
#'
#' @return a list;
#'
#' @examples
#' # See get_time_entries.R
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#'
#' @importFrom magrittr %>%
#'
coerce_time_entries <- function(time_entries = NULL) {
  # Nothing we can do without a time_entry
  if (is.null(time_entries)) {
    stop("A time_entry parameter is required!!")
  }

  coerced_time_entries <-
    time_entries %>%
    dplyr::mutate_at(if('id' %in% names(.)) 'id' else integer(0), as.integer) %>%
    dplyr::mutate_at(if('spent_date' %in% names(.)) 'spent_date' else integer(0), as.Date) %>%
    dplyr::mutate_at(if('user' %in% names(.)) 'user' else integer(0), coerce_users)

  return(coerced_time_entries)
}
