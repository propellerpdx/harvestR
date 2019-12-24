#' @title Get normalized Harvest time_entries
#'
#' @description Requests time_entries from Harvest and then normalizes them
#'
#' @details This function requests time_entries from get_harvestR(), then
#'   normalizes them. Right now, normalization only includes data type coercion
#'   to native R data types and structures.
#'
#' @param ... additional parameters passed to get_harvestR()
#'
#' @return a list; json object converted to R list
#'
#' @examples
#'\dontrun{
#' get_time_entries(user = user_id,
#'                  key = api_key,
#'                  email = 'your_email',
#'                  query = list(
#'                    from = '2018-01-01',
#'                    to = '2018-03-31')
#'                  )
#'}
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#' @references \url{https://help.getharvest.com/api-v2},
#'
#' @importFrom magrittr %>%
#'
#' @export
get_time_entries <- function(...) {
  time_entries <- get_table(table = 'time_entries', ...)

  coerced_time_entries <-
    # purrr::map(time_entries,
    #            function(time_entry)
                 coerce_time_entries(time_entries)
               # )

  return(coerced_time_entries)
}
