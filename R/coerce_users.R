#' @title Harvest User Object with Data Type Conversion
#'
#' @description Coerces the Harvest user object to R-native data types
#'
#' @details Coerces the data types of the elements of the user object from
#'   Harvest. Coercions are based on the Harvest API docs
#'   \href{https://help.getharvest.com/api-v2/users-api/users/users/}
#'    as follows (Harvest Data Type > R Data type): integer > integer, date >
#'   Date, object > list, decimal > numeric, string > character, boolean >
#'   logical, datetime > datetime, time > ---
#'
#' @param users a list; *required*; a list of users with a full or partial list
#'   of the user attributes; defaults to NULL
#'
#' @return a list;
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#'
#' @importFrom magrittr %>%
#'
coerce_users <- function(users = NULL) {
  # Nothing we can do without a list of users
  if (is.null(users)) {
    stop("A users parameter is required!!")
  }
  print("in coerce_users")
  print(paste0("users: ", users))
  coerced_users <-
    users %>%
    dplyr::mutate_at(if('first_name' %in% names(.)) 'first_name' else integer(0), as.character) %>%
    dplyr::mutate_at(if('last_name' %in% names(.)) 'last_name' else integer(0), as.character) %>%
    dplyr::mutate_at(if('name' %in% names(.)) 'name' else integer(0), as.character)

  return(coerced_users)

}
