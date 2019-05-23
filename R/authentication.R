#' Retrieve Harvest Personal Access Token
#'
#' Looks in env var HARVEST_PAT
#'
#' @return A string of the Harvest Personal Access Token
#' @export
harvest_pat <- function() {
  var <- Sys.getenv('HARVEST_PAT')
  if (identical(var, "")) {
    warning("Please set env var HARVEST_PAT to your Harvest personal access token",
         call. = FALSE)
    return(NULL)
  }
  var
}

#' Retrieve Harvest Account ID
#'
#' Looks in env var HARVEST_ACCOUNT_ID
#'
#' @return A string of the Harvest Account ID
#' @export
harvest_id <- function() {
  var <- Sys.getenv('HARVEST_ACCOUNT_ID')
  if (identical(var, "")) {
    warning("Please set env var HARVEST_ACCOUNT_ID to your Harvest account ID",
         call. = FALSE)
    return(var)
  }
  var
}
