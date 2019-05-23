#' @title Retrieve Harvest Personal Access Token
#'
#' @description A convenience function to allow you to store your Harvest
#'   API key in a system env variable
#'
#' @details The HARVEST_PAT env variable should contain your bearer token,
#'   formatted as "Bearer [token goes here]". You can get an API key by
#'   registering at \href{https://id.getharvest.com/developers}{Harvest
#'   Developers}.
#'
#'   NOTE: The user is advised to do this as her/his own risk and this method
#'   may not be wise for production systems. System env variables are *not* a
#'   secure storage mechanism. They are in plain text and a hacker could
#'   theoretically gain access to your API key once they have access to your
#'   machine. The recommended approach is to store your API key in a secure
#'   location, like your keychain, and pull it from there to pass to the
#'   harvestR package.
#'
#' @return A string of the Harvest Personal Access Token
#'
#' @export
harvest_pat <- function() {
  var <- Sys.getenv('HARVEST_PAT')
  if (identical(var, "")) {
    stop("Please pass an API key or set the env var HARVEST_PAT",
         call. = FALSE)
    return(NULL)
  }
  var
}

#' @title Retrieve Harvest Account ID
#'
#' @description A convenience function to allow you to store your Harvest
#'   account ID in a system env variable
#'
#' @details The HARVEST_ACCOUNT_ID env variable should contain your Harvest
#'   account ID. Register at \href{https://id.getharvest.com/developers}{Harvest
#'   Developers}.
#'
#'   NOTE: The user is advised to do this as her/his own risk and this method
#'   may not be wise for production systems. System env variables are *not* a
#'   secure storage mechanism. They are in plain text and a hacker could
#'   theoretically gain access to your API key once they have access to your
#'   machine. The recommended approach is to store your API key in a secure
#'   location, like your keychain, and pull it from there to pass to the
#'   harvestR package.
#'
#' @return A string of the Harvest Account ID
#'
#' @export
harvest_id <- function() {
  var <- Sys.getenv('HARVEST_ACCOUNT_ID')
  if (identical(var, "")) {
    stop("Please pass a Harvest account ID or set the env var HARVEST_ACCOUNT_ID",
         call. = FALSE)
    return(var)
  }
  var
}
