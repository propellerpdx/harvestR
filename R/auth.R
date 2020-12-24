#' \lifecycle{experimental}
#' @title Get Harvest account ID from system environment variables
#' @description Retrieve the harvestR account ID from system environment variables under the default alias HARVEST_ACCOUNT_ID
#'
#' @importFrom assertthat assert_that
#' @export

get_harvest_account <- function() {
  account <- Sys.getenv('HARVEST_ACCOUNT_ID')
  assertthat::assert_that(account != "", msg = "HARVEST_ACCOUNT_ID is empty, please update and try again. For help updating see ?create_harvest_creds().")
  return(account)
}

#' \lifecycle{experimental}
#' @title Get Harvest personal access token from system environment variables
#' @description Retrieve the harvestR personal access token from system environment variables under the default alias HARVEST_PAT
#'
#' @importFrom assertthat assert_that
#' @export

get_harvest_pat <- function() {
  pat <- Sys.getenv('HARVEST_PAT')
  assertthat::assert_that(pat != "", msg = "HARVEST_PAT is empty, please update and try again. For help updating see ?create_harvest_creds().")
  assertthat::assert_that(grepl('^Bearer ', pat), msg = "HARVEST_PAT must start with `Bearer `, please update and try again. For help updating see ?create_harvest_creds().")
  return(pat)
}

#' \lifecycle{experimental}
#' @title Set the Harvest account ID & pat as system environment variables for the R session
#' @description Retrieve the Harvest account ID & personal access token from the keyring and set them as environment variables for the R session
#'
#' @param keyring keyring to retreive creds from, use the keyring set in create_harvest_creds (default value of NULL is valid)
#'
#' @seealso [keyring::key_get()] which this function wraps.
#'
#' @import keyring
#' @export

set_harvest_creds <- function(keyring = NULL){
  if(!rlang::is_interactive()){
    stop("set_harvest_creds() imports the keyring package which requires a user to unlock the keyring. For non-interactive R sessions set your credentials in the system environment. See package documentation for more details.")
  }
  if(!keyring::has_keyring_support() && !is.null(keyring)){
    stop("System only supports one keyring, leave keyring NULL (default). See keyring package for more details.")
  }
  service <- 'harvest_creds'
  keyring_entry <- keyring::key_list(service = service, keyring = keyring)
  keyring_entry <- keyring::key_list(service = service, keyring = NULL)
  if(nrow(keyring_entry) == 0){stop("No credentials were found on the keyring, please set keys using harvestR::create_harvest_creds().")}
  account <- keyring_entry$username
  token <- keyring::key_get(service = service, username = account)
  Sys.setenv("HARVEST_ACCOUNT_ID" = account)
  Sys.setenv("HARVEST_PAT" = token)
}

#' \lifecycle{experimental}
#' @title Create or reset the Harvest account ID & pat on the keyring to use as Harvest credentials in the future
#' @description  Creates a keyring record for the Harvest account ID & personal access token to use with set_harvest_creds() in future R sessions. New entries replace old credentials.
#'
#' @param keyring keyring to retreive creds from, use the keyring set in create_harvest_creds (default value of NULL is valid)
#'
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
#' @import keyring
#' @export
create_harvest_creds <- function(keyring = NULL){
  service <- "harvest_creds"
  account <- ask_harvest_account()
  pat <- ask_harvest_pat()
  delete_harvest_creds()
  keyring::key_set_with_value(service = "harvest_creds", username = account, password = pat, keyring = keyring)
  message("Credentials were added to the keyring.")
}

#' \lifecycle{experimental}
#' @title Delete the Harvest account ID & pat on the keyring
#' @description  Delete the keyring record for the Harvest account ID & personal access token
#'
#' @param keyring keyring to retreive creds from, use the keyring set in create_harvest_creds (default value of NULL is valid)
#'
#' @import keyring
#' @export
delete_harvest_creds <- function(keyring = NULL){
  service <- "harvest_creds"
  try(keyring::key_delete("harvest_creds"),silent=T)
}

#' @title Ask user for Harvest account ID
#' @param msg message in the Rstudio prompt
#'
#' @importFrom assertthat assert_that
#' @importFrom getPass getPass
ask_harvest_account <- function (msg="Harvest API Account ID"){
  account <- getPass::getPass(msg)
  assertthat::assert_that(!is.null(account),
                          trimws(account) != "",
                          msg = "Empty or NULL account IDs are not accepted, please use the account ID provided at https://id.getharvest.com/developers")
  return(account)
}

#' @title Ask user for Harvest pat
#' @param msg message in the Rstudio prompt
#'
#' @importFrom assertthat assert_that
#' @importFrom getPass getPass
ask_harvest_pat <- function (msg="Harvest API Bearer Token"){
  pat <- getPass::getPass(msg)
  assertthat::assert_that(!is.null(pat), msg = "NULL tokens are not accepted, please try again. For more info see ?create_harvest_creds().")
  assertthat::assert_that(pat != "", msg = "Blank tokens are not accepted, please try again. For more info see ?create_harvest_creds().")
  assertthat::assert_that(grepl('^Bearer ', pat), msg = "Tokens must start with `Bearer `, please try again. For more info see ?create_harvest_creds().")
  return(pat)
}
