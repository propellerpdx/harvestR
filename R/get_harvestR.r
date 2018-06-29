#' Harvest API v2 get request wrapper
#'
#' Submits get requests to the Harvest API v2. The function is a wrapper around the httr::GET function and returns Harvest table(s) with their respective names from the API documentation.
#'
#' @param table Harvest API table or tables to call. Some tables have not been included, but may be added in the future. Refer to \href{https://help.getharvest.com/api-v2}{Harvest APIv2}. Possible values include any combination of the following strings in a vector:  (\code{"clients"} or \code{"invoices"} or \code{"estimates"} or \code{"expenses"} or \code{"tasks"} or \code{"time_entries"} or \code{"projects"} or \code{"roles"} or \code{"users"} or \code{"user_project_assignments"} or \code{"project_task_assignments"} or \code{"project_user_assignments"} or \code{"all"})
#' @param user Harvest account number for authentication. Register at \href{https://id.getharvest.com/developers}{Harvest Developers}.
#' @param key Harvest API key for API authentication. Register at \href{https://id.getharvest.com/developers}{Harvest Developers}.
#' @param email Optional argument, requester email address for the "From" header in the get request.
#' @param query Optional argument, API query parameters to be provided in a list. Refer to \href{https://help.getharvest.com/api-v2}{Harvest APIv2} for acceptable parameters for each table. A few examples include:  (\code{from = "2018-1-1"} or \code{to = "2018-3-31"} or \code{project_id = "1234"} or \code{client_id = "1234"} or \code{user_id = "1234"})
#'
#' @return A data frame for each Harvest table with the name of the Harvest table e.g. 'clients'.
#'
#' @examples
#'
#' user_id <- 'your_user_id'
#' api_key <- paste0('Bearer ','you_api_key')
#' get_harvestR(table = c('clients','users','projects') , user = user_id,key = api_key)
#' get_harvestR(table = c('time_entries') , user = user_id, key = api_key, email = 'your_email',query = list(from = '2018-1-1', to = '2018-3-31'))
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#' @references \url{https://id.getharvest.com/developers}, \url{https://help.getharvest.com/api-v2}, \url{https://github.com/r-lib/httr}
#'
#' @import httr
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @importFrom purrr map_dbl
#' @importFrom purrr map2_chr
#' @importFrom jsonlite fromJSON
#'
#' @export
get_harvestR <- function(
  table = NULL,
  user = NULL,
  key = NULL,
  email = '',
  query=NULL)
  {
  #if(exists('query')==F){query<-NULL}
  if(table %in% c('clients','invoices','estimates','expenses','tasks','time_entries','projects','roles','users')){
    table_url <- paste0('v2/',table)
    total_pages <- httr::modify_url(url="https://api.harvestapp.com",path=table_url) %>%
      purrr::map(~httr::GET(.,httr::add_headers("Harvest-Account-ID" = user,Authorization = key,'User-Agent=Propeller R API Helper (mdruffel@propellerpdx.com)', 'From' = email), query = query)) %>%
      purrr::map(~httr::content(., as="text", encoding = "UTF-8")) %>%
      purrr::map(~jsonlite::fromJSON(., flatten = T)) %>%
      purrr::map_dbl("total_pages")
    if(total_pages<100){
      assign(paste0(table),envir = .GlobalEnv,
             total_pages %>%
               seq(from=1,to=.) %>%
               #Make an API call for every page
               purrr::map_chr(~httr::modify_url(url="https://api.harvestapp.com",path=paste0(table_url,"?page=",.))) %>%
               purrr::map(~httr::GET(.,httr::add_headers("Harvest-Account-ID" = user,Authorization = key,'User-Agent=Propeller R API Helper (mdruffel@propellerpdx.com)', 'From' = email),query = query)) %>%
               purrr::map(~httr::content(., as="text", encoding = "UTF-8")) %>%
               purrr::map(~jsonlite::fromJSON(., flatten = T)) %>%
               purrr::map(paste0(table)) %>%
               #Combine the results into one data frame
               do.call("bind_rows", .))
    }
    else{
      i <- 1
      x <- 99
      frames <- NULL
      while(i<total_pages){
        print(paste0("Requesting pages ",i," to ",x))
        begin_time <- Sys.time()
        assign(paste0(table,i),
               seq(from=i,to=x) %>%
                 #Make an API call for every page
                 purrr::map_chr(~httr::modify_url(url="https://api.harvestapp.com",path=paste0(table_url,"?page=",.))) %>%
                 purrr::map(~httr::GET(.,httr::add_headers("Harvest-Account-ID" = user,Authorization = key,'User-Agent=Propeller R API Helper (mdruffel@propellerpdx.com)', 'From' = email),query = query)) %>%
                 purrr::map(~httr::content(., as="text", encoding = "UTF-8")) %>%
                 purrr::map(~jsonlite::fromJSON(., flatten = T)) %>%
                 #Combine the results into one data frame
                 purrr::map(paste0(table)) %>%
                 #Combine the results into one data frame
                 do.call("bind_rows", .))
        i <- i+99
        x <- if(x+99>total_pages){total_pages}else{x+99}
        naptime::naptime(15-lubridate::seconds(Sys.time()-begin_time))
      }
      frames <- lapply(ls(pattern=paste0(table)), function(x) get(x))
      assign(table,envir = .GlobalEnv,purrr::map_df(frames,data.frame))
    }
  }
  else if(table %in% c('project_task_assignments','project_user_assignments'))
  {
    get_harvestR(table='projects',user=user,key=key,email=email)
    project_ids <<- projects$id
    api_name <<- stringr::str_sub(table,start=9,end = nchar(table))
    assign(paste0(table),envir = .GlobalEnv,
           # Find the number of pages
           purrr::map2_chr(.x = project_ids, .y=api_name, function(.x,.y) paste0('https://api.harvestapp.com/v2/projects/',.x,'/',.y)) %>%
             purrr::map(~httr::GET(.,httr::add_headers("Harvest-Account-ID" = user,Authorization = key,'User-Agent=Propeller R API Helper (mdruffel@propellerpdx.com)', 'From' = email),query = query)) %>%
             purrr::map(~httr::content(., as="text", encoding = "UTF-8")) %>%
             purrr::map(~jsonlite::fromJSON(., flatten = T)) %>%
             purrr::map(paste0(api_name)) %>%
             do.call("bind_rows", .))
  }
  else if(table %in% c('user_project_assignments'))
  {
    get_harvestR(table='users',user=user,key=key,email=email)
    user_ids <- users$id
    api_name <- stringr::str_sub(table,start=6,end = nchar(table))
    assign(paste0(table),envir = .GlobalEnv,
           # Find the number of pages
           purrr::map2_chr(.x = user_ids, .y=api_name, function(.x,.y) paste0('https://api.harvestapp.com/v2/users/',.x,'/',.y)) %>%
             purrr::map(~httr::GET(.,httr::add_headers("Harvest-Account-ID" = user,Authorization = key,'User-Agent=Propeller R API Helper (mdruffel@propellerpdx.com)', 'From' = email),query = query)) %>%
             purrr::map(~httr::content(., as="text", encoding = "UTF-8")) %>%
             purrr::map(~jsonlite::fromJSON(., flatten = T)) %>%
             purrr::map(paste0(api_name)) %>%
             do.call("bind_rows", .))
  }
  else if(table=='all'){
    purrr::map(c('clients','invoices','tasks','time_entries','projects','roles','users','user_project_assignments','project_task_assignments','project_user_assignments'),get_harvestR,user=user,key=key, email = email,query = query) #'estimates','expenses'
  }
  else
  {
    print('Please refer to the documentation and choose an available table.')
  }
}
