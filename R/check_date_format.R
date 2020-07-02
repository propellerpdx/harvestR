#' Check date params for Harvest API v2 standards
#'
#' Harvest API v2 accepts a few date parameters which have strict requirements. Date parameters must
#' be in `yyyy-mm-dd` format. `yyyy/mm/dd`, `mm-dd-yyyy`, `yyyy-m-d`,  etc. will result in failure.
#' This function simply stops the user if they try an incorrect format.
#'
#' @inheritParams get_table
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}

#TODO Modifying this function to actually coerce dates into the proper format is a bit tedious and not necessarily worth the time,
# but it could be a great project for a beginner coder.
check_date_format <- function(query = NULL) {
  if(is.null(query) == F){
    for(i in 1:length(query)){
      # Parms to & from  --------------------------------------------------------

      if(names(query[i]) %in% c('to','from')){

        # ^ is a front anchor, so it must start with [1-9]
        # {1} tests the one length, {3} tests three length
        # - is just that exact text in that position
        # $ is an end anchor, so the query value must be the exact length tested and end in a number [0-9]
        if(grepl('^[1-9]{1}[0-9]{3}-[0-1]{1}[0-9]{1}-[0-3]{1}[0-9]{1}$', trimws(query[[i]][1]))==F){
          stop(
            glue::glue('{names(query[i])} = {query[i]} is not an accepted format for the parameter. Please refer to help file for more information.')
          )
        }

      # Coercsion Code ----------------------------------------------------------
      #if(is.character(query[[i]][1])){
      #  if(nchar(query[i])<11){
      #    query[i] <- as.character(lubridate::as_date(query[[i]][1]))
      #  }else if(nchar(query[i])>11){
      #    stop(
      #      glue::glue('{names(query[i])} = {query[i]} is not an accepted format for the parameter. Please refer to help file for more information.')
      #    )
      #  }
      #}
      }
    }
  }
}
