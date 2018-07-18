##############################################################################
# copperr - A Copper API wrapper package
#
# references [\url{https://developer.prosperworks.com/#intro}]
#
# NB: 
# 1. All requests must be sent using HTTPS with TLS 1.2 or higher
# 2. All API calls are limited to 600 requests every 10 minutes. Once this limit 
# has been reached, calls will return and error with status code 429. This rate 
# limit is evaluated on a rolling 10 minute window.
#
# 3. Our /search endpoints return multiple records per response. 
# How many records are included in a single response (=page size) is determined by 
# an optional search parameter called "page_size". The default value for "page_size" is 20, 
# and its value can be set to any numeric between 1 and 200. 
# When the search criteria match more records than what fits on a single page then 
# ou have to paginate the search results using one of the following strategies in 
# order to get all the records that match your search.
#
# 4. HTTP Return Code  Meaning
#
# Successful Responses:  
# 200 OK
#
# Error Responses:
# 400 Bad Request
# 401 Unauthorized
# 422 Unable to process request - unrecognized argument 
# 429 Too many requests (max allowed 600 in a 10 minute period)
# 500 Internal Server Error
#
# author Robert Drummond
# (C) 2017,2018 http://www.robert-drummond.com
##############################################################################

# Libraries   ---------------------------------------------------------------
#library(httr)          # for HTTP requests
#library(jsonlite)      # for json encoding of HTTP payloads

# CONSTANTS ------------------------------------------------------------------
#

# ----------------------------------------------------------------------------
#' cppr_getUsers
#
#' returns a table of all Copper users
#'
#' @return a data frame of users, or NULL on error
#' Field Type    Details
#' id    number  Unique identifier 
#' name  string  The user name 
#' email string  email address 
#' @importFrom jsonlite fromJSON 
#' @importFrom httr POST content authenticate add_headers http_type user_agent
#' @family user functions
#' @examples
#' \dontrun{ 
#' users <- cppr_getUsers()
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getUsers <- function() {
    stopifnot(pkg_is_initialized())    

    api <- "/users/search"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api)
    printIfVerbose("cppr_getUsers:",path)

    users <- NULL

    raw.result <- httr::POST(url = .pkgglobalenv$cppr_Url, 
                      httr::user_agent(.pkgglobalenv$cppr_Ua),
                      path = path, 
                      httr::add_headers(
                        `X-PW-AccessToken` = .pkgglobalenv$cppr_AccessToken,
                        `X-PW-Application` = "developer_api",
                        `X-PW-UserEmail` = .pkgglobalenv$cppr_UserEmail,
                        `Content-Type` = "application/json"),
                      httr::authenticate(.pkgglobalenv$cppr_AccessToken,"",type="basic"))
    if (httr::http_type(raw.result) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    raw.content <- rawToChar(raw.result$content)
    result <- jsonlite::fromJSON(raw.content, flatten=TRUE)

    if(raw.result$status_code == 200){
        if( length(result)==0 ){
          printIfVerbose("cppr_getUsers: no User records found in Copper")
          users <- as.data.frame(NULL) # return an empty data frame
        } else {
          users <- result
          printIfVerbose("cppr_getUsers result: ",users)
          assign("pwUsers", users, envir=.pkgglobalenv) # store in local persistent env
        } # else result >0
    } else{
        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getUsers Failed. status code 400: Bad Request")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("getUsers Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getUsers Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getUsers(): API request failed [%s]\n%s", 
              result$status, result$message), call. = FALSE)
    } # else result != 200 
    return(users)
}

# ----------------------------------------------------------------------------
#' cppr_getUserId 
#'
#' get the User Id for a named User
#'
#' @param name the name of a Copper User : e.g. "Bob"
#' 
#' @return a Copper ID, or NULL if not found
#' @family user functions
#' @examples
#' \dontrun{ 
#' id <- cppr_getUserId("Bob")
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getUserId <- function(name) {
  stopifnot(pkg_is_initialized())    
  stopifnot(is.character(name))

  id <- NULL # initialize return variable

  # if the user table has not yet been loaded into the package global environment,
  # load it now
  if(is.null(.pkgglobalenv$cppr_Users)){
    users <- cppr_getUsers() # get the table of Copper Users
    if(!is.null(users)){
      assign("cppr_Users", users, envir=.pkgglobalenv)
    }
  }
  # if we have at least one user in the table, search for the name passed as arg
  if(!is.null(.pkgglobalenv$cppr_Users)){
    if(nrow(.pkgglobalenv$cppr_Users) >0){
      lookup <-  .pkgglobalenv$cppr_Users$id[.pkgglobalenv$cppr_Users$name==name]
      #if a match found, return it
      if(length(lookup)==1){
        id <- lookup
      }
    }
  }
  return(id)
}

