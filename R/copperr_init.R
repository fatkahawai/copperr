##############################################################################
# copperr - A Copper API wrapper package
#
# references [\url{https://developer.copper.com/#intro}]
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

# GLOBAL VARS ---------------------------------------------------------------
#
.pkgglobalenv <- new.env(parent=emptyenv())

# ----------------------------------------------------------------------------
#' printIfVerbose
#'
#' prints arguments if the verbose flag has been set to TRUE
#' a private function within this package
#'
#' @param x a value you are asserting is something
#' 
#' printIfVerbose("the value of x is",x)
#' @keywords internal
# ----------------------------------------------------------------------------
printIfVerbose <- function(x, ...){
  if(.pkgglobalenv$verboseFlag)
    print(paste(x, ...))
}

# ----------------------------------------------------------------------------
#' pkg_is_initialized
#'
#' a test for use with the stopifnot() assertion 
#' a private function within this package
#'
#' @param x a value you are asserting is something
#' 
#' @return TRUE if environment is initialized. 
#' @keywords internal
# ----------------------------------------------------------------------------
pkg_is_initialized <- function(){
  !is.null(.pkgglobalenv$cppr_Endpoint)
}
# calling stopifnot(pkg_is_initialized())
# prints "Error: pkg_is_initialized() is not TRUE"
# so modify the error msg to be more informative
#on_failure(pkg_is_initialized) <- function(call, env) {
#  return("package copperr has not been initialized. Call copperr::initializePw() first")
#}
# now stopifnot(pkg_is_initialized())
# will print "Error: package copperr has not been initialized. Call initializePw() first"
# ----------------------------------------------------------------------------
#' cppr_initialize
#' 
#' initializes the package by setting the fields of the private global environment.
#' 
#' @note You must call this function first before calling any other package function
#' @param uid string - the email address used for your PW account 
#' @param api_key string - your API key from copper Settings>Preferences>API Keys
#' @param verbose boolean - set to TRUE if you are not getting the expected output 
#' and want to get verbose status messages printed to the console
#' @return TRUE if successful
#' @examples
#' \dontrun{ 
#' cppr_initialize("foo@@bar.com","1234567890ABCDEF")
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_initialize <- function(uid,api_key,verbose=FALSE) {

  stopifnot(is.character(uid))
  stopifnot(is.character(api_key))

  # these fields are set using the paramaters passed to this function
  assign("cppr_AccessToken", api_key, envir=.pkgglobalenv)
  assign("cppr_UserEmail", uid, envir=.pkgglobalenv)
  assign("verboseFlag", verbose, envir=.pkgglobalenv)

  # these fields are constants
  assign("cppr_Ua", "http://github.com/fatkahawai/copperr", envir=.pkgglobalenv)
  assign("cppr_Url", "https://api.prosperworks.com", envir=.pkgglobalenv)
  assign("cppr_Endpoint", "/developer_api/v1", envir=.pkgglobalenv)

  # These fields will be loaded upon the first call to a corresponding function
  # the results are stored here for performance reasons - to avoid unnecessary repeat calls
  assign("cppr_CustomFieldDefs", NULL, envir=.pkgglobalenv)
  assign("cppr_Users", NULL, envir=.pkgglobalenv)

  printIfVerbose("cppr_initialize: copperr package environment successfully initialized")
#  printIfVerbose("initializePw: stored in package env:",.pkgglobalenv$cppr_AccessToken,"/",.pkgglobalenv$cppr_UserEmail)

  return(TRUE)
}

