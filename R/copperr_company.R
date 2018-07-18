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
#' cppr_getCompanyById
#'
#' get the Company record identified by a Copper Id 
#'
#' @param id numeric - the Id of the Company to return
#' @return a List of fields with various types, or Null if not found
#' Field               Type  Details
#' id                  number  Unique identifier for the Opportunity.
#' name*               string  The name of the Opportunity.
#' assignee_id         number  Unique identifier of the User that will be the owner of the Opportunity.
#' details             string  Description of the Opportunity.
#' primary_contact_id  number  The unique identifier of the Person who is the primary contact for this Opportunity.
#' tags                list  An array of the tags associated with the Opportunity, represented as strings.
#' date_created        number  A Unix timestamp representing the time at which this Opportunity was created.
#' date_modified       number  A Unix timestamp representing the time at which this Opportunity was last modified.
#' custom_fields[]     list  An array of custom field values belonging to the Opportunity.
#' custom_fields[].custom_field_definition_id  number  The id of the Custom Field Definition for which this Custom Field stores a value.
#' custom_fields[].value mixed The value (number, string, option id, or timestamp) of this Custom Field.
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content authenticate add_headers http_type user_agent
#' @seealso \code{\link{cppr_getCompanyById}} for full list of available Company fields
#' @family Company functions
#' @examples
#' \dontrun{
#' company <- cppr_getCompanyById(12345)
#' print(paste("it's name is",company$name))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getCompanyById <- function(id) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.numeric(id))

    api <- "/companies/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api,id)
    company <- NULL

    printIfVerbose("cppr_getCompanyById:",path)

    raw.result <- httr::GET(url = .pkgglobalenv$cppr_Url, path = path, 
                      httr::user_agent(.pkgglobalenv$cppr_Ua),
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
          printIfVerbose("cppr_getCompanyById: no matching Company found in Copper with Id",id)
          company <- as.data.frame(NULL) # return an empty data frame
        } else {
          company <- result
          printIfVerbose("cppr_getCompanyById result:",class(company),"of",length(company),"fields")
        }
    } else{
        if(  raw.result$status_code == 404 ) {
          printIfVerbose("cppr_getCompanyById Failed. status code 404: Not found")
          company <- as.data.frame(NULL) # return an empty data frame
        } else if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getCompanyById Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_getCompanyById Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getCompanyById Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getCompanyById Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getCompanyById(%d): API request failed. HTTP Status code %s: %s", 
              id, result$status, result$message), call. = FALSE)
    }   
    return(company)
}

# ----------------------------------------------------------------------------
#' cppr_getCompanyByName
#'
#' get the Company record using its Name 
#'
#' @note This uses the search API, which could return multiple companies with the same name. 
#' Only the first match is returned! (if you initialized the package with Verbose=TRUE, 
#' you will see a warning displayed in this case)
#' To be sure of getting the record you want, use cppr_getCompanyById which uses
#' a field guaranteed by Copper to be unique, or use cppr_getCompanies() to get all matches.
#' @param name string - the name of the Company to return
#' @return a List of fields with various types, or Null if not found
#' @importFrom jsonlite fromJSON
#' @importFrom httr POST content authenticate add_headers http_type user_agent
#' @seealso \code{\link{cppr_getCompanyById}} for full list of available Company fields
#' @family Company functions
#' @examples
#' \dontrun{
#' company <- cppr_getCompanyByName("Kodak")
#' print(paste("company Id is",company$id))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getCompanyByName <- function(name) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.character(name))

    api <- "/companies/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api,"search")
    company <- NULL

    printIfVerbose("cppr_getCompanyByName:",path,name)

    raw.result <- httr::POST(url = .pkgglobalenv$cppr_Url, path = path, 
                      httr::user_agent(.pkgglobalenv$cppr_Ua),
                      httr::add_headers(
                        `X-PW-AccessToken` = .pkgglobalenv$cppr_AccessToken,
                        `X-PW-Application` = "developer_api",
                        `X-PW-UserEmail` = .pkgglobalenv$cppr_UserEmail,
                        `Content-Type` = "application/json"),
                      httr::authenticate(.pkgglobalenv$cppr_AccessToken,"",type="basic"),
                      body= list( `name` = name),
                      encode = "json")
    if (httr::http_type(raw.result) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    raw.content <- rawToChar(raw.result$content)
    result <- jsonlite::fromJSON(raw.content, flatten=TRUE)

    if(raw.result$status_code == 200){
        if( length(result)==0 ){
          printIfVerbose("cppr_getCompanyByName: no matching Company found in Copper named",name)
          company <- as.list(NULL) # empty list
        } else {
          printIfVerbose("cppr_getCompanyByName: Got",nrow(result),"Company records from Copper named",name,
                         ". Returning the first one with the Id",company$id[1])
          company <- as.list(result[1,])
          printIfVerbose("cppr_getCompanyByName result:",class(company),"of",length(company),"fields")
        }
    } else{
        if(  raw.result$status_code == 404 ) {
          printIfVerbose("cppr_getCompanyByName Failed. status code 404: Not found")
          company <- as.data.frame(NULL) # return an empty data frame
        } else if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getCompanyByName Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_getCompanyByName Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getCompanyByName Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getCompanyByName Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getCompanyByName(%s): API request failed. HTTP Status code %s: %s", 
              name, result$status, result$message), call. = FALSE)
    }   
    return(company)
}

# ----------------------------------------------------------------------------
#' cppr_getCompanies
#'
#' return a full table of all Company records from Copper
#'
#' @param assignee_id numeric - select companies assigned to this User
#' @param country string - select companies from this country (specify the 2-letter country code, e.g. "NZ")
#' @param state   string - select companies from this state (specify the full name of state, e.g. "Idaho")
#' @param tags    string - select companies with one of these tags 
#' @param sort_by values can be any of : (can be combined to provided ANDed)
#' name
#' assignee
#' company_name
#' customer_source
#' city
#' state
#' inactive_days
#' @param sort_direction can be "desc" or "asc"
#' @return a data frame of Company field lists, or Null if not found
#' each Company is a row in the data frame, and contains a list of fields 
#' @importFrom utils str
#' @importFrom jsonlite fromJSON
#' @importFrom httr POST content authenticate add_headers http_type user_agent
#' @seealso \code{\link{cppr_getCompanyById}} for full list of available Company fields
#' @family Company functions
#' @examples
#' \dontrun{
#' companies <- cppr_getCompanies()
#' print(paste("retrieved",nrow(companies),"companies from Copper"))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getCompanies <- function(assignee_id=NULL,country=NULL,state=NULL,tags=NULL,sort_by="name", sort_direction="asc") {

  stopifnot(pkg_is_initialized())    
  stopifnot(is.character(sort_by))
  stopifnot(is.character(sort_direction))
  stopifnot(sort_direction %in% c("asc","desc"))

  api <- "/companies/"
  path <- paste0(.pkgglobalenv$cppr_Endpoint,api,"search")
  companies <- as.data.frame(NULL)
  
  printIfVerbose("cppr_getCompanies: path for api call=",path, str(list( 
              `assignee_ids` = assignee_id,
              `country` = country,
              `state` = state,
              `tags` = tags,
              `sort_by` = sort_by,
              `sort_direction` = sort_direction)))

  # init
  pageSize <- 200 # NB: page_size max is 200
  totalHits <- 0
  cumTotal <- 0
  maxBatches <- 100 # set a plausible upper limit on pages of companies

  for( pageNum in 1:maxBatches ){

    companiesBatch <- as.data.frame(NULL) # init an empty data frame to hold each batch retrieved
    printIfVerbose("cppr_getCompanies: getting page", pageNum)


    raw.result <- httr::POST(url = .pkgglobalenv$cppr_Url, path = path, 
                      httr::user_agent(.pkgglobalenv$cppr_Ua),
                      httr::add_headers(
                        `X-PW-AccessToken` = .pkgglobalenv$cppr_AccessToken,
                        `X-PW-Application` = "developer_api",
                        `X-PW-UserEmail` = .pkgglobalenv$cppr_UserEmail,
                        `Content-Type` = "application/json"),
                      httr::authenticate(.pkgglobalenv$cppr_AccessToken,"",type="basic"),
                      body=list(`page_number` = pageNum,
                                `page_size` = pageSize,
                                `assignee_ids` = assignee_id,
                                `country` = country,
                                `state` = state,
                                `tags` = tags,
                                `sort_by` = sort_by,
                                `sort_direction` = sort_direction),
                      encode = "json")
    if (httr::http_type(raw.result) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    raw.content <- rawToChar(raw.result$content)
    result <- jsonlite::fromJSON(raw.content, flatten=TRUE)

    if(raw.result$status_code == 200){
      if( length(result)==0 ){
        printIfVerbose("cppr_getCompanies: received empty batch")
        break 
      } else {
        companiesBatch <- as.data.frame(result)
        printIfVerbose("cppr_getCompanies result: got",nrow(companiesBatch),"companies in batch",pageNum)

        if(pageNum == 1) {
          companies <- companiesBatch
          totalHits <- raw.result$headers[[11]]
          printIfVerbose("cppr_getCompanies: total number of matching companies found:",totalHits)
        }
        else {
          if(!is.null(companiesBatch))
            if(length(companiesBatch) > 0 )
              if(nrow(companiesBatch) > 0 )
                companies <- rbind(companies,companiesBatch)
        } # else pageNum > 1
      } # else length result >0
    } else{
        companies <- NULL # set error return code

        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getCompanies Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_getCompanies Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getCompanies Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getCompanies Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getCompanies(): API request failed. HTTP Status code %s: %s", 
              result$status, result$message), call. = FALSE)
        break
    }  # else status != 200

   if( length(companiesBatch) == 0) {
      printIfVerbose("cppr_getCompanies: Done. got all",totalHits,"companies returning",nrow(companies),"records in companies dataframe")
      break
    }
  } # for 
 return(companies)
}

# ----------------------------------------------------------------------------
#' cppr_updateCompany
#'
#' updates a Company record in Copper using a list of values you provide 
#' 
#' @param id numeric - the Id of the Company to be updated
#' @param list_of_fields list - containing oly the fields you want to modify
#' @return the id of the Company record if successful else NULL 
#' @importFrom jsonlite fromJSON
#' @importFrom httr PUT content authenticate add_headers http_type user_agent
#' @seealso \code{\link{cppr_getCompanyById}} for full list of Company fields
#' @family Company functions
#' @examples
#' \dontrun{
#' fields <- list( `assignee_id` = 12345 )
#' if(cppr_updateCompany(fields))
#'   print("succesfully assigned new owner"))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_updateCompany <- function(id,list_of_fields) {

  stopifnot(pkg_is_initialized())    
  stopifnot(is.numeric(id))
  stopifnot(is.list(list_of_fields))

    api <- "/companies/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api,id)
    returnId = NULL

    printIfVerbose("cppr_updateCompany:",path)

    raw.result <- httr::PUT(url = .pkgglobalenv$cppr_Url, path = path, 
                      httr::user_agent(.pkgglobalenv$cppr_Ua),
                      httr::add_headers(
                        `X-PW-AccessToken` = .pkgglobalenv$cppr_AccessToken,
                        `X-PW-Application` = "developer_api",
                        `X-PW-UserEmail` = .pkgglobalenv$cppr_UserEmail,
                        `Content-Type` = "application/json"),
                      httr::authenticate(.pkgglobalenv$cppr_AccessToken,"",type="basic"),
                      body= list_of_fields,
                      encode = "json")
    if (httr::http_type(raw.result) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    raw.content <- rawToChar(raw.result$content)
    result <- jsonlite::fromJSON(raw.content, flatten=TRUE)

    if(raw.result$status_code == 200){
        returnId = result$id
        printIfVerbose("cppr_updateCompany: successfully updated Company with id",returnId)
    } else{
        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_updateCompany Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_updateCompany Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_updateCompany Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_updateCompany Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_updateCompany(%d): API request failed. HTTP Status code %s: %s", 
              id, result$status, result$message), call. = FALSE)
    }   
    return(returnId)
}

# ----------------------------------------------------------------------------
#' cppr_createNewCompany
#' 
#' Creates a new Company record in Copper using the list of values you provide.
#' You only need to provide the same mandatory fields required when creating a
#' Company manually in the Copper UI 
#' 
#' @param company list - a list of Company fields 
#' @return the id given to the new Company record if successful else NULL
#' @importFrom jsonlite fromJSON
#' @importFrom httr POST content authenticate add_headers http_type user_agent
#' @seealso \code{\link{cppr_getCompanyById}} for full list of available Company fields
#' @family Company functions
#' @examples 
#' \dontrun{
#' fields <- list( `name` = "Kodak",
#'                 `assignee_id` = 12345)
#' newCompanyId <- cppr_createNewCompany(fields)) 
#' if(!is.null(newCompanyId))
#'   sprint("successfully created a new Company with id %d",newCompanyId)
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_createNewCompany <- function(company) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.list(company))

    api <- "/companies/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api)
    returnId = NULL

    printIfVerbose("cppr_createNewCompany:",path,company$name)

    raw.result <- httr::POST(url = .pkgglobalenv$cppr_Url, path = path, 
                      httr::user_agent(.pkgglobalenv$cppr_Ua),
                      httr::add_headers(
                        `X-PW-AccessToken` = .pkgglobalenv$cppr_AccessToken,
                        `X-PW-Application` = "developer_api",
                        `X-PW-UserEmail` = .pkgglobalenv$cppr_UserEmail,
                        `Content-Type` = "application/json"),
                      httr::authenticate(.pkgglobalenv$cppr_AccessToken,"",type="basic"),
                      body= company,
                      encode = "json")
    if (httr::http_type(raw.result) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    raw.content <- rawToChar(raw.result$content)
    result <- jsonlite::fromJSON(raw.content, flatten=TRUE)

    if(raw.result$status_code == 200){
        returnId = result$id
        printIfVerbose("cppr_createNewCompany: successfully created new Company with id",returnId)

    } else{
        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_createNewCompany Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_createNewCompany Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_createNewCompany Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_createNewCompany Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_createNewCompany(): API request failed. HTTP Status code %s: %s", 
              result$status, result$message), call. = FALSE)
    }   
    return(returnId)
}

# ----------------------------------------------------------------------------
#' cppr_deleteCompany
#'
#' permanently removes a Company record in Copper identified by the object id
#' 
#' @param id numeric - the Id of the Company to be deleted
#' @return the id of the Company record if successful else NULL 
#' @importFrom jsonlite fromJSON
#' @importFrom httr DELETE content authenticate add_headers http_type user_agent
#' @seealso \code{\link{cppr_getCompanyById}} for full list of Company fields
#' @family Company functions
#' @examples
#' \dontrun{
#' if(cppr_deleteCompany(id))
#'   sprintf("succesfully deleted Company %d",id)
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_deleteCompany <- function(id) {

  stopifnot(pkg_is_initialized())    
  stopifnot(is.numeric(id))

    api <- "/companies/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api,id)
    returnId = NULL

    printIfVerbose("cppr_deleteCompany:",path)

    if(is.null(cppr_getCompanyById(id))){
      printIfVerbose("cppr_deleteCompany: Warning - no Company found with the id",id)
    }

    raw.result <- httr::DELETE(url = .pkgglobalenv$cppr_Url, path = path, 
                      httr::user_agent(.pkgglobalenv$cppr_Ua),
                      httr::add_headers(
                        `X-PW-AccessToken` = .pkgglobalenv$cppr_AccessToken,
                        `X-PW-Application` = "developer_api",
                        `X-PW-UserEmail` = .pkgglobalenv$cppr_UserEmail,
                        `Content-Type` = "application/json"),
                      httr::authenticate(.pkgglobalenv$cppr_AccessToken,"",type="basic"),
                      encode = "json")
    if (httr::http_type(raw.result) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    raw.content <- rawToChar(raw.result$content)
    result <- jsonlite::fromJSON(raw.content, flatten=TRUE)

    if(raw.result$status_code == 200){
        returnId = result$id
        printIfVerbose("cppr_deleteCompany: successfully deleted Company with id",returnId)
    } else{
        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_deleteCompany Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_deleteCompany Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_deleteCompany Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_deleteCompany Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_deleteCompany(%d): API request failed. HTTP Status code %s: %s", 
              id, result$status, result$message), call. = FALSE)
    }   
    return(returnId)
}

