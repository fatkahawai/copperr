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

# ----------------------------------------------------------------------------
#' cppr_getLeads
#'
#' Get a table containg all Leads from Copper. You can filter the recirds using the optional arguments.
#' Each row of the table is a Lead, represented by a list of field names and values. 
#' Some fields are themselves lists.
#'
#' @param assignee_id numeric - select leads assigned to this User
#' @param country string - select leads from this country (specify the 2-letter country code, e.g. "NZ")
#' @param state   string - select leads from this state (specify the full name of state, e.g. "Idaho")
#' @param tags    string - select leads with one of these tags 
#' @param sort_by  string - "name", etc
#' @param sort_direction - allowed values "asc" or "desc"
#' @return a data frame
#' Field          Type    Details
#' id             number  Unique identifier 
#' name           string  firstname lastname
#' address        data frame encapulsation of street,city,postcode,state, country
#' assignee_id    number  Unique identifier of the User that will be the owner of the Lead.
#' company_id     string  The unique identifier of the primary Company with which the Lead is associated.
#' company_name   string  The name of the primary Company with which the Lead is associated.
#' customer_source_id number  The unique identifier of the source of the Lead
#' details        string  Description of the Lead.
#' email[]       data frame  An array of email addresses belonging to the Lead.
#' email[].email  string  An email address.
#' email[].category string  The category of the email address.
#' phone_numbers[] list  An array of phone numbers belonging to the Lead.
#' phone_numbers[].number  string  A phone number.
#' phone_numbers[].category  string  The category of the phone number.
#' socials[]       list  An array of social profiles belonging to the Lead.
#' socials[].url    string  The URL of a social profile.
#' socials[].category  string  The category of the social profile.
#' status         string_enum 
#' tags           list  An array of the tags associated with the Lead, represented as strings.
#' title          string  The professional title of the Lead.
#' websites[]     list  An array of websites belonging to the Lead.
#' websites[].url  string  The URL of a website.
#' websites[].category string  The category of the website.
#' date_created   number  A Unix timestamp representing the time at which this Lead was created.
#' date_modified  number  A Unix timestamp representing the time at which this Lead was last modified.
#' custom_fields[] list  An array of custom field values belonging to the Person.
#' custom_fields[]   
#' .custom_field_definition_id number  The id of the Custom Field Definition for which this Custom Field stores a value.
#' custom_fields[]   
#' .value  mixed The value (number, string, option id, or timestamp) of this Custom Field.
#' @importFrom utils str
#' @importFrom jsonlite fromJSON 
#' @importFrom httr POST content authenticate add_headers http_type user_agent
#' @family Lead functions
#' @examples 
#' \dontrun{ 
#' leads <- cppr_getLeads()
#' print(paste("retrieved",nrow(leads),"leads from PW"))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getLeads <- function(assignee_id=NULL,country=NULL,state=NULL,tags=NULL,sort_by="name",sort_direction="asc") {

  stopifnot(pkg_is_initialized())    
  stopifnot(is.character(sort_by))
  stopifnot(is.character(sort_direction))
  stopifnot(sort_direction %in% c("asc","desc"))

  api <- "/leads/search"
  path <- paste0(.pkgglobalenv$cppr_Endpoint,api)
  printIfVerbose("cppr_getLeads:",path, str(list( 
              `assignee_ids` = assignee_id,
              `country` = country,
              `state` = state,
              `tags` = tags,
              `sort_by` = sort_by,
              `sort_direction` = sort_direction)))

  leads <- as.data.frame(NULL)

# init
  pageSize <- 200
  totalHits <- 0
  cumTotal <- 0

  for( pageNum in 1:100 ){

    printIfVerbose("cppr_getLeads: getting page", pageNum)


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
        printIfVerbose("cppr_getLeads: empty batch received")
        break
      } else {
        leadsBatch <- as.data.frame(result)

        printIfVerbose("cppr_getLeads: got",nrow(leadsBatch),"leads in batch",pageNum)

        if(pageNum == 1) { # only on first time thru loop
          leads <- leadsBatch
          totalHits <- raw.result$headers[[11]]
          printIfVerbose("cppr_getLeads: total number of matching leads found:",totalHits)
        }
        else {
          if(!is.null(leadsBatch))
            if(length(leadsBatch) > 0 )
              if(nrow(leadsBatch) > 0 ){
                printIfVerbose("cppr_getLeads: appending new batch of leads found")
                leads <- rbind(leads,leadsBatch)
                printIfVerbose("cppr_getLeads: appended")
              }
        } # else pageNum >1
      } # else length result >0
    } else{
        leads <- NULL # set error return code
        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getLeads Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_getLeads Failed. status code 422: invalid argument")
        } else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getLeads Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getLeads Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getLeads(): API request failed. HTTP Status code %s: %s", 
              result$status, result$message), call. = FALSE)
        break
    }  # else result != 200

   if( length(leadsBatch) == 0) {
      printIfVerbose("cppr_getLeads: Done. got all",totalHits,"leads returning",nrow(leads),"records in leads dataframe")
      break
    }
  } # for     
  return(leads)
}

# ----------------------------------------------------------------------------
#' cppr_updateLead
#'
#' Update an Lead record using the fields passed as arguments  
#'
#' @seealso \code{\link{cppr_getLeads}} for full list of Lead fields
#'
#' @param id numeric - the Id of the Lead to be updated
#' @param list_of_fields list - containing only the fields you want to modify
#'
#' @return the id of the Lead if update was successful, else NULL
#' @importFrom jsonlite fromJSON 
#' @importFrom httr PUT content authenticate add_headers http_type user_agent
#' @family Lead functions
#' @examples 
#' \dontrun{ 
#' fields <- list( `details` = "updated this Lead",
#'                 `assignee_id` = myUserId )
#' if(cppr_updateLead(id,fields)) 
#'   sprintf("Successfully updated details of Lead %d and reassigned to user %d",id,myUserId)
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_updateLead <- function(id,list_of_fields) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.numeric(id))
    stopifnot(is.list(list_of_fields))

    api <- "/leads/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api,id)
    returnId = NULL

    printIfVerbose("cppr_updateLead: path=",path,list_of_fields)

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
        printIfVerbose("cppr_updateLead: Successfully updated Lead",returnId)
    } else{
        if(  raw.result$status_code == 404 ) {
          printIfVerbose("cppr_updateLead Failed. status code 404: Not found")
        } else if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_updateLead Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_updateLead Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_updateLead Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_updateLead Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_updateLead(%d): API request failed. HTTP Status code %s: %s", 
              id, result$status, result$message), call. = FALSE)
    }   
    return(returnId)
}

# ----------------------------------------------------------------------------
#' cppr_createNewLead
#'
#' Create a new Lead record using the fields passed as arguments  
#'
#' @seealso \code{\link{cppr_getLeads}} for full list of Lead fields
#'
#' @param lead list - containing at a minimum the mandatory fields of your new Lead
#'
#' @return the id of the Lead if created successfully, else NULL
#' @importFrom jsonlite fromJSON 
#' @importFrom httr POST content authenticate add_headers http_type user_agent
#' @family Lead functions
#' @examples 
#' \dontrun{ 
#' fields <- list( `name` = "New Lead",
#'                 `assignee_id` = myUserId )
#' if(cppr_createNewLead(fields)) 
#'   sprintf("Successfully created new Lead %d",id)
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_createNewLead <- function(lead) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.list(lead))

    api <- "/leads/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api)
    returnId = NULL

    printIfVerbose("cppr_createNewLead: path=",path,lead)

    raw.result <- httr::POST(url = .pkgglobalenv$cppr_Url, path = path, 
                      httr::user_agent(.pkgglobalenv$cppr_Ua),
                      httr::add_headers(
                        `X-PW-AccessToken` = .pkgglobalenv$cppr_AccessToken,
                        `X-PW-Application` = "developer_api",
                        `X-PW-UserEmail` = .pkgglobalenv$cppr_UserEmail,
                        `Content-Type` = "application/json"),
                      httr::authenticate(.pkgglobalenv$cppr_AccessToken,"",type="basic"),
                      body= lead,
                      encode = "json")
    if (httr::http_type(raw.result) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    raw.content <- rawToChar(raw.result$content)
    result <- jsonlite::fromJSON(raw.content, flatten=TRUE)

    if(raw.result$status_code == 200){
        returnId = result$id
        printIfVerbose("cppr_createNewLead: Successfully created Lead",returnId)
    } else{
        if(  raw.result$status_code == 404 ) {
          printIfVerbose("cppr_createNewLead Failed. status code 404: Not found")
        } else if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_createNewLead Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_createNewLead Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_createNewLead Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_createNewLead Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_createNewLead(): API request failed. HTTP Status code %s: %s", 
              result$status, result$message), call. = FALSE)
    }   
    return(returnId)
}

# ----------------------------------------------------------------------------
#' cppr_deleteLead
#'
#' Permanently remove a Lead record identified by the the id passed 
#' as argument
#'
#' @param id numeric - the Id of the Lead to be updated
#'
#' @return the id of the Lead if update was successful, else NULL
#' @importFrom jsonlite fromJSON 
#' @importFrom httr DELETE content authenticate add_headers http_type user_agent
#' @family Lead functions
#' @examples 
#' \dontrun{ 
#' if(!is.null(cppr_deleteLead(id))) 
#'   sprintf("Lead %d successfully removed",id)
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_deleteLead <- function(id) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.numeric(id))

    api <- "/leads/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api,id)
    returnId = NULL

    printIfVerbose("cppr_deleteLead: path=",path)

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
        printIfVerbose("cppr_deleteLead: Successfully removed Lead",returnId)
    } else{
        if(  raw.result$status_code == 404 ) {
          printIfVerbose("cppr_deleteLead Failed. status code 404: Not found")
        } else if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_deleteLead Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_deleteLead Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_deleteLead Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_deleteLead Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_deleteLead(%d): API request failed. HTTP Status code %s: %s", 
              id, result$status, result$message), call. = FALSE)
    }   
    return(returnId)
}

