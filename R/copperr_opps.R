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
# you have to paginate the search results using one of the following strategies in 
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
#' cppr_updateOppCustomFieldDirect
#'
#' update any custom field of an Opportunity that is not a dropdown (which needs an option id)
#' can be text, number, currency, date or checkbox
#'
#' @param id numeric - Copper Id for the Opportunity you are updating
#' @param field_name string - name of custom field, e.g. "Nickname"
#' @param value various types - value to set the field to. e.g. "Bucky" or 23 or TRUE
#' @return the id of the opp if update was successful, else NULL
#' @family Opportunity functions
#' @examples
#' \dontrun{ 
#' if(cppr_updateOppCustomFieldDirect(202193, "Sales Region", "Mid-West"))
#'   print("Opportunity updated successfully"))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_updateOppCustomFieldDirect <- function(id,field_name,value) {

   stopifnot(pkg_is_initialized())    
   stopifnot(is.numeric(id))
   stopifnot(is.character(field_name))

   printIfVerbose("cppr_updateOppCustomFieldDirect:",id,field_name,value)
   
   fieldId <- cppr_getCustomFieldId(field_name)
   fieldsList <- list( `custom_field_definition_id` = fieldId,
                       `value` = value)
   mods <- list(`custom_fields` = data.frame(fieldsList))

   return(cppr_updateOpp(id,mods))
}

# ----------------------------------------------------------------------------
#' cppr_updateOppCustomFieldDropdown
#'
#' updates an Opportunity: changes the value for a custom field of dropdown type
#'
#' @param id numeric - Copper Id for the Opportunity you are updating
#' @param field_name string - name of the custom field
#' @param value string - name of the dropdown option to find and set the id for
#'
#' @return the id of the opp if update was successful, else NULL
#' @family Opportunity functions
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @examples
#' \dontrun{ 
#' if(cppr_updateOppCustomFieldDropdown(12345,"Industry","Finance")) 
#'   print("Opp updated successfully!")
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_updateOppCustomFieldDropdown <- function(id,field_name,value) {

   stopifnot(pkg_is_initialized())    
   stopifnot(is.numeric(id))
   stopifnot(is.character(field_name))
   stopifnot(is.character(value))

   printIfVerbose("cppr_updateOppCustomFieldDropdown:",id,field_name,value)

   fieldId <- cppr_getCustomFieldId(field_name)
   optionId <- cppr_getCustomFieldOptionId(field_name,value)
   fieldsList <- list( `custom_field_definition_id` = fieldId,
                       `value` = optionId)
   mods <- list(`custom_fields` = data.frame(fieldsList))

   return(cppr_updateOpp(id,mods))
}

# ----------------------------------------------------------------------------
#' cppr_updateOppCustomFieldDropdownMulti
#'
#' updates an Opportunity: changes the value for a custom field of the Multi-choice dropdown type
#' You need to pass it the Copper Ids of the dropdown options, not the option names
#'
#' @param id          numeric - Copper Id for the Opportunity you are updating
#' @param field_name   string - name of custom field
#' @param valueList   list - list of names of the dropdown options to select
#'
#' @return the id of the opp if update was successful, else NULL
#' @family Opportunity functions
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @examples 
#' \dontrun{ 
#' optionsList <- list("Finance","Banking")
#' if(cppr_updateOppCustomFieldDropdown(12345,"Industry",optionsList)) 
#' print("Opp updated successfully!")
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_updateOppCustomFieldDropdownMulti <- function(id,field_name,valueList) {

   stopifnot(pkg_is_initialized())    
   stopifnot(is.numeric(id))
   stopifnot(is.character(field_name))
   stopifnot(is.list(valueList))

   printIfVerbose("cppr_updateOppCustomFieldDropdownMulti:",id,field_name,valueList)

   fieldId <- cppr_getCustomFieldId(field_name)

   optionsIdList <- list()
   for(i in 1:length(valueList)){
      optionsIdList[[i]] <- cppr_getCustomFieldOptionId(field_name,valueList[[i]])
      fieldsList <- list( `custom_field_definition_id` = fieldId,
                          `value` = optionsIdList)
    }
   mods <- list(`custom_fields` = data.frame(fieldsList))

   return(cppr_updateOpp(id,mods))
}

# ----------------------------------------------------------------------------
#' cppr_getOppById
#'
#' Get a single Opportunity Record identified by the Copper Id. 
#' A record is a list of field names and values
#'
#' @param id numeric - the Id of the opportunity to return
#' @return a List of Opportunity fields with various types, or Null if no Opp found\cr
#' Field               Type  Details\cr
#' id                  number  Unique identifier for the Opportunity.\cr
#' name               string  The name of the Opportunity.\cr
#' assignee_id         number  Unique identifier of the User that will be the owner of the Opportunity.\cr
#' close_date          string  The expected close date of the Opportunity in MM/DD/YYYY or DD/MM/YYYY format.\cr
#' company_id          string  The unique identifier of the primary Company with which the Opportunity is associated.\cr
#' company_name        string  The name of the primary Company with which the Opportunity is associated.\cr
#' customer_source_id  number  Unique identifier of the Customer Source that generated this Opportunity.\cr
#' details             string  Description of the Opportunity.\cr
#' loss_reason_id      number  If the Opportunity's status is "Lost", the unique identifier of the loss reason that caused this Opportunity to be lost.\cr
#' monetary_value      number  The monetary value of the Opportunity.\cr
#' pipeline_id         number  The unique identifier of the Pipeline in which this Opportunity is.\cr
#' primary_contact_id  number  The unique identifier of the Person who is the primary contact for this Opportunity.\cr
#' priority            string_enum The priority of the Opportunity. Valid values are: "None", "Low", "Medium", "High".\cr
#' pipeline_stage_id   number  The unique identifier of the Pipeline Stage of the Opportunity.\cr
#' status              string_enum The status of the Opportunity. Valid values are: "Open", "Won", "Lost", "Abandoned".\cr
#' tags                list  An array of the tags associated with the Opportunity, represented as strings.\cr
#' win_probability     number  The expected probability of winning the Opportunity. Valid values are [0-100] (inclusive).\cr
#' date_created        number  A Unix timestamp representing the time at which this Opportunity was created.\cr
#' date_modified       number  A Unix timestamp representing the time at which this Opportunity was last modified.\cr
#' custom_fields[]     list  An array of custom field values belonging to the Opportunity.\cr
#' custom_fields[].custom_field_definition_id  number  The id of the Custom Field Definition for which this Custom Field stores a value.\cr
#' custom_fields[].value mixed The value (number, string, option id, or timestamp) of this Custom Field.
#' @importFrom jsonlite fromJSON 
#' @importFrom httr GET content authenticate add_headers http_type user_agent
#' @family Opportunity functions
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @examples
#' \dontrun{ 
#' opp <- cppr_getOppById(12345)
#' print(paste("opp name is",opp$name))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getOppById <- function(id) {

  stopifnot(pkg_is_initialized())    
  stopifnot(is.numeric(id))

    api <- "/opportunities/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api,id)
    opp <- NULL

    printIfVerbose("cppr_getOppById:",path)

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
          printIfVerbose("cppr_getOppById: no matching Opp found in Copper with Id",id)
          opp <- as.list(NULL) # return an empty data frame
        } else {
          opp <- as.list(result)
          printIfVerbose("cppr_getOppById result: ",class(opp),"of",length(opp),"fields")
        }
    } else{
        if(  raw.result$status_code == 404 ) {
          printIfVerbose("cppr_getOppById Failed. status code 404: Not found")
          opp <- as.data.frame(NULL) # return an empty data frame
        } else {
          if(  raw.result$status_code == 400 ) {
      	    printIfVerbose("cppr_getOppById Failed: status code 400: Bad Request")
          } else if(  raw.result$status_code == 422 ) {
            printIfVerbose("cppr_getOppById Failed. status code 422: invalid argument")
          } else if(  raw.result$status_code == 429 ) {
            printIfVerbose("cppr_getOppById Failed. status code 429: Too Many Requests")
          }else{
        	 printIfVerbose("cppr_getOppById Failed. status code", raw.result$status_code )
          }
          stop( sprintf("cppr_getOppById(%d): API request failed. HTTP Status code %s: %s", 
                id, result$status, result$message), call. = FALSE)
      } # else not 404
    } # else not 200  
    return(opp)
}

# ----------------------------------------------------------------------------
#' cppr_getOppByName
#'
#' Get an Opportunity record identified by its Name.
#' A record is a list of field names and values
#'
#' @note This uses the search API, which could return multiple opportunities with the same name. 
#' Only the first match is returned! (if you initialized the package with Verbose=TRUE, 
#' you will see a warning displayed in this case)
#' To be sure of getting the record you want, use cppr_getOppById which uses
#' a field guaranteed by Copper to be unique, or use cppr_getOpps() to get all matches.
#' @param name string - the full name of the opportunity
#' @return the list of Opportunity fields, or Null on failure
#' @importFrom jsonlite fromJSON 
#' @importFrom httr POST content authenticate add_headers http_type user_agent
#' @seealso \code{\link{cppr_getOppById}} for full list of Opportunity fields
#' @family Opportunity functions
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @examples 
#' \dontrun{ 
#' opp <- cppr_getOppByName("Kodak film")
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getOppByName <- function(name) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.character(name))

    api <- "/opportunities/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api,"search")
    opp <- NULL

    printIfVerbose("cppr_getOppByName:",path,name)

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
          printIfVerbose("cppr_getOppByName: no matching Opps found in Copper named",name)
          opp <- as.list(NULL) # return an empty data frame
        } else {
          opp <- as.list(result[1,])
          printIfVerbose("cppr_getOppByName result:",class(opp),"of",length(opp),"fields")
        }
    } else{
        if(  raw.result$status_code == 404 ) {
          printIfVerbose("cppr_getOppByName Failed. status code 404: Not found")
        } else if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getOppByName Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_getOppByName Failed. status code 422: invalid argument")
        } else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getOppByName Failed. status code 429: Too Many Requests")
        } else{
          printIfVerbose("cppr_getOppByName Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getOppByName(%s): API request failed. HTTP Status code %s: %s", 
              name,result$status, result$message), call. = FALSE)
    }   
    return(opp)
}

# ----------------------------------------------------------------------------
#' cppr_getOpps
#
#' Get a table containing all Opportunities.
#' Each row of the data frame returned contains a list of field names and values.
#'
#' @param pipeline_id   numeric - select opps in this pipeline 
#' @param pipeline_stage_id   numeric - select opps at this stage in the pipeline
#' @param assignee_id numeric - select opps assigned to this User
#' @param tags    string - select leads with one of these tags 
#' @param sort_by string - values can be any of : (can be combined to provided ANDed)
#' \itemize{
#'   \item name
#'   \item assignee
#'   \item company_name
#'   \item customer_source_id
#'   \item monetary_value
#'   \item primary_contact
#'   \item priority
#'   \item status
#'   \item inactive_days
#'   \item last_interaction
#' }
#' @param sort_direction string - can be "desc" or "asc"
#'
#' @return a dataframe of Opportunity field lists, or Null if no matching Opps found
#' @importFrom utils str
#' @importFrom jsonlite fromJSON 
#' @importFrom httr POST content authenticate add_headers http_type user_agent
#' @seealso \code{\link{cppr_getOppById}} for full list of Opportunity fields
#' @family Opportunity functions
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @examples 
#' \dontrun{ 
#' opps <- cppr_getOpps()
#' print(paste("retrieved",nrow(opps),"opportunities from Copper"))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getOpps <- function( pipeline_id=NULL,pipeline_stage_id=NULL, assignee_id=NULL,
                      tags=NULL,sort_by="name", sort_direction="asc") {

  stopifnot(pkg_is_initialized())    
  stopifnot(is.character(sort_by))
  stopifnot(is.character(sort_direction))
  stopifnot(sort_direction %in% c("asc","desc"))

  api <- "/opportunities/"
  path <- paste0(.pkgglobalenv$cppr_Endpoint,api,"search")
  opps <- as.data.frame(NULL)
  
  printIfVerbose("cppr_getOpps: path for api call=", path, str(list( `pipeline_ids` = pipeline_id,
              `pipeline_stage_ids` = pipeline_stage_id,
              `assignee_ids` = assignee_id,
              `tags` = tags,
              `sort_by` = sort_by,
              `sort_direction` = sort_direction)))

  # init
  pageSize <- 200
  totalHits <- 0
  cumTotal <- 0
  maxBatches <- 100 # a plausible upper limit on number of pages you can get

  for( pageNum in 1:maxBatches ){

    oppsBatch <- as.data.frame(NULL) # init an empty data frame to hold each batch of opps retreived 
    printIfVerbose("cppr_getOpps: getting page", pageNum)

    raw.result <- httr::POST(url = .pkgglobalenv$cppr_Url, path = path, 
                      httr::user_agent(.pkgglobalenv$cppr_Ua),
                      httr::add_headers(
                        `X-PW-AccessToken` = .pkgglobalenv$cppr_AccessToken,
                        `X-PW-Application` = "developer_api",
                        `X-PW-UserEmail` = .pkgglobalenv$cppr_UserEmail,
                        `Content-Type` = "application/json"),
                      httr::authenticate(.pkgglobalenv$cppr_AccessToken,"",type="basic"),
                      body=list( `page_number` = pageNum,
                                 `page_size` = pageSize,
                                 `pipeline_ids` = pipeline_id,
                                 `pipeline_stage_ids` = pipeline_stage_id,
                                 `assignee_ids` = assignee_id,
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
        printIfVerbose("cppr_getOpps: received empty batch")
        break
      } else {
        oppsBatch <- result
#        printIfVerbose("All-headers:")
#        printIfVerbose(raw.result$headers)
        printIfVerbose("cppr_getOpps: got",nrow(oppsBatch),"opps in batch",pageNum)

        if(pageNum == 1) {
          opps <- oppsBatch
          totalHits <- raw.result$headers[[11]]
          printIfVerbose("cppr_getOpps: total number of matching Opps found:",totalHits)
        }
        else {
          if(!is.null(oppsBatch))
            if(length(oppsBatch) > 0 )
              if(nrow(oppsBatch) > 0 )
                opps <- rbind(opps,oppsBatch)
        } # else pageNum >1
      } # else length result >0
    } else{
        opps <- NULL # set error return code 

        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getOpps Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_getOpps Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getOpps Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getOpps Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getOpps(): API request failed. HTTP Status code %s: %s", 
              result$status, result$message), call. = FALSE)
        break
    }  # else status != 200

   if( length(oppsBatch) == 0) {
      printIfVerbose("cppr_getOpps: Done. got all",totalHits,"opps. returning",nrow(opps),"records in opps dataframe")
      break
    }
  } # for 
 return(opps)
}

# ----------------------------------------------------------------------------
#' cppr_updateOpp
#'
#' Update an Opportunity record using the fields passed as arguments  
#'
#' @seealso \code{\link{cppr_getOppById}} for full list of Opportunity fields
#'
#' @param id numeric - the Id of the Opportunity to be updated
#' @param list_of_fields list - containing only the fields you want to modify
#'
#' @return the id of the opp if update was successful, else NULL
#' @importFrom jsonlite fromJSON 
#' @importFrom httr PUT content authenticate add_headers http_type user_agent
#' @family Opportunity functions
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @examples 
#' \dontrun{ 
#' fields <- list( `name` = "new name for this Opportunity",
#'                 `assignee_id` = 12345 )
#' if(cppr_updateOpp(23456,fields)) 
#'   print("opportunity successfully updated")
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_updateOpp <- function(id,list_of_fields) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.numeric(id))
    stopifnot(is.list(list_of_fields))

    api <- "/opportunities/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api,id)
    returnId = NULL

    printIfVerbose("cppr_updateOpp: path=",path,list_of_fields)

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
        printIfVerbose("cppr_updateOpp: Successfully updated Opp",returnId)
    } else{
        if(  raw.result$status_code == 404 ) {
          printIfVerbose("cppr_updateOpp Failed. status code 404: Not found")
        } else if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_updateOpp Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_updateOpp Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_updateOpp Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_updateOpp Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_updateOpp(%d): API request failed. HTTP Status code %s: %s", 
              id,result$status, result$message), call. = FALSE)
    }   
    return(returnId)
}

# ----------------------------------------------------------------------------
#' cppr_updateOppCloseDate
#'
#' Update the 'Close Date' field of an Opportuntiy record
#'
#' @param id numeric - the Id of the Opportunity to be updated
#' @param closedate_sz string - the close date in string format (e.g. "03/31/2018")
#' @note Unlike the other date fields used in Copper, the 'Close Date' field takes a 
#' text date, NOT a POSIX date class !!
#
#' @return the id of the opp if update was successful, else NULL
#' @family Opportunity functions
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @examples
#' \dontrun{ 
#' if(cppr_updateOppCloseDate(12345,"03/31/2018")) 
#'   print("Close date changed successfully")
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_updateOppCloseDate <- function(id,closedate_sz) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.numeric(id))
    stopifnot(is.character(closedate_sz))


    printIfVerbose("cppr_updateOppCloseDate: setting close date for Opportunity",id,"to",closedate_sz)


   return(cppr_updateOpp(id,list( `close_date` = closedate_sz)))
}

# ----------------------------------------------------------------------------
#' cppr_updateOppStage
#
#' Update the 'Stage' field of an Opportuntiy record
#'
#' @param id numeric - Copper Id for the Opportunity you are updating
#' @param stage_id numeric - the id of the Stage you want to move it to
#
#' @return the id of the opp if update was successful, else NULL
#' @seealso \code{\link{cppr_getPipelineStages}} to geta table of stage Ids and names
#' @family Opportunity functions
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @examples
#' \dontrun{ 
#' stages <- cppr_getPipelineStages(12345)
#' newStage <- stages$id[stages$name=="Qualified"]
#' if(cppr_updateOppStage(12345,newStage)) 
#'   print("Stage changed successfully")
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_updateOppStage <- function(id,stage_id) {

  stopifnot(pkg_is_initialized())    
  stopifnot(is.numeric(id))
  stopifnot(is.numeric(stage_id))


  return(cppr_updateOpp(id,list( `stage` = stage_id)))
}

# ----------------------------------------------------------------------------
#' cppr_createNewOpp
#' 
#' Create a New Opportunity record in Copper using the field values passed in the argument
#'
#' @param opp   list - a list of all the Opportunity fields
#' @return the id of the new Opp record if successfully created, else NULL
#' @importFrom jsonlite fromJSON 
#' @importFrom httr POST content authenticate add_headers http_type user_agent
#' @seealso \code{\link{cppr_getOppById}} for full list of Opportunity fields
#' @family Opportunity functions
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @examples 
#' \dontrun{ 
#' opp <- list( `name` = "New Name",
#'              `assignee_id` = 12345,
#'              `company_name` = "Kodak",
#'              `pipeline_id` = 23456)
#' newOppId <- cppr_updateOpp(34567,opp)
#' if(!is.null(newOppId)) 
#'   print("new opportunity successfully created with id %d",newOppId)
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_createNewOpp <- function(opp) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.list(opp))

    api <- "/opportunities/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api)
    returnId = NULL

    printIfVerbose("cppr_createNewOpp:",path,opp$name)

    raw.result <- httr::POST(url = .pkgglobalenv$cppr_Url, path = path, 
                      httr::user_agent(.pkgglobalenv$cppr_Ua),
                      httr::add_headers(
                        `X-PW-AccessToken` = .pkgglobalenv$cppr_AccessToken,
                        `X-PW-Application` = "developer_api",
                        `X-PW-UserEmail` = .pkgglobalenv$cppr_UserEmail,
                        `Content-Type` = "application/json"),
                      httr::authenticate(.pkgglobalenv$cppr_AccessToken,"",type="basic"),
                      body= opp,
                      encode = "json")
    if (httr::http_type(raw.result) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    raw.content <- rawToChar(raw.result$content)
    result <- jsonlite::fromJSON(raw.content, flatten=TRUE)

    if(raw.result$status_code == 200){
        returnId = result$id
        printIfVerbose("cppr_createNewOpp: successfully created new Opp with id",returnId)
    } else{
        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_createNewOpp Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_createNewOpp Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_createNewOpp Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_createNewOpp Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_createNewOpp(): API request failed. HTTP Status code %s: %s", 
              result$status, result$message), call. = FALSE)
    }   
    return(returnId)
}

# ----------------------------------------------------------------------------
#' cppr_deleteOpp
#'
#' Permanently remove an Opportunity record identified by the the id passed 
#' as argument
#'
#' @param id numeric - the Id of the Opportunity to be updated
#'
#' @return the id of the opp if update was successful, else NULL
#' @importFrom jsonlite fromJSON 
#' @importFrom httr DELETE content authenticate add_headers http_type user_agent
#' @family Opportunity functions
#' @examples 
#' \dontrun{ 
#' if(!is.null(cppr_deleteOpp(id))) 
#'   sprintf("opportunity %d successfully removed",id)
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_deleteOpp <- function(id) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.numeric(id))

    api <- "/opportunities/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api,id)
    returnId = NULL

    printIfVerbose("cppr_deleteOpp: path=",path,id)

    if(is.null(cppr_getOppById(id))){
      printIfVerbose("cppr_deleteOpp: Warning - no Opportunity found with the id",id)
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
        printIfVerbose("cppr_deleteOpp: Successfully removed Opp",returnId)
    } else{
        if(  raw.result$status_code == 404 ) {
          printIfVerbose("cppr_deleteOpp Failed. status code 404: Not found")
        } else if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_deleteOpp Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_deleteOpp Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_deleteOpp Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_deleteOpp Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_deleteOpp(%d): API request failed. HTTP Status code %s: %s", 
              id, result$status, result$message), call. = FALSE)
    }   
    return(returnId)
}
