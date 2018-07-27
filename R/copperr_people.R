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


# ----------------------------------------------------------------------------
#' cppr_updatePersonCustomFieldDirect
#'
#' update any custom field of an Person that is not a dropdown (which needs an option id)
#' can be text, number, currency, date or checkbox
#'
#' @param id numeric - Copper Id for the Person you are updating
#' @param field_name string - name of custom field, e.g. "Nickname"
#' @param value various types - value to set the field to. e.g. "Bucky" or 23 or TRUE
#' @return the id of the opp if update was successful, else NULL
#' @family Opportunity functions
#' @examples
#' \dontrun{ 
#' if(cppr_updatePersonCustomFieldDirect(202193, "Sales Region", "Mid-West"))
#'   print("Person updated successfully"))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_updatePersonCustomFieldDirect <- function(id,field_name,value) {

   stopifnot(pkg_is_initialized())    
   stopifnot(is.numeric(id))
   stopifnot(is.character(field_name))

   printIfVerbose("cppr_updatePersonCustomFieldDirect:",id,field_name,value)
   
   fieldId <- cppr_getCustomFieldId(field_name)
   fieldsList <- list( `custom_field_definition_id` = fieldId,
                       `value` = value)
   mods <- list(`custom_fields` = data.frame(fieldsList))

   return(cppr_updatePerson(id,mods))
}


# ----------------------------------------------------------------------------
#' cppr_getPersonByName 
#'
#' Get a People record identfied by the person's Name
#'
#' @note This uses the search API, which could return multiple people with the same name. 
#' Only the first match is returned! (if you initialized the package with Verbose=TRUE, 
#' you will see a warning displayed in this case)
#' To be sure of getting the record you want, use cppr_getPersonById or cppr_getPersonByEmail which both
#' use a field guaranteed by Copper to be unique, or use cppr_getPeople() to get all matches.
#' @param name string - the full name of the person 'firstname lastname'
#' @return a list of Person fields, or NULL if unsuccessful.\cr
#' Field        Type    Details\cr
#' id           number  Unique identifier\cr
#' name         string  firstname lastname.\cr
#' address      address An encapsulation of the Person's street, city, state, postal code, and country.\cr
#' assignee_id  number Unique identifier of the User that will be the owner of the Person.\cr
#' etc
#' @importFrom jsonlite fromJSON 
#' @importFrom httr POST content authenticate add_headers http_type user_agent
#' @seealso \code{\link{cppr_getPeople}} for full list of Person fields
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @family People functions
#' @examples 
#' \dontrun{ 
#' person <- cppr_getPersonByName("John Smith")
#' print(paste("Person is related to the company",person$company_name))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getPersonByName <- function(name) {

    stopifnot(pkg_is_initialized())
    stopifnot(is.character(name))

    api <- "/people/search"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api)

    printIfVerbose("cppr_getPersonByName:",.pkgglobalenv$cppr_Url,path,name)

    person <- NULL

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
          printIfVerbose("cppr_getPersonByName: no matching record found in Copper for Person named",name)
          person <- as.list(NULL) # return an empty data frame
        } else {
          if(nrow(result)>1) 
            printIfVerbose("cppr_getPersonByName warning: multiple matches found for Person named",name,
                            ". Returning first match")
          person <- as.list(result[1,])
          printIfVerbose("cppr_getPersonByName returning: ",class(person),"of",length(person),"fields")
        }
    } else{
        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getPersonByName Failed. status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_getPersonByName Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getPersonByName Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getPersonByName Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getPersonByName(%s): API request failed. HTTP Status code %s: %s", 
              name, result$status, result$message), call. = FALSE)
    }   
    return(person)
}

# ----------------------------------------------------------------------------
#' cppr_getPersonById
#'
#' Get a People record identfied by the person's Id
#'
#' @param id    numeric - the Copper Id of the person 
#' @return a list, or NULL if unsuccessful.\cr
#' Field        Type    Details\cr
#' id           number  Unique identifier.\cr
#' name*        string  firstname lastname.\cr
#' address      address An encapsulation of the Person's street, city, state, postal code, and country.\cr
#' assignee_id  number Unique identifier of the User that will be the owner of the Person.\cr
#' etc
#' @importFrom jsonlite fromJSON 
#' @importFrom httr GET content authenticate add_headers http_type user_agent
#' @seealso \code{\link{cppr_getPeople}} for full list of Person fields
#' @seealso \code{\link{cppr_getPersonId}} to find the Person Id if you know their name
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @family People functions
#' @examples 
#' \dontrun{ 
#' id <- cppr_getPersonId("John Smith")
#' if( nrow(id) == 1 ) # found exactly one matching record in Copper
#'   person <- cppr_getPersonById(id)
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getPersonById <- function(id) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.numeric(id))

    api <- "/people/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api,id)

    printIfVerbose("cppr_getPersonById:",path)

    person <- NULL

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
          printIfVerbose("cppr_getPersonById: no matching record found in Copper for Person with Id",id)
          person <- as.list(NULL) # return an empty data frame
        } else {
          person <- as.list(result)
          printIfVerbose("cppr_getPersonById returning: ",class(person),"of",length(person),"fields")
        }
    } else{
        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getPersonById Failed. status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_getPersonById Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getPersonById Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getPersonById Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getPersonById(%d): API request failed. HTTP Status code %s: %s", 
              id, result$status, result$message), call. = FALSE)
    }   
    return(person)
}

# ----------------------------------------------------------------------------
#' cppr_getPersonByEmail
#'
#' Get a People record identfied by the person's Name
#'
#' @param email    string - the Copper Id of the person 
#' @return a list, or NULL if unsuccessful.\cr
#' Field        Type    Details\cr
#' id           number  Unique identifier \cr
#' name*        string  firstname lastname\cr
#' address      address An encapsulation of the Person's street, city, state, postal code, and country.\cr
#' assignee_id  number Unique identifier of the User that will be the owner of the Person.\cr
#' etc
#' @importFrom jsonlite fromJSON 
#' @importFrom httr POST content authenticate add_headers http_type user_agent
#' @seealso \code{\link{cppr_getPeople}} for full list of Person fields
#' @family People functions
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @examples 
#' \dontrun{ 
#' person <- cppr_getPersonByEmail("john@@foo.bar")
#' print(paste("his/her name is",person$name))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getPersonByEmail <- function(email) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.character(email))

    api <- "/people/fetch_by_email"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api)

    printIfVerbose("cppr_getPersonByEmail:",path,email)

    person <- NULL

    raw.result <- httr::POST(url = .pkgglobalenv$cppr_Url, path = path, 
                      httr::user_agent(.pkgglobalenv$cppr_Ua),
                      httr::add_headers(
                        `X-PW-AccessToken` = .pkgglobalenv$cppr_AccessToken,
                        `X-PW-Application` = "developer_api",
                        `X-PW-UserEmail` = .pkgglobalenv$cppr_UserEmail,
                        `Content-Type` = "application/json"),
                      httr::authenticate(.pkgglobalenv$cppr_AccessToken,"",type="basic"),
                      body= list( `email` = email),
                      encode = "json")
    if (httr::http_type(raw.result) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    raw.content <- rawToChar(raw.result$content)
    result <- jsonlite::fromJSON(raw.content, flatten=TRUE)

    if(raw.result$status_code == 200){
        if( length(result)==0 ){
          printIfVerbose("cppr_getPersonByEmail: no matching record found in Copper for Person with email",email)
          person <- as.list(NULL) # return an empty data frame
        } else {
          person <- as.list(result)
          printIfVerbose("cppr_getPersonByEmail result: ",class(person),"of",length(person),"fields")
        }
    } else{
        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getPersonByEmail Failed. status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_getPersonByEmail Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getPersonByEmail Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getPersonByEmail Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getPersonByEmail(%s): API request failed. HTTP Status code %s: %s", 
              email, result$status, result$message), call. = FALSE)
    }   
    return(person)
}

# ----------------------------------------------------------------------------
#' cppr_getPersonId
#'
#' Get a Copper Id for the Person record identfied by the person's Name
#'
#' @note This uses the search API, which could return multiple people with the same name. 
#' Only the first match is returned! (if you initialized the package with Verbose=TRUE, 
#' you will see a warning displayed in this case)
#' To be sure of getting the record you want, use cppr_getPersonById or cppr_getPersonByEmail which both
#' use a field guaranteed by Copper to be unique, or use getUsers() to get all matches.
#' @param name string - the full name of the person in format 'firstname lastname'
#' @return an numeric Id. The first matching Copper Id, 0 if no match found, or NULL on error.
#' @family People functions
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @examples
#' \dontrun{ 
#' id <- cppr_getPersonId("John Smith")
#' print(paste("found",nrow(id),"matching People with this name in Copper"))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getPersonId <- function(name) {

  stopifnot(pkg_is_initialized())    
  stopifnot(is.character(name))

  id <- NULL
  people <- cppr_getPersonByName(name)

  if(!is.null(people)){
    if( length(people)==0)
      id <- 0 # no match found
    else 
      id <-  people$id
  }
  return(id)
}
# ----------------------------------------------------------------------------
#' cppr_getPeople 
#'
#' Get a table containg all People from Copper. You can filter the recirds using the optional arguments.
#' Each row of the table is a Person, represented by a list of field names and values. 
#' Some fields are themselves lists.
#'
#' @param assignee_id numeric - select people assigned to this User
#' @param country string - select people from this country (specify the 2-letter country code, e.g. "NZ")
#' @param state   string - select people from this state (specify the full name of state, e.g. "Idaho")
#' @param tags    string - select people with one of these tags 
#' @param sort_by string - sort records by "name", etc
#' @param sort_direction string - allowed values "asc" or "desc"
#' @return a data frame, where each row is a Person, or NULL on error.\cr
#' Field          Type    Details\cr
#' id             number  Unique identifier \cr
#' name           string  firstname lastname\cr
#' address        address ecapulsation of street,city,postcode,state, country\cr
#' assignee_id    number  Unique identifier of the User that will be the owner of the Person.\cr
#' company_id     string  The unique identifier of the primary Company with which the Person is associated.\cr
#' company_name   string  The name of the primary Company with which the Person is associated.\cr
#' contact_type_id number  The unique identifier of the Contact Type of the Person.\cr
#' details        string  Description of the Person.\cr
#' emails[]       list  An array of email addresses belonging to the Person.\cr
#' emails[].email  string  An email address.\cr
#' emails[].category string  The category of the email address.\cr
#' phone_numbers[] list  An array of phone numbers belonging to the Person.\cr
#' phone_numbers[].number  string  A phone number.\cr
#' phone_numbers[].category  string  The category of the phone number.\cr
#' socials[]       list  An array of social profiles belonging to the Person.\cr
#' socials[].url    string  The URL of a social profile.\cr
#' socials[].category  string  The category of the social profile.\cr
#' tags           list  An array of the tags associated with the Person, represented as strings.\cr
#' title string  The professional title of the Person.\cr
#' websites[]     list  An array of websites belonging to the Person.\cr
#' websites[].url  string  The URL of a website.\cr
#' websites[].category string  The category of the website.\cr
#' date_created   number  A Unix timestamp representing the time at which this Person was created.\cr
#' date_modified  number  A Unix timestamp representing the time at which this Person was last modified.\cr
#' custom_fields[] list  An array of custom field values belonging to the Person.\cr
#' custom_fields[]   \cr
#' .custom_field_definition_id number  The id of the Custom Field Definition for which this Custom Field stores a value.\cr
#' custom_fields[]   \cr
#' .value  mixed The value (number, string, option id, or timestamp) of this Custom Field.
#' @importFrom utils str
#' @importFrom jsonlite fromJSON 
#' @importFrom httr POST content authenticate add_headers http_type user_agent
#' @family People functions
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @examples
#' \dontrun{ 
#' people <- cppr_getPeople()
#' print(paste("retrieved",nrow(people),"people from Copper"))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getPeople <- function(assignee_id=NULL,country=NULL,state=NULL,tags=NULL,sort_by="name",sort_direction="asc") {

  stopifnot(pkg_is_initialized())    
  stopifnot(is.character(sort_by))
  stopifnot(is.character(sort_direction))
  stopifnot(sort_direction %in% c("asc","desc"))

  api <- "/people/search"
  path <- paste0(.pkgglobalenv$cppr_Endpoint,api)
  printIfVerbose("cppr_getPeople:",path, str(list( 
              `assignee_ids` = assignee_id,
              `country` = country,
              `state` = state,
              `tags` = tags,
              `sort_by` = sort_by,
              `sort_direction` = sort_direction)))

  people <- as.data.frame(NULL)

  # init
  pageSize <- 200
  totalHits <- 0
  cumTotal <- 0
  maxBatches <- 100 # set a plausible upper bound on number of batches

  for( pageNum in 1:maxBatches ){

    peopleBatch <- as.data.frame(NULL) # init an empty data frame to hold each batch retrieved
    printIfVerbose("cppr_getPeople: getting page", pageNum)

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
        printIfVerbose("cppr_getPeople: empty batch received")
        break
      } else {
        peopleBatch <- result
        printIfVerbose("cppr_getPeople: got",nrow(peopleBatch),"people in batch",pageNum)

        if(pageNum == 1) { # only on first time thru loop
          people <- peopleBatch
          totalHits <- raw.result$headers[[11]]
          printIfVerbose("cppr_getPeople: total number of matching People found:",totalHits)
        }
        else {
          if(!is.null(peopleBatch))
            if(length(peopleBatch) > 0 )
              if(nrow(peopleBatch) > 0 ){
                printIfVerbose("cppr_getPeople: appending new batch of People found")
                people <- rbind(people,peopleBatch)
                printIfVerbose("cppr_getPeople: appended")
              }
        } #else PageNum >1
      } # else length(result)>0

    } else{
        people <- NULL # set the erro return code
        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getPeople Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_getPeople Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getPeople Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getPeople Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getPeople(): API request failed. HTTP Status code %s: %s", 
              result$status, result$message), call. = FALSE)
        break
    }  # else status != 200

   if( length(peopleBatch) == 0) {
      printIfVerbose("cppr_getPeople: Done. got all",totalHits,"people returning",nrow(people),"records in people dataframe")
      break
    }
  } # for     
  return(people)
}

# ----------------------------------------------------------------------------
#' cppr_getAllRecordsRelatedToPerson
#'
#' Get the ids of all records in Copper related to a Person. These can be 
#' Opportunities, Companies, Projects, etc
#'
#' @param id string - the id of the person 
#' @return a data frame of record ids , or NULL on error.\cr
#' Field        Type    Details\cr
#' id           number  Unique identifier of the record\cr
#' type         string  the type of record, e.g. "opportunity", "company", "project"
#' @importFrom jsonlite fromJSON 
#' @importFrom httr GET content authenticate add_headers http_type user_agent
#' @seealso \code{\link{cppr_getOppsRelatedToPerson}}
#' @seealso \code{\link{cppr_getPersonId}}
#' @family People functions
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @examples 
#' \dontrun{ 
#' relrecs <- cppr_getAllRecordsRelatedToPerson(12345)
#' relps <- dplyr::filter(relrcs,type=="project")
#' print(paste("this person is related to",nrow(relps),"projects"))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getAllRecordsRelatedToPerson <- function(id) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.numeric(id))

    api <- "/people/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api,id,"/related")

    printIfVerbose("cppr_getAllRecordsRelatedToPerson:",path)

    records <- NULL

    raw.result <- httr::GET(url = .pkgglobalenv$cppr_Url, path = path, 
                      httr::user_agent(.pkgglobalenv$cppr_Ua),
                      httr::add_headers(
                        `X-PW-AccessToken` = .pkgglobalenv$cppr_AccessToken,
                        `X-PW-Application` = "developer_api",
                        `X-PW-UserEmail` = .pkgglobalenv$cppr_UserEmail,
                        `Content-Type` = "application/json"),
                      httr::authenticate(.pkgglobalenv$cppr_AccessToken,"",type="basic")
                      )
    if (httr::http_type(raw.result) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    raw.content <- rawToChar(raw.result$content)
    result <- jsonlite::fromJSON(raw.content, flatten=TRUE)

    if(raw.result$status_code == 200){
        if( length(result)==0 ){
          printIfVerbose("cppr_getAllRecordsRelatedToPerson: no matching records found in Copper for Person with Id",id)
          records <- as.data.frame(NULL) # return an empty data frame
        } else {
          records <- as.data.frame(result)
          printIfVerbose("cppr_getAllRecordsRelatedToPerson result: ",class(records),"of",length(records),"fields")
        }

    } else{
        if(  raw.result$status_code == 404 ) {
          printIfVerbose("cppr_getAllRecordsRelatedToPerson Failed. status code 404: Not found")
          records <- as.data.frame(NULL) # return an empty data frame
        } else if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getAllRecordsRelatedToPerson Failed. status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_getAllRecordsRelatedToPerson Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getAllRecordsRelatedToPerson Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getAllRecordsRelatedToPerson Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getAllRecordsRelatedToPerson(%d): API request failed. HTTP Status code %s: %s", 
              id, result$status, result$message), call. = FALSE)
    }   
    return(records)
}

# ----------------------------------------------------------------------------
#' cppr_getOppsRelatedToPerson
#'
#' get the ids of all opportunity records in Copper related to a Person.
#'
#' @param id string - the id of the person 
#' @return a data frame of record ids , or NULL on error.\cr
#' Field        Type    Details\cr
#' id           number  Unique identifier of the record\cr
#' type         string  "opportunity"
#' @importFrom jsonlite fromJSON 
#' @importFrom httr GET content authenticate add_headers http_type user_agent
#' @seealso \code{\link{cppr_getAllRecordsRelatedToPerson}}
#' @seealso \code{\link{cppr_getPersonId}}
#' @family People functions
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @examples
#' \dontrun{ 
#' relopps <- cppr_getOppsRelatedToPerson(12345)
#' print(paste("this person is related to",nrow(relopps),"opportunities"))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getOppsRelatedToPerson <- function(id) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.numeric(id))

    api <- "/people/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api,id,"/related/opportunities")

    printIfVerbose("cppr_getOppsRelatedToPerson:",path)

    records <- NULL

    raw.result <- httr::GET(url = .pkgglobalenv$cppr_Url, path = path, 
                      httr::user_agent(.pkgglobalenv$cppr_Ua),
                      httr::add_headers(
                        `X-PW-AccessToken` = .pkgglobalenv$cppr_AccessToken,
                        `X-PW-Application` = "developer_api",
                        `X-PW-UserEmail` = .pkgglobalenv$cppr_UserEmail,
                        `Content-Type` = "application/json"),
                      httr::authenticate(.pkgglobalenv$cppr_AccessToken,"",type="basic")
                      )
    if (httr::http_type(raw.result) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    raw.content <- rawToChar(raw.result$content)
    result <- jsonlite::fromJSON(raw.content, flatten=TRUE)

    if(raw.result$status_code == 200){
        if( length(result)==0 ){
          printIfVerbose("cppr_getOppsRelatedToPerson: no matching Opps found in Copper for Person with Id",id)
          records <- as.data.frame(NULL) # return an empty data frame
        } else {
          records <- as.data.frame(result)
          printIfVerbose("cppr_getOppsRelatedToPerson result: ",class(records),"of",length(records),"fields")
        }
    } else{
        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getOppsRelatedToPerson Failed. status code 400: Bad Request")
        } else if(  raw.result$status_code == 404 ) {
          printIfVerbose("cppr_getOppsRelatedToPerson Failed. status code 404: Not found")
          records <- as.data.frame(NULL) # return an empty data frame
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_getOppsRelatedToPerson Failed. status code 422: invalid argument")
        } else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getOppsRelatedToPerson Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getOppsRelatedToPerson Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getOppsRelatedToPerson(%d): API request failed. HTTP Status code %s: %s", 
              id, result$status, result$message), call. = FALSE)
    }   
    return(records)
}


# ----------------------------------------------------------------------------
#' cppr_updatePerson
#'
#' Update a Person record identified by the the id 
#'
#' @param id numeric - the Id of the Person to be updated
#' @param list_of_fields list - a list of fields for the new Person 
#' \itemize{
#'   \item Field        Type    Details
#'   \item id           number  Unique identifier 
#'   \item name         string  firstname lastname
#'   \item address      address An encapsulation of the Person's street, city, state, postal code, and country.
#'   \item assignee_id  number Unique identifier of the User that will be the owner of the Person.
#'   \item etc
#' }
#' @return the id of the Person if update was successful, else NULL
#' @importFrom jsonlite fromJSON 
#' @importFrom httr DELETE content authenticate add_headers http_type user_agent
#' @family People functions
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @examples 
#' \dontrun{ 
#' if(!is.null(cppr_updatePerson(id))) 
#'   sprintf("Person %d successfully removed",id)
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_updatePerson <- function(id, list_of_fields) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.numeric(id))
    stopifnot(is.list(list_of_fields))

    api <- "/people/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api,id)
    returnId = NULL

    printIfVerbose("cppr_updatePerson: path=",path,id,list_of_fields)

    raw.result <- httr::PUT(url = .pkgglobalenv$cppr_Url, path = path, 
                      httr::user_agent(.pkgglobalenv$cppr_Ua),
                      httr::add_headers(
                        `X-PW-AccessToken` = .pkgglobalenv$cppr_AccessToken,
                        `X-PW-Application` = "developer_api",
                        `X-PW-UserEmail` = .pkgglobalenv$cppr_UserEmail,
                        `Content-Type` = "application/json"),
                      httr::authenticate(.pkgglobalenv$cppr_AccessToken,"",type="basic"),
                      body = list_of_fields,
                      encode = "json")
    if (httr::http_type(raw.result) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    raw.content <- rawToChar(raw.result$content)
    result <- jsonlite::fromJSON(raw.content, flatten=TRUE)

    if(raw.result$status_code == 200){
        returnId = result$id
        printIfVerbose("cppr_updatePerson: Successfully updated Person",returnId)
    } else{
        if(  raw.result$status_code == 404 ) {
          printIfVerbose("cppr_updatePerson Failed. status code 404: Not found")
        } else if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_updatePerson Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_updatePerson Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_updatePerson Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_updatePerson Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_updatePerson(%d): API request failed. HTTP Status code %s: %s", 
              id, result$status, result$message), call. = FALSE)
    }   
    return(returnId)
}


# ----------------------------------------------------------------------------
#' cppr_createNewPerson
#'
#' Create a new record in the People object using the fields passed in the argument
#' @param person list - a list of fields for the new Person.\cr 
#' Field        Type    Details\cr
#' id           number  Unique identifier \cr
#' name*        string  firstname lastname\cr
#' address      address An encapsulation of the Person's street, city, state, postal code, and country.\cr
#' assignee_id  number Unique identifier of the User that will be the owner of the Person.\cr
#' etc
#' @return the numeric id of the new Person if successful, or NULL 
#' @importFrom jsonlite fromJSON 
#' @importFrom httr POST content authenticate add_headers http_type user_agent
#' @seealso \code{\link{cppr_getPeople}} for full list of Person fields
#' @seealso \code{\link{cppr_getPersonId}} to find the Person Id if you know their name
#' @family People functions
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @examples 
#' \dontrun{ 
#' id <- cppr_getPersonId("John Smith")
#' if( nrow(id) == 1 ) # found exactly one matching record in Copper
#'   person <- cppr_createNewPerson(id)
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_createNewPerson <- function(person) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.list(person))

    api <- "/people/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api)

    printIfVerbose("cppr_createNewPerson:",path, person$name)

    returnId <- NULL

    raw.result <- httr::POST(url = .pkgglobalenv$cppr_Url, path = path, 
                      httr::user_agent(.pkgglobalenv$cppr_Ua),
                      httr::add_headers(
                        `X-PW-AccessToken` = .pkgglobalenv$cppr_AccessToken,
                        `X-PW-Application` = "developer_api",
                        `X-PW-UserEmail` = .pkgglobalenv$cppr_UserEmail,
                        `Content-Type` = "application/json"),
                      httr::authenticate(.pkgglobalenv$cppr_AccessToken,"",type="basic"),
                      body= person,
                      encode = "json")
    if (httr::http_type(raw.result) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    raw.content <- rawToChar(raw.result$content)
    result <- jsonlite::fromJSON(raw.content, flatten=TRUE)

    if(raw.result$status_code == 200){
          returnId <- result$id
          printIfVerbose("cppr_createNewPerson: successfully created new Person with Id",returnId)
    } else{
        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_createNewPerson Failed. status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_createNewPerson Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_createNewPerson Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_createNewPerson Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_createNewPerson(): API request failed. HTTP Status code %s: %s", 
              result$status, result$message), call. = FALSE)
    }   
    return(returnId)
}

# ----------------------------------------------------------------------------
#' cppr_deletePerson
#'
#' Permanently remove an Person record identified by the the id passed 
#' as argument
#'
#' @param id numeric - the Id of the Person to be updated
#'
#' @return the id of the Person if update was successful, else NULL
#' @importFrom jsonlite fromJSON 
#' @importFrom httr DELETE content authenticate add_headers http_type user_agent
#' @family People functions
#' @seealso For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}
#' @examples 
#' \dontrun{ 
#' if(!is.null(cppr_deletePerson(id))) 
#'   sprintf("Person %d successfully removed",id)
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_deletePerson <- function(id) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.numeric(id))

    api <- "/people/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api,id)
    returnId = NULL

    printIfVerbose("cppr_deletePerson: path=",path)

    if(is.null(cppr_getPersonById(id))){
      printIfVerbose("cppr_deletePerson: Warning - no Person found with the id",id)
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
        printIfVerbose("cppr_deletePerson: Successfully removed Person",returnId)
    } else{
        if(  raw.result$status_code == 404 ) {
          printIfVerbose("cppr_deletePerson Failed. status code 404: Not found")
        } else if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_deletePerson Failed: status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_deletePerson Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_deletePerson Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_deletePerson Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_deletePerson(%d): API request failed. HTTP Status code %s: %s", 
              id, result$status, result$message), call. = FALSE)
    }   
    return(returnId)
}


