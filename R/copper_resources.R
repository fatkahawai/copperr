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
#' cppr_getContactTypes 
#'
#' get a table of Contact Types defined in Copper (e.g. "Existing Customer")
#' for use in the corresponding field in Company records
#'
#' @return a data frame, or NULL on error
#' Field Type    Details
#' id    number  Unique identifier for the Contact Type.
#' name  string  The name of the Contact Type.
#' @importFrom jsonlite fromJSON 
#' @importFrom httr GET content authenticate add_headers http_type user_agent
#' @family Resource functions
#' @examples
#' \dontrun{ 
#' ct <- cppr_getContactTypes()
#' print(paste("the contact types defined in Copper are", ct$name))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getContactTypes <- function() {
  stopifnot(pkg_is_initialized())    

    api <- "/contact_types/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api)

    printIfVerbose("cppr_getContactTypes:",path)

    contactTypes <- NULL

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
          printIfVerbose("cppr_getContactTypes: no ContactType records found in Copper")
          contactTypes <- as.data.frame(NULL) # return an empty data frame
        } else {
          printIfVerbose("cppr_getContactTypes result: ",result)
          contactTypes <- result
        } # else result >0
    } else{
        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getContactTypes Failed. status code 400: Bad Request")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getContactTypes Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getContactTypes Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getContactTypes(): API request failed. HTTP Status code %s: %s", 
              result$status, result$message), call. = FALSE)

    }   
    return(contactTypes)
}

# ----------------------------------------------------------------------------
#' cppr_getCustomerSources 
#'
#' get a table of Customer Sources defined in Copper (e.g. "Inbound", etc)
#' for use in the corresponding field in Opportunity records
#'
#' @return a data frame, or NULL on error
#' Field Type    Details
#' id    number  Unique identifier for the Contact Type.
#' name  string  The name of the Contact Type.
#' @importFrom jsonlite fromJSON 
#' @importFrom httr GET content authenticate add_headers http_type user_agent
#' @family Resource functions
#' @examples
#' \dontrun{ 
#' ct <- cppr_getCustomerSources()
#' print(paste("the customer sources defined in Copper are", ct$name))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getCustomerSources <- function() {
  stopifnot(pkg_is_initialized())    

    api <- "/customer_sources/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api)

    printIfVerbose("cppr_getCustomerSources:",path)

    sources <- NULL

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
          printIfVerbose("cppr_getCustomerSources: no Customer Sources records found in Copper")
          sources <- as.data.frame(NULL) # return an empty data frame
        } else {
          printIfVerbose("cppr_getCustomerSources result: ",result)
          sources <- result
        } # else result >0
    } else{
        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getCustomerSources Failed. status code 400: Bad Request")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getCustomerSources Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getCustomerSources Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getCustomerSources(): API request failed. HTTP Status code %s: %s", 
              result$status, result$message), call. = FALSE)

    }   
    return(sources)
}


# ----------------------------------------------------------------------------
#' cppr_getCustomerSourceId 
#'
#' get the Id for a named Customer Source (e.g. "Inbound")
#'
#' @param source_label - the name given to an option in the dropdown, e.g. "outbound"
#' get Id for a Source label
#' @return a numeric Id
#' @family Resource functions
#' @examples
#' \dontrun{ 
#' id <- cppr_getCustomerSourceId("Inbound")
#' print(paste("the id for 'Inbound' source is", id))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getCustomerSourceId <- function(source_label){
  stopifnot(pkg_is_initialized())    
  stopifnot(is.character(source_label))

  srcId <- NULL
  sourcesList <- cppr_getCustomerSources()
  if(!is.null(sourcesList)) {
    id <- sourcesList$id[sourcesList$name==source_label]
    if(length(id)==1)
      srcId <- id
  }
  return(srcId)
}

#----------------------------------------------------------------------------
#' cppr_getCustomFieldDefinitions
#'
#' get a table of defintions for your custom fields
#'
#' @return a data frame, or NULL if not found
#' Field     Type          Details
#' id        number        Unique identifier for the custom field definition.
#' name      string        Label for the custom field definition
#' data_type string enum   The type of data that should be stored within this custom field. 
#'                         Possible values are: String, Text, Dropdown, Date, Checkbox, Float, 
#'                         Url, Percentage, Currency
#' currency  string enum   The currency used for this custom field definition. Valid only when 
#'                         the data type is Currency.
#' options   options array A list of possible dropdown options. Valid only when the data type is Dropdown.
#' @importFrom utils str
#' @importFrom jsonlite fromJSON 
#' @importFrom httr GET content authenticate add_headers http_type user_agent
#' @family Resource functions
#' @examples
#' \dontrun{ 
#' customFields <- cppr_getCustomFieldDefinitions()
#' print(paste("there are", nrow(customFields),"custom fields defined in Copper"))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getCustomFieldDefinitions <- function() {
  stopifnot(pkg_is_initialized())    

    api <- "/custom_field_definitions/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api)

    printIfVerbose("cppr_getCustomFieldDefinitions:",path)

    defs <- NULL

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
          printIfVerbose("cppr_getCustomFieldDefinitions: no Custom Field Definitions found in Copper")
          defs <- as.data.frame(NULL) # return an empty data frame
        } else {
          printIfVerbose("cppr_getCustomFieldDefinitions result: ",str(result))
          defs <- result
          # cache table in local environment
          assign("cppr_CustomFieldDefs", defs, envir=.pkgglobalenv)
        } # else result >0
    } else{
        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getCustomFieldDefinitions Failed. status code 400: Bad Request")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getCustomFieldDefinitions Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getCustomFieldDefinitions Failed. status code", raw.result$status_code)
        }
        stop( sprintf("cppr_getCustomFieldDefinitions(): API request failed. HTTP Status code %s: %s", 
              result$status, result$message), call. = FALSE)
    }   
    return(defs)
}

# ----------------------------------------------------------------------------
#' cppr_getCustomFieldDatatype
#' 
#' get the data type for a named custom field
#'
#' @param field_name - the name text of the custom field, e.g. "Vertical"
#' @note on first call, stores list of custom field definitions in a global var 
#' to avoid unnecessary subsequent API calls
#' @return string - the datatype, or NULL if not found
#' data_type string enum   The type of data that should be stored within this custom field. 
#'                         Possible values are: String, Text, Dropdown, Date, Checkbox, Float, 
#'                         Url, Percentage, Currency
#' @family Resource functions
#' @examples
#' \dontrun{ 
#' type <- cppr_getCustomFieldDatatype("Industry")
#' print(paste("the data type for the Industry custom field is",type))
#' }
#' @export
# ----------------------------------------------------------------------------

cppr_getCustomFieldDatatype <- function(field_name){

  stopifnot(pkg_is_initialized())    
  stopifnot(is.character(field_name))

  optDatatype <- NULL
  # if definitions are not cached in the local environment yet, get and cache them 
  if(is.null(.pkgglobalenv$cppr_CustomFieldDefs)){
    cppr_getCustomFieldDefinitions()
  }

  type <- .pkgglobalenv$cppr_CustomFieldDefs$data_type[.pkgglobalenv$cppr_CustomFieldDefs$name==field_name]
  if(length(type)==1){
    optDatatype <- type
  }
  return(optDatatype)
}

# ----------------------------------------------------------------------------
#' cppr_getCustomFieldId
#' 
#' get the Id of a named custom fields
#'
#' @param field_name - the name text of the custom field, e.g. "Vertical"
#' @return the Id of that custom field, or NULL if not found
#' @note on the first call to this function, a list of custom field definitions is 
#' stored in a local environment for performance - to avoid unnecessary subsequent API calls
#' @family Resource functions
#' @examples
#' \dontrun{ 
#' id <- cppr_getCustomFieldId("Industry")
#' print(paste("the Id for the Industry custom field is",id))
#' }
#' @export
# ----------------------------------------------------------------------------

cppr_getCustomFieldId <- function(field_name){

  stopifnot(pkg_is_initialized())    
  stopifnot(is.character(field_name))

  fieldId <- NULL
  # if definitions are not cached in the local environment yet, get and cache them 
  if(is.null(.pkgglobalenv$cppr_CustomFieldDefs)){
    cppr_getCustomFieldDefinitions()
  }

  id <- .pkgglobalenv$cppr_CustomFieldDefs$id[.pkgglobalenv$cppr_CustomFieldDefs$name==field_name]
  if(length(id)==1){
    fieldId <- id
  }
  return(fieldId)
}
# ----------------------------------------------------------------------------
#' cppr_getCustomFieldName
#' 
#' get the name of a Custom Field by Id 
#'
#' @param field_id - the idof the custom field, e.g. 127800
#' @note stores list of custom field definitions in a global env 
#' to avoid unnecessary subsequent API calls
#' @return the Id of that custom field, or NULL if not found
#' @family Resource functions
#' @examples
#' \dontrun{ 
#' name <- cppr_getCustomFieldName(20278)
#' print(name)
#' }
#' @export
# ----------------------------------------------------------------------------

cppr_getCustomFieldName <- function(field_id){

  stopifnot(pkg_is_initialized())    
  stopifnot(is.numeric(field_id))

  field_name <- NULL
  # if definitions are not cached in the local environment yet, get and cache them 
  if(is.null(.pkgglobalenv$cppr_CustomFieldDefs)){
    cppr_getCustomFieldDefinitions()
  }

  name <- .pkgglobalenv$cppr_CustomFieldDefs$name[.pkgglobalenv$cppr_CustomFieldDefs$id==field_id]
  if(length(name)==1){
    field_name <- name
  }
  return(field_name)
}
# ----------------------------------------------------------------------------
#' cppr_getCustomFieldOptionId
#' 
#' get the Id of a named Custom Field Option for a custom field of dropdown or 
#' MultiSelect type.
#' Each dropdown option you define in Copper for a custom field is given an Id by Copper
#'
#' @param field_name - the name text of the custom field, e.g. "Vertical"
#' @param option_label - the label text of the option , e.g. "Transport"
#' @note on first call, stores list of custom field definitions in a global env 
#' to avoid unnecessary subsequent API calls
#' @return the Id number of the option label, or NULL if not found
#' @family Resource functions
#' @examples
#' \dontrun{ 
#' id <- cppr_getCustomFieldOptionId("Industry","Finance")
#' }
#' @export
# ----------------------------------------------------------------------------

cppr_getCustomFieldOptionId <- function(field_name, option_label){

  stopifnot(pkg_is_initialized())    
  stopifnot(is.character(field_name))
  stopifnot(is.character(option_label))

  optId <- NULL
  # if definitions are not cached in the local environment yet, get and cache them 
  if(is.null(.pkgglobalenv$cppr_CustomFieldDefs)){
    cppr_getCustomFieldDefinitions()
  }

  optlist <- .pkgglobalenv$cppr_CustomFieldDefs$options[.pkgglobalenv$cppr_CustomFieldDefs$name==field_name]
  id <- optlist[[1]]$id[optlist[[1]]$name==option_label]
  if(length(id)==1){
    optId <- id
  }
  return(optId)
}

# ----------------------------------------------------------------------------
#' cppr_getCustomFieldOptionName
#' 
#' get the name of a Custom Field Option by Id, for a custom field of dropdown type.
#' Each dropdown option you define in Copper for a custom field is given an Id by Copper
#'
#' @param field_name - the name text of the custom field, e.g. "Vertical"
#' @param option_id - the Id of the option , e.g. 230189
#' @note on first call, stores list of custom field definitions in a global env 
#' to avoid unnecessary subsequent API calls
#' @return the text label for the option, or NULL if not found
#' @family Resource functions
#' @examples
#' \dontrun{ 
#' name <- cppr_getCustomFieldOptionName("Industry",202193)
#' print(paste("The dropdown option 202193 is displayed as",name)
#' }
#' @export
# ----------------------------------------------------------------------------

cppr_getCustomFieldOptionName <- function(field_name, option_id){

  stopifnot(pkg_is_initialized())    
  stopifnot(is.numeric(option_id))
  stopifnot(is.character(field_name))

  optionName <- NULL
  # if definitions are not cached in the local environment yet, get and cache them 
  if(is.null(.pkgglobalenv$cppr_CustomFieldDefs)){
    cppr_getCustomFieldDefinitions()
  }

  optionsList <- .pkgglobalenv$cppr_CustomFieldDefs$options[.pkgglobalenv$cppr_CustomFieldDefs$name==field_name]
  if(length(optionsList)>0){
    name <- optionsList[[1]]$name[optionsList[[1]]$id==option_id]
    if(length(name)==1){
      optionName <- name
    }
  }
  return(optionName)
}

# ----------------------------------------------------------------------------
#' cppr_getCustomFieldOptionsAll
#' 
#' get a list of all Custom Field Options for a named Custom Field of dropdown type
#' Each dropdown option you define in Copper for a custom field is given an Id by Copper
#'
#' @param field_name - the name text of the custom field, e.g. "Vertical"
#' @note on first call, stores list of custom field definitions in a global env 
#' to avoid unnecessary subsequent API calls
#' @return a data frame of options, or NULL if not found
#' @family Resource functions
#' @examples
#' \dontrun{ 
#' options <- cppr_getCustomFieldOptionsAll("Industry")
#' print("the options in the list are called",options$name)
#' }
#' @export
# ----------------------------------------------------------------------------

cppr_getCustomFieldOptionsAll <- function(field_name){

  stopifnot(pkg_is_initialized())    
  stopifnot(is.character(field_name))

  options <- NULL
  # if definitions are not cached in the local environment yet, get and cache them 
  if(is.null(.pkgglobalenv$cppr_CustomFieldDefs)){
    cppr_getCustomFieldDefinitions()
  }

  list <- .pkgglobalenv$cppr_CustomFieldDefs$options[.pkgglobalenv$cppr_CustomFieldDefs$name==field_name]
  if(length(list)==1){
    options <- as.data.frame(list)
  }
  return(options)
}

# ----------------------------------------------------------------------------
#' cppr_getPipelines
#'
#' get a table containing the definitions of the stages defined for a specified Pipeline
#' Opportunity records can be assigned to a Pipeline in Copper
#'
#' @return a data frame, or NULL if none found
#' Field                     Details
#' id (number)               Unique identifier for the Pipeline
#' name (string)             The name of the Pipeline.
#' stages (list)             The Stages in this pipeline.
#' win_probability (number)  The expected probability of winning an Opportunity in this Pipeline Stage. Valid values are [0-100] (inclusive).#
#' @importFrom utils str
#' @importFrom jsonlite fromJSON 
#' @importFrom httr GET content authenticate add_headers http_type user_agent
#' @family Resource functions
#' @examples 
#' \dontrun{ 
#' pipelines <- cppr_getPipelines()
#' print(paste("the pipeline Ids are ",pipelines$name))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getPipelines <- function() {

    stopifnot(pkg_is_initialized())    

    api <- "/pipelines/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api)

    printIfVerbose("cppr_getPipelines:",path)

    pipelines<- as.data.frame(NULL)

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
          printIfVerbose("cppr_getPipelines: no defined pipelines found in Copper")
        } else {
          printIfVerbose("cppr_getPipelines result: ",str(result))
          pipelines <- result
        } # else result >0
    } else{
        pipelines <- NULL # set error return code

        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getPipelines Failed. status code 400: Bad Request")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getPipelines Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getPipelines Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getPipelines(): API request failed. HTTP Status code %s: %s", 
              result$status, result$message), call. = FALSE)
    }   
    return(pipelines)
}

# ----------------------------------------------------------------------------
#' cppr_getPipelineStages
#'
#' get a table containing the definitions of the stages defined for a specified Pipeline
#' for use in the corresponding field in Opportunity records
#'
#' @param pipeline_id numeric - the Copper Id of the pipeline you want information on
#'
#' @return a data frame, or NULL on error
#' Field                     Details
#' id (number)               Unique identifier for the Pipeline Stage.
#' name (string)             The name of the Pipeline Stage.
#' pipeline_id (number)      The unique identifier of the Pipeline in which this Pipeline Stage is.
#' win_probability (number)  The expected probability of winning an Opportunity in this Pipeline Stage. Valid values are [0-100] (inclusive).#
#' @importFrom utils str
#' @importFrom jsonlite fromJSON 
#' @importFrom httr GET content authenticate add_headers http_type user_agent
#' @family Resource functions
#' @examples 
#' \dontrun{ 
#' stages <- cppr_getPipelineStages(12345)
#' print(paste("the stages are called",stages$name))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getPipelineStages <- function(pipeline_id) {

    stopifnot(pkg_is_initialized())    
    stopifnot(is.numeric(pipeline_id))

    api <- "/pipeline_stages/pipeline/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api,pipeline_id)

    printIfVerbose("cppr_getPipelineStages:",path)

    stages <- as.data.frame(NULL)

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
    printIfVerbose("cppr_getPipelineStages str(raw content):",str(raw.content))

    result <- jsonlite::fromJSON(raw.content, flatten=TRUE)
    printIfVerbose("cppr_getPipelineStages result: ",str(result))
    
    if(raw.result$status_code == 200){
        if( length(result)==0 ){
          printIfVerbose("cppr_getPipelineStages: no defined Stages found in Copper")
          stages <- as.data.frame(NULL)
        } else {
          printIfVerbose("cppr_getPipelineStages result: ",str(result))
          stages <- result
        } # else result >0
    } else{
        stages <- NULL # set error return code

        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getPipelineStages Failed. status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_getPipelineStages Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getPipelineStages Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getPipelineStages Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getPipelineStages(%d): API request failed. HTTP Status code %s: %s", 
              pipeline_id, result$status, result$message), call. = FALSE)
    }   
    return(stages)
}

# ----------------------------------------------------------------------------
#' cppr_getPipelineStageId
#'
#' Get a Copper Id for the stage record identfied by the name argument
#' for use in the corresponding field in Opportunity records
#' @param pipeline_id numeric - the Copper Id of the pipeline you want information on
#' @param name string - the label (name) of the stage, e.g. "Quote sent"
#' @return an numeric Id. The first matching Copper Id, or NULL if no match found or on error.
#' @family Resource functions
#' @examples
#' \dontrun{ 
#' pipes <- cppr_getPipelines() 
#' id <- cppr_getPipelineStageId(pipes$id[1],"Quote sent")
#' if(!is.null(id)) print(paste("stage 'Quote sent' has the id",id))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getPipelineStageId <- function(pipeline_id,name) {

  stopifnot(pkg_is_initialized())    
  stopifnot(is.numeric(pipeline_id))
  stopifnot(is.character(name))

  id <- NULL
  stages <- cppr_getPipelineStages(pipeline_id)

  if(!is.null(stages)){
    if( length(stages)>0)
      id <-  stages$id[stages$pipeline_id==pipeline_id & stages$name==name]
  }
  return(id)
}

# ----------------------------------------------------------------------------
#' cppr_getLeadStatuses
#'
#' get a table containing the definitions of the Lead Statuses defined in Copper
#' for use in the corresponding field in Lead records
#'
#' @return a data frame, or NULL on error
#' Field                     Details
#' id (number)               Unique identifier for the Pipeline Stage.
#' name (string)             The name of the Pipeline Stage.
#' order (number)            The position it is displayed in the list
#' is_default (boolean)      indicates if this value is the default for new records
#' @importFrom utils str
#' @importFrom jsonlite fromJSON 
#' @importFrom httr GET content authenticate add_headers http_type user_agent
#' @family Resource functions
#' @examples 
#' \dontrun{ 
#' statuses <- cppr_getLeadStatuses()
#' print(paste("the statuses are called",statuses$name))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getLeadStatuses <- function() {

    stopifnot(pkg_is_initialized())    

    api <- "/lead_statuses/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api)

    printIfVerbose("cppr_getLeadStatuses:",path)

    statuses <- as.data.frame(NULL)

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
    printIfVerbose("cppr_getLeadStatuses str(raw content):",str(raw.content))

    result <- jsonlite::fromJSON(raw.content, flatten=TRUE)
    printIfVerbose("cppr_getLeadStatuses result: ",str(result))
    
    if(raw.result$status_code == 200){
        if( length(result)==0 ){
          printIfVerbose("cppr_getLeadStatuses: no defined statuses found in Copper")
          statuses <- as.data.frame(NULL)
        } else {
          printIfVerbose("cppr_getLeadStatuses result: ",str(result))
          statuses <- result
        } # else result >0
    } else{
        statuses <- NULL # set error return code

        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getLeadStatuses Failed. status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_getLeadStatuses Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getLeadStatuses Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getLeadStatuses Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getLeadStatuses(%d): API request failed. HTTP Status code %s: %s", 
              pipeline_id, result$status, result$message), call. = FALSE)
    }   
    return(statuses)
}

# ----------------------------------------------------------------------------
#' cppr_getLeadStatusId
#'
#' Get a Copper Id for the Status  identfied by the name argument
#' @param name string - the label (name) of the status, e.g. "Qualified"
#' @return an numeric Id. The first matching Copper Id, or NULL if not found.
#' @family Resource functions
#' @examples
#' \dontrun{ 
#' pipes <- cppr_getLeadStatuses() 
#' id <- cppr_getLeadStatusId(pipes$id[1],"Qualified")
#' if(!is.null(id)) 
#'   print(paste("stage 'Qualified' has the id",id))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getLeadStatusId <- function(name) {

  stopifnot(pkg_is_initialized())    
  stopifnot(is.character(name))

  id <- NULL
  statuses <- cppr_getLeadStatuses()

  if(!is.null(statuses)){
    if( length(statuses)>0)
      id <-  statuses$id[statuses$name==name]
  }
  return(id)
}


# ----------------------------------------------------------------------------
#' cppr_getLossReasons
#'
#' get a table containing the definitions of the Loss Reasons defined
#' for use in the corresponding field in Opportunity records
#'
#' @return a data frame, or NULL on error
#' Field                     Details
#' id (number)               Unique identifier for the Pipeline Stage.
#' name (string)             The name of the Pipeline Stage.
#' order (number)            The position it is displayed in the list
#' is_default (boolean)      indicates if this value is the default for new records
#' @importFrom utils str
#' @importFrom jsonlite fromJSON 
#' @importFrom httr GET content authenticate add_headers http_type user_agent
#' @family Resource functions
#' @examples 
#' \dontrun{ 
#' statuses <- cppr_getLossReasons()
#' print(paste("the statuses are called",statuses$name))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getLossReasons <- function() {

    stopifnot(pkg_is_initialized())    

    api <- "/loss_reasons/"
    path <- paste0(.pkgglobalenv$cppr_Endpoint,api)

    printIfVerbose("cppr_getLossReasons:",path)

    reasons <- as.data.frame(NULL)

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
    printIfVerbose("cppr_getLossReasons str(raw content):",str(raw.content))

    result <- jsonlite::fromJSON(raw.content, flatten=TRUE)
    printIfVerbose("cppr_getLossReasons result: ",str(result))
    
    if(raw.result$status_code == 200){
        if( length(result)==0 ){
          printIfVerbose("cppr_getLossReasons: no defined loss reasons found in Copper")
          reasons <- as.data.frame(NULL)
        } else {
          printIfVerbose("cppr_getLossReasons result: ",str(result))
          reasons <- result
        } # else result >0
    } else{
        reasons <- NULL # set error return code

        if(  raw.result$status_code == 400 ) {
          printIfVerbose("cppr_getLossReasons Failed. status code 400: Bad Request")
        } else if(  raw.result$status_code == 422 ) {
          printIfVerbose("cppr_getLossReasons Failed. status code 422: invalid argument")
        }else if(  raw.result$status_code == 429 ) {
          printIfVerbose("cppr_getLossReasons Failed. status code 429: Too Many Requests")
        }else{
          printIfVerbose("cppr_getLossReasons Failed. status code", raw.result$status_code )
        }
        stop( sprintf("cppr_getLossReasons(%d): API request failed. HTTP Status code %s: %s", 
              pipeline_id, result$status, result$message), call. = FALSE)
    }   
    return(reasons)
}

# ----------------------------------------------------------------------------
#' cppr_getLossReasonId
#'
#' Get a Copper Id for the Loss Reason identfied by the name argument
#' @param name string - the label (name) of the status, e.g. "Budget"
#' @return an numeric Id. The first matching Copper Id, or NULL if not found.
#' @family Resource functions
#' @examples
#' \dontrun{ 
#' pipes <- cppr_getLossReasons() 
#' id <- cppr_getLossReasonId(pipes$id[1],"Budget")
#' if(!is.null(id)) 
#'   print(paste("stage 'Budget' has the id",id))
#' }
#' @export
# ----------------------------------------------------------------------------
cppr_getLossReasonId <- function(name) {

  stopifnot(pkg_is_initialized())    
  stopifnot(is.character(name))

  id <- NULL
  reasons <- cppr_getLossReasons()

  if(!is.null(reasons)){
    if( length(reasons)>0)
      id <-  reasons$id[reasons$name==name]
  }
  return(id)
}


