#' copperr: A package for accessing the API for the copper CRM
#'
#' The copperr package provides five categories of important functions:
#' Reference data functions, Opportunity functions, Company functions, Lead functions, 
#' People functions
#' 
#' @references [\url{https://developer.prosperworks.com/#intro}]
#'
#' @section User functions:
#' The User functions are used to access the profiles of Users registered in PW 
#'
#' @section Resource functions:
#' The Resource functions are used to access custom fields options and other 
#' static reference data defined in PW 
#'
#' @section Opportunity functions:
#' The Opportunity functions are used to get individual, multiple or all records,
#' to update records, and to create new records
#'
#' @section Company functions:
#' The Company functions functions are used to get individual, multiple or all records,
#' to update records, and to create new records
#'
#' @section Lead functions:
#' The Lead functions functions are used to get individual, multiple or all records,
#' to update records, and to create new records
#'
#' @section People functions:
#' The People functions functions are used to get individual, multiple or all records,
#' to update records, and to create new records, and to get other records related to a 
#' Person
#'
#' @docType package
#' @name copperr
#'
#' @importFrom utils str
#' @importFrom httr GET PUT POST DELETE authenticate add_headers http_type
#' @importFrom jsonlite fromJSON
#'
#' @author Robert Drummond
#' (C) 2017,2018 http://www.robert-drummond.com

NULL
