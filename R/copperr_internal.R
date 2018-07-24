##############################################################################
# copperr - A Copper API wrapper package
#
# author Robert Drummond
# (C) 2017,2018 http://www.robert-drummond.com
##############################################################################

# ----------------------------------------------------------------------------
## printIfVerbose
##
## prints arguments if the verbose flag has been set to TRUE
## a private function within this package
##
## @param x a value you are asserting is something
## 
## printIfVerbose("the value of x is",x)
## @keywords internal
# ----------------------------------------------------------------------------
printIfVerbose <- function(x, ...){
  if(!is.null(.pkgglobalenv))
    if(.pkgglobalenv$verboseFlag)
      print(paste(x, ...))
}

# ----------------------------------------------------------------------------
## pkg_is_initialized
##
## a test for use with the stopifnot() assertion 
## a private function within this package
##
## @param x a value you are asserting is something
## 
## @return TRUE if environment is initialized. 
## @keywords internal
# ----------------------------------------------------------------------------
pkg_is_initialized <- function(){
  if(!is.null(.pkgglobalenv))
    if(!is.null(.pkgglobalenv$cppr_Endpoint))
    	return(TRUE)
   return(FALSE)
}
# calling stopifnot(pkg_is_initialized())
# prints "Error: pkg_is_initialized() is not TRUE"
# so modify the error msg to be more informative
#on_failure(pkg_is_initialized) <- function(call, env) {
#  return("package copperr has not been initialized. Call copperr::initializePw() first")
#}
# now stopifnot(pkg_is_initialized())
# will print "Error: package copperr has not been initialized. Call initializePw() first"


