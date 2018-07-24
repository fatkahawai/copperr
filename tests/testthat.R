library(testthat)
library(copperr)
library(config)

dw <- config::get("copper") 
cppr_initialize(dw$uid,dw$api_key)

test_check("copperr")
