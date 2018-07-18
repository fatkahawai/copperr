context("Lead functions")
#library()

dw <- config::get("copper") 
cppr_initialize(dw$uid,dw$api_key)

# clean up any test data from previous test runs
artifactLd <- cppr_getLeads(tags="test_copperr")
if(nrow(artifactLd) > 0 ){
  cppr_deleteLead(artifactLd$id)
}

testLeadId <- cppr_createNewLead( list( 
                        `name` = "Test Lead", 
                        `address` = list(`city`="Auckland", `country` = "NZ"), 
                        `tags` = list("test_copperr"), 
                        `email` = list(`email`="test.lead@testcompany.com", `category`="work")
                        ))

test_that("cppr_createNewLead() with a list containing all mandatory fields should return a non-Null numeric id result", {
  expect_failure(expect_null( testLeadId ))
#  expect_null(cppr_createNewLead(list( NULL))) # will get a 500 response
})

test_that("cppr_getLeads(tags=\"test_copperr\") should return a non-NULL result", {
  if(!is.null(testLeadId)){
    expect_failure(expect_null( cppr_getLeads(tags="test_copperr")))
  }
})

test_that("cppr_updateLead() with a valid id for an existing Lead should return a non-Null numeric id result", {
  if(!is.null(testLeadId)){
    expect_failure(expect_null( cppr_updateLead(testLeadId,list(`details`="updated"))))
  }
#  expect_null(cppr_deleteLead(-1)) # will get a 500 response
})

test_that("cppr_deleteLead() with a valid id for an existing Lead should return a non-Null numeric id result", {
  if(!is.null(testLeadId)){
    expect_failure(expect_null( cppr_deleteLead(testLeadId)))
  }
#  expect_null(cppr_deleteLead(-1)) # will get a 500 response
})

