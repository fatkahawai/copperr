context("Opportunity functions")
#library()


# clean up any test data from previous test runs
artifactOp <- cppr_getOpps(tags="test_copperr")
if(nrow(artifactOp) > 0 ){
  cppr_deleteOpp(artifactOp$id[1])
}

# create the test case reference data we need

# first check if test data exists from previous test runs. if not, create it
artifactPp <- cppr_getPeople(tags="test_copperr")
if(nrow(artifactPp) > 0 ){
  refPersonId <- artifactPp$id[1]
} else {

  refPersonId <- cppr_createNewPerson( list( 
                                      `name` = "Test Person 2", 
                                      `tags` = list("test_copperr"), 
                                      `emails` = list(list(`email`="test.person2@testcompany2.com", `category`="work"))
                                      ))
}

artifactCo <- cppr_getCompanies(tags="test_copperr")
if(nrow(artifactCo) > 0 ){
  refCompanyId <- artifactCo$id[1]
} else {
  refCompanyId <- cppr_createNewCompany( list( 
                                      `name` = "Test Company 2",
                                      `country` = "NZ", 
                                      `tags` = list("test_copperr"), 
                                      `email domain` = "testcompany2.com"
                                      ))
}

testOppId <- cppr_createNewOpp( list( 
                                      `name` = "Test Opp", 
                                      `company_name` = refCompanyId, 
                                      `tags` = list("test_copperr"), 
                                      `primary_contact_id` = refPersonId,
                                      `country` = "NZ"))

test_that("cppr_createNewOpp() with a list containing all mandatory fields should return a non-Null numeric id result", {
  expect_failure(expect_null(testOppId))
#  expect_null(cppr_createNewOpp(list( NULL)))
})

test_that("cppr_getOppByName() should return a non-NULL, non-empty result if that opportunity exists", {
  if(!is.null(testOppId)){
    opp <- cppr_getOppByName("Test Opp")
    expect_failure(expect_null( opp))
#    expect_gt(length( opp),0)  
  }
  expect_equal(length(cppr_getOppByName("A guaranteed non-existent opportunity name")),0)
})

test_that("cppr_getOppById() should return a non-NULL, non-empty result if that Id exists", {
  if(!is.null(testOppId)){
    expect_failure(expect_null( cppr_getOppById(testOppId)))
    expect_gt(length( cppr_getOppById(testOppId)),0)
  }
#  expect_equal(length(cppr_getOppById(-1)),0) # will get a 500 response
})

test_that("cppr_getOpps(country=\"NZ\") should return a non-NULL result", {
  if(!is.null(testOppId)){
    expect_failure(expect_null( cppr_getOpps(tags="test_copperr")))
    expect_gt(length( cppr_getOpps(tags="test_copperr")),0)
  }
})

test_that("cppr_updateOpp() with a valid id and details field should return a non-Null numeric id result", {
  if(!is.null(testOppId)){
    expect_failure(expect_null(cppr_updateOpp(testOppId, list( `details` = "updated"))))
#    expect_null(cppr_updateOpp(-1, list( `details` = "updated")))
  }
})

test_that("cppr_deleteOpp() with a valid id for an existing Opp should return a non-Null numeric id result", {
  if(!is.null(testOppId)){
    expect_failure(expect_null(cppr_deleteOpp(testOppId)))
  }
#  expect_null(cppr_deleteOpp(-1)) # will get a 500 response
})

# remove test case data created earlier
if(!is.null(refPersonId))
  cppr_deletePerson(refPersonId)

if(!is.null(refCompanyId))
  cppr_deleteCompany(refCompanyId)


