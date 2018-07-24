context("People functions")
#library()

# clean up any test data from previous test runs
artifactPp <- cppr_getPeople(tags="test_copperr")
if(nrow(artifactPp) > 0 ){
  cppr_deletePerson(artifactPp$id[1])
}

testPersonId <- cppr_createNewPerson(list( 
                                      `name` = "Test Person", 
                                      `address` = list(`city`="Auckland", `country` = "NZ"), 
                                      `tags` = list("test_copperr"), 
                                      `emails` = list(list(`email`="test.person@testcompany.com", `category`="work"))
                                    ))

test_that("cppr_createNewPerson() with a list containing all mandatory fields should return a non-Null numeric id result", {
  expect_failure(expect_null(testPersonId))
#  expect_null(cppr_createNewPerson(list( NULL))) # will get a 500 response
})

test_that("cppr_getPersonByName() should return a non-NULL, non-empty result if that person exists", {
  if(!is.null(testPersonId)){
    expect_failure(expect_null( cppr_getPersonByName("Test Person")))
  }
  expect_equal(length(cppr_getPersonByName("A guaranteed non-existent person name")),0) 
})

test_that("cppr_getPersonId() should return a non-NULL, >0 numeric result if that person exists", {
  if(!is.null(testPersonId)){
    expect_failure(expect_null( cppr_getPersonId("Test Person")))
#    expect_gt( cppr_getPersonId("Test Person"),0)
  }
  expect_equal(cppr_getPersonId("A guaranteed non-existent person name"),0)
})

test_that("cppr_getPersonById() should return a non-NULL, non-empty result if that Id exists", {
  if(!is.null(testPersonId)){
    expect_failure(expect_null( cppr_getPersonById(testPersonId)))
    expect_gt(length( cppr_getPersonById(testPersonId)),0)
  }
})

test_that("cppr_getPersonByEmail() should return a non-NULL, non-empty  result if that person exists", {
  if(!is.null(testPersonId)){
    expect_failure(expect_null( cppr_getPersonByEmail("test.person@testcompany.com")))
    expect_gt(length( cppr_getPersonByEmail("test.person@testcompany.com")),0)
  }
})

test_that("cppr_getPeople(tags=\"test_copperr\") should return a non-NULL result", {
  if(!is.null(testPersonId)){
    expect_failure(expect_null( cppr_getPeople(tags="test_copperr")))
  }
})

test_that("cppr_getAllRecordsRelatedToPerson() should return a non-NULL result if that Id exists", {
  if(!is.null(testPersonId)){
    expect_failure(expect_null( cppr_getAllRecordsRelatedToPerson(testPersonId)))
  }
})

test_that("cppr_getOppsRelatedToPerson() should return a non-NULL result if that Id exists", {
  if(!is.null(testPersonId)){
    expect_failure(expect_null( cppr_getOppsRelatedToPerson(testPersonId)))
  }
})

test_that("cppr_deletePerson() with a list containing all mandatory fields should return a non-Null numeric id result", {
  if(!is.null(testPersonId)){
    expect_failure(expect_null( cppr_deletePerson(testPersonId)))
  }
#  expect_null(cppr_deletePerson(-1)) # will get a 500 response
})
