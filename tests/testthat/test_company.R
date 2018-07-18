context("Company functions")
#library()

dw <- config::get("copper") 
cppr_initialize(dw$uid,dw$api_key)

# clean up any test data from previous test runs
artifactCo <- cppr_getCompanies(tags="test_copperr")
if((nrow(artifactCo) > 0) & nrow(artifactCo) < 5){
  sapply(artifactCo$id,cppr_deleteCompany)
}

testCompanyId <- cppr_createNewCompany(list( 
                                                `name` = "Test Company", 
                                                `address` = list(`city`="Auckland", `country` = "NZ"), 
                                                `email_domain` = "testcompany.com",
                                                `tags` = list("test")
                                                ))

test_that("cppr_createNewCompany() with a list containing all mandatory fields should return a non-Null numeric id  result", {
  expect_failure(expect_null(testCompanyId))
#  expect_null(cppr_createNewCompany(list( NULL))) # gets a 500 error response
})

test_that("cppr_getCompanyByName() should return a non-NULL, non-empty result if that company exists", {
  if(!is.null(testCompanyId)) 
    expect_failure(expect_null( cppr_getCompanyByName("Test Company") ))
  expect_equal( length(cppr_getCompanyByName("A guaranteed non-existent company name")),0)
})

test_that("cppr_getCompanyById() should return a non-NULL, non-empty result if that Id exists", {
  if(!is.null(testCompanyId)){
    expect_failure(expect_null( cppr_getCompanyById(testCompanyId)))
    expect_gt(length( cppr_getCompanyById(testCompanyId)),0)
  }
#  expect_null(cppr_getCompanyById(-1)) # will cause a 500 error
})

test_that("cppr_getCompanies(tags=\"test_copperr\") should return a non-NULL result", {
  if(!is.null(testCompanyId)){
    expect_failure(expect_null( cppr_getCompanies(tags="test_copperr")))
#    expect_gt(length( cppr_getCompanies(tags="test_copperr")),0)
  }
})

test_that("cppr_updateCompany() with a valid id should return a non Null numeric id as result", {
  if(!is.null(testCompanyId)){
    expect_failure(expect_null( cppr_updateCompany(testCompanyId, list( `details` = "updated"))))
  }
#  expect_null(cppr_updateCompany(-1, list( `details` = "updated")))
})

test_that("cppr_deleteCompany() with a list containing all mandatory fields should return a non-Null numeric id  result", {
  if(!is.null(testCompanyId)){
    expect_failure(expect_null( cppr_deleteCompany(testCompanyId)))
  }
})


