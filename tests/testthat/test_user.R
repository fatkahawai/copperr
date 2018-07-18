context("User functions")
#library()

dw <- config::get("copper") 
cppr_initialize(dw$uid,dw$api_key)

users <- cppr_getUsers()

test_that("cppr_getUsers() should return a non-NULL result", {
  expect_failure(expect_null(users))
  expect_gt( nrow(users),0)
})

test_that("cppr_getUserId() should return a non-NULL result if that User Exists", {
  if(!is.null(users)){
  	expect_failure(expect_null(cppr_getUserId(users$name[1])))
  }
  expect_null(cppr_getUserId("A guaranteed non-existent user name")) 
})
