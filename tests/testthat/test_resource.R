context("Resource functions")
#library()

test_that("cppr_getContactTypes() should return a non-NULL result", {
  expect_failure(expect_null(cppr_getContactTypes()))
})

test_that("cppr_getCustomerSources() should return a non-NULL result", {
  expect_failure(expect_null(cppr_getCustomerSources()))
})

test_that("cppr_getCustomerSourceId() should return a non-NULL result if valid", {
  expect_failure(expect_null(cppr_getCustomerSourceId("Inbound")))
  expect_null(cppr_getCustomerSourceId("Some random text"))
})

#--------------
pipes<-cppr_getPipelines()

test_that("cppr_getPipelines() should return a non-NULL, non empty result", {
  expect_failure(expect_null(pipes))
  expect_gt(nrow(pipes),0)
})

stages<-cppr_getPipelineStages(pipes$id[1])

test_that("cppr_getPipelineStages() should return a non-NULL, non empty result", {
  expect_failure(expect_null(stages))
  expect_gt(nrow(stages),0)
})

test_that("cppr_getPipelineStageId() should return a non-NULL, numeric result", {
  expect_failure(expect_null(cppr_getPipelineStageId(pipes$id[1],stages$name[1])))
  expect_gt(cppr_getPipelineStageId(pipes$id[1],stages$name[1]),0)
})
#--------------
customFields <- cppr_getCustomFieldDefinitions()

test_that("cppr_getCustomFieldDefinitions() should return a non-NULL result", {
 expect_failure(expect_null(customFields))
})

test_that("cppr_getCustomFieldDatatype() should return a non-NULL result if valid", {
  expect_failure(expect_null(cppr_getCustomFieldDatatype("Vertical")))
  expect_null(cppr_getCustomFieldDatatype("Some random text"))
})

test_that("cppr_getCustomFieldId() should return a non-NULL result if valid", {
  expect_failure(expect_null(cppr_getCustomFieldId(customFields$name[1])))
  expect_null(cppr_getCustomFieldId("Some random text"))
})

test_that("cppr_getCustomFieldName() should return a non-NULL result if valid", {
  expect_failure(expect_null(cppr_getCustomFieldName(customFields$id[1])))
  expect_null(cppr_getCustomFieldName(999999))
})

options <- cppr_getCustomFieldOptionsAll(customFields$name[1])

test_that("cppr_getCustomFieldOptionsAll() should return a non-NULL result if valid", {
  expect_failure(expect_null(options))
  expect_null(cppr_getCustomFieldOptionsAll("Some random text"))
})

#--------------
dropdownFields <- customFields[(customFields$data_type=="Dropdown" | customFields$data_type=="MultiSelect"),] 
anyDropdownField <- as.character(dropdownFields$name[1])

test_that("cppr_getCustomFieldOptionId()should return a non-NULL result if the field is a dropdown and the name is valid", {
  expect_failure(expect_null(cppr_getCustomFieldOptionId(anyDropdownField,options$name[1])))
  expect_null(cppr_getCustomFieldOptionId(anyDropdownField,"Some random text"))
})

test_that("cppr_getCustomFieldOptionName() should return a non-NULL result if the field is a dropdown and the id is valid", {
  expect_failure(expect_null(cppr_getCustomFieldOptionName(anyDropdownField,options$id[1])))
  expect_null(cppr_getCustomFieldOptionName(anyDropdownField,999999))
})

#--------------
statuses<-cppr_getLeadStatuses()

test_that("cppr_getLeadStatuses() should return a non-NULL, non empty result", {
  expect_failure(expect_null(statuses))
  expect_gt(nrow(statuses),0)
})

test_that("cppr_getLeadStatusId() should return a non-NULL, numeric result", {
  expect_failure(expect_null(cppr_getLeadStatusId(statuses$name[1])))
  expect_gt(cppr_getLeadStatusId(statuses$name[1]),0)
})



