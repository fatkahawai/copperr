README
======

_(C) 2018 Robert Drummond_

**copperr** is an R package that connects to Copper CRM Platform APIs using tidy principles. The package implements most key actions from the REST API. Package features include:

-   Basic (Username/API-key) Authentication methods (`cppr_initialize`)
-   CRUD (Create, Retrieve, Update, Delete) methods for records (`cppr_createXxx`, `cppr_getXxx`, `cppr_updateXxxx`, and `cppr_deleteXxx`)
-   Query a set of records (`cppr_getXxxxx`)
-   Retrieve metadata Resources (Custom Fields, Sources, Pipeline Stages etc.) 
-   Retrieve User profiles (`cppr_getUsers`)
-   Helper functions to simplify some common tasks

Table of Contents
-----------------

-   [Installation](#installation)
-   [Usage](#usage)
    -   [HTTP Error Codes](#http-error-codes)
    -   [Initialize](#inititalize)
    -   [Get](#get)
    -   [Create](#create)
    -   [Update](#update)
    -   [Resources](#resources)
-   [Future](#future)
-   [Credits](#credits)
-   [More Information](#more-information)

Installation
------------

You can install the copperr package in your R environment using devtools like this:

# install.packages("devtools")
devtools::install_github("fatkahawai/copperr")

# if you want to access the vignettes from within the package:
devtools::install_github("fatkahawai/copperr", build_vignettes = TRUE)
browseVignettes("copperr")

Usage
-----

### HTTP Error Codes
Responses returned by the Copper API use the customary HTTP status codes. On anything other than a 200 OK response, the **copperr** methods will throw an error, displaying the status code and message returned. The Copper documentation lists the most typical error codes being:
> Code	Meaning
> 400	Bad Request
> 401	Unauthorized
> 429	Too many requests
> 500	Internal Server Error

I have also observed these undocumented error responses for the common situations described:
> Code	Meaning
> 404	When attempting to retrieve a record using an id Copper does not recognize, e.g. `999`
> 500 	When attempting to retrieve a record using an invalid id, e.g. `-1`
> 422	When the the parameter list to a create or update call is incorrectly formatted and can't be parsed by Copper.

### Initialize
First, load the **copperr** package and login using your user Id and API Key. User credentials will 
be cached in a package-internal environment. The verbose flag controls whether the package functions print status log messages to the console

``` r
library(copperr)
cppr_initialize(uid = "<your copper user id>", api_key= "<your copper API key>", verbose = FALSE)
```

After initializing, you can now check your connectivity by looking at the information returned about registered Copper Users at your organization. 

``` r
# get a table of users
# it's a simple easy call to get started 
# and confirm a connection to the APIs
users.df <- cppr_getUsers()
print("Table of Users:")
users.df
```

### Get 
Copper has objects and those objects contain records. If you're using R with Copper, it will likely be mainly for extracting and analyzing the data held in Copper, and you will find the set of get functions most useful. 
The `cppr_getXXX()` functions pull down a specific set of records and fields and are similar to running a query. Let's retrieve a set of Companies as a data frame using a few of the available filter arguments, and sort the table alphabetically by name in reverse order

``` r
floridaRecords.df <- cppr_getCompanies(country="US", state="FL", sort_by="name", sort_direction="desc")

if(!is.null(floridaRecords.df)) {
  sprintf("retrieved %d matching records",nrow(floridaRecords.df))
  floridaRecords.df
}
```

or we can simply extract all records of any type of object, by omitting the filter arguments, like this, then filter, sort and manipulate the data frame using tidy `dplyr` functions

``` r
allRecords.df <- cppr_getCompanies()

if(!is.null(allRecords.df)) {
  sprintf("retrieved all %d records",nrow(allRecords.df))
}
```

There are also functions which retrieve an individual record, which is returned as a nested list of fields. 

``` r
company.lst <- cppr_getCompanyById(allRecords.df$id[1])
str(company.lst)
```

### Create 
The `cppr_createNewXxx` functions will create new records for any Copper Object. This example shows how to create a new record in the Company object. the fields are provided as a list, which must contain at a minimum those fields your organization has defined as mandatory under the Copper settings. The function returns the id assigned by Copper to the new record. Copper creates this unique numeric identifier for each record and uses that to know which record to attach the update information you provide. 

``` r
fields <- list( `name` = "Test Company",
                `country` = "NZ", 
                `tags` = list("testcopperr"), 
                `email_domain` = "testcompany.com" # this field is treated as a unique key by Copper 
                )
newCompanyId <- cppr_createNewCompany(fields)
if(!is.null(newCompanyId))
  sprintf("successfully created a new Company with the id %d",newCompanyId)
```

We can then retrieve the record we just created, using the id returned. 

``` r
retrievedRecord <- cppr_getCompanyById(newCompanyId)
if(!is.null(retrievedRecord))
  sprintf("retrieved the Company named %s",retrievedRecord$name)
```

In the same way, we can create a new record in the Person Object. Note that the email address must be unique in Person and Lead records, and that email addresses are passed as a nested list of email address & category records 

``` r
pfields <- list( `name` = "Test Person", 
                  `country` = "NZ", 
                  `tags` = list("testcopperr"), 
                  `company_id` = newCompanyId,
                  `emails` = list(list(`email`="test.person@testcompany.com", `category`="work")) 
                  )
newPersonId <- cppr_createNewPerson(pfields)
if(!is.null(newPersonId))
  sprintf("successfully created a new Person with id %d",newPersonId)
```

Having created a Company and a Person, we can now create an Opportunity and relate it to both these

``` r
ofields <- list(  `name` = "Test Opp", 
                  `company_id` = newCompanyId, 
                  `tags` = list("testcopperr"), 
                  `primary_contact_id` = newPersonId
                  )
newOppId <- cppr_createNewOpp(ofields)
if(!is.null(newOppId)){
  print(sprintf("successfully created a new Opp with id %d",newOppId))
```

### Update 

After creating records you can also modify or update them using the `cppr_updateXxx()` functions. 
Updating a record requires you to pass the Copper id of the record, and a list of field labels and values (you only provide the fields that you want to modify). Here is an example where we update the Company record we created earlier with new `details` information 

``` r
# Update that new Company we created

updatedRecord <- cppr_updateCompany(newCompanyId, list(`details`="updated today"))
updatedRecord
```

### Resources
Copper contains static reference data it calls Resources. These are the pipelines, contact types, custom fields, dropdown menu options, etc. you will find under the Copper settings. You will need to retrieve these Resources to associate the labels you see in the Copper UI with their corresponding ids assigned by Copper internally. Copper delivers the ids for these resources in the Object records you retrieve, and expects you to provide the ids, not the labels when creating and updating record fields.

A simple resource to start with is the list of options you see in the dropdown menu when selecting a `Contact Type` (typically these will have labels scuh as "Current Customer", "Potential Customer", etc).

``` r
cType <- cppr_getContactTypes()
print("the dropdown for Contact Type contains these options:")
cType
```

The Custom Fields your organization has defined in Copper, and the options defined for any that are dropdown type, are also all identified only by their Ids in records delivered by Copper. 
You are likely to need to map field names and options to their ids and vice versa many times, so the Custome Field Definitions are retrieved and cached in the package's local environment on the first call to any of the custom field functions. Subsequent calls will access the cached table 

Lets see how we can handle Custom Fields using the package functions provided

``` r
suppressWarnings(library(dplyr))

# retrieve a table of all defined Custom Fields
customFields <- cppr_getCustomFieldDefinitions()
sprintf("there are %d custom fields defined", nrow(customFields))

# let's identify any of these fields that has an associated list of dropdown menu options 
# for its values
dropdownFields <- customFields %>% filter(`data_type`=="Dropdown" | `data_type`=="MultiSelect") %>% select(name) 
anyDropdownField <- as.character(dropdownFields$name[1])
if(length(anyDropdownField)==0)
  stop("no dropdown fields have been defined in Copper settings")

# we can check again that the data type for this field is Dropdown using this helper function 
# and the field's id
dtype <- cppr_getCustomFieldDatatype(anyDropdownField)
sprintf("the data type for the %s custom field is %s",anyDropdownField,dtype)
```

Now let's try out dereferencing the id from the name for that Custom Field using a helper function, and also just for fun then look up the name again using the id

``` r
fieldId <- cppr_getCustomFieldId(anyDropdownField)
sprintf("the id for the %s custom field is %d",anyDropdownField, fieldId)

fieldName <- cppr_getCustomFieldName(fieldId)
sprintf("the name for the custom field %d is %s", fieldId, fieldName)
```

A Field of Dropdown (or MultiSelect) data type has a list of valid options to select from. Each option has a text label and an id assigned by Copper. Lets see how we can use these

``` r
# retrieve the table of valid options available for this Custom Field
options <- cppr_getCustomFieldOptionsAll(anyDropdownField)
sprintf("the options available for the %s custom field dropdown:",anyDropdownField)
options

# look up the label name for one of the options in the table. let's simply take the first
optionName <- cppr_getCustomFieldOptionName(anyDropdownField, options$id[1])
sprintf("the label for option %d of dropdown field %s is %s", options$id[1], anyDropdownField, optionName)

optionId <- cppr_getCustomFieldOptionId(anyDropdownField,optionName)
sprintf("the id for the option [%s] of dropdown field %s is %d", optionName, anyDropdownField, optionId)
```

Lets see how we use these functions to update a custom field of the Company we created above

``` r
fields <- list( `custom_fields` = data.frame( `custom_field_definition_id` = fieldId, `value`= optionId))
if(cppr_updateCompany(newCompanyId, fields) == newCompanyId){
	sprintf("updated dropdown custom field [%s] of new Company to [%s] successfully", anyDropdownField, optionName)
}
```

When we are dealing with records from the Opportunity Object, we will also need to deal with Pipeline and Pipeline Stage resources.
Let's start by retrieving all the Pipelines your organzation has defined in Copper

``` r
pipelines.df <- cppr_getPipelines()
pipelines.df
```

Now lets see what Stages have been defined for the first Pipeline in the table

``` r
stages.df <- cppr_getPipelineStages(pipelines.df$id[1])
sprintf("Stages for the [%s] Pipeline",pipelines.df$name[1])
stages.df
```


### Check out the Tests

The **Copperr** package has quite a bit of unit test coverage to track any 
changes made between newly released versions of the Copper API. 
These tests are an excellent source of examples because they cover most all cases of 
utilizing the package functions. 

Future
------

Future APIs to support:
* Tasks and Activities
* the Project Object 

More Information
----------------

Copper provides client libraries and examples in many programming langauges (Java, Python, Ruby, and PhP) but unfortunately R is not a supported language. However, most key operations supported by the Copper APIs are available via this package. This package makes requests best formatted to match what the APIs require as input. This articulation is not perfect and continued progress will be made to add and improve functionality. For details on formatting, attributes, and methods please refer to the [Copper API documentation(http://developer.copper.com). as they are explained more fully there.


