% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/copperr_company.R
\name{cppr_getCompanyByName}
\alias{cppr_getCompanyByName}
\title{cppr_getCompanyByName}
\usage{
cppr_getCompanyByName(name)
}
\arguments{
\item{name}{string - the name of the Company to return}
}
\value{
a List of fields with various types, or Null if not found
}
\description{
Get the Company record using its Name
}
\note{
This uses the search API, which could return multiple companies with the same name. 
Only the first match is returned! (if you initialized the package with Verbose=TRUE, 
you will see a warning displayed in this case)
To be sure of getting the record you want, use cppr_getCompanyById which uses
a field guaranteed by Copper to be unique, or use cppr_getCompanies() to get all matches.
}
\examples{
\dontrun{
company <- cppr_getCompanyByName("Kodak")
print(paste("company Id is",company$id))
}
}
\seealso{
\code{\link{cppr_getCompanyById}} for full list of available Company fields

For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}

Other Company functions: \code{\link{cppr_createNewCompany}},
  \code{\link{cppr_deleteCompany}},
  \code{\link{cppr_getCompanies}},
  \code{\link{cppr_getCompanyById}},
  \code{\link{cppr_updateCompany}}
}
