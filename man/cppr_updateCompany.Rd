% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/copperr_company.R
\name{cppr_updateCompany}
\alias{cppr_updateCompany}
\title{cppr_updateCompany}
\usage{
cppr_updateCompany(id, list_of_fields)
}
\arguments{
\item{id}{numeric - the Id of the Company to be updated}

\item{list_of_fields}{list - containing oly the fields you want to modify}
}
\value{
the id of the Company record if successful else NULL
}
\description{
Updates a Company record in Copper using a list of values you provide
}
\examples{
\dontrun{
fields <- list( `assignee_id` = 12345 )
if(cppr_updateCompany(fields))
  print("succesfully assigned new owner"))
}
}
\seealso{
\code{\link{cppr_getCompanyById}} for full list of Company fields

For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}

Other Company functions: \code{\link{cppr_createNewCompany}},
  \code{\link{cppr_deleteCompany}},
  \code{\link{cppr_getCompanies}},
  \code{\link{cppr_getCompanyById}},
  \code{\link{cppr_getCompanyByName}}
}
