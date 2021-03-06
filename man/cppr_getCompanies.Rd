% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/copperr_company.R
\name{cppr_getCompanies}
\alias{cppr_getCompanies}
\title{cppr_getCompanies}
\usage{
cppr_getCompanies(assignee_id = NULL, country = NULL, state = NULL,
  tags = NULL, sort_by = "name", sort_direction = "asc")
}
\arguments{
\item{assignee_id}{numeric - select companies assigned to this User}

\item{country}{string - select companies from this country (specify the 2-letter country code, e.g. "NZ")}

\item{state}{string - select companies from this state (specify the full name of state, e.g. "Idaho")}

\item{tags}{string - select companies with one of these tags}

\item{sort_by}{values can be any of : (can be combined to provided ANDed)
\itemize{
  \item name
  \item assignee
  \item company_name
  \item customer_source
  \item city
  \item state
  \item inactive_days
}}

\item{sort_direction}{can be "desc" or "asc"}
}
\value{
a data frame of Company field lists, or Null if not found
each Company is a row in the data frame, and contains a list of fields
}
\description{
Return a full table of all Company records from Copper
}
\examples{
\dontrun{
companies <- cppr_getCompanies()
print(paste("retrieved",nrow(companies),"companies from Copper"))
}
}
\seealso{
\code{\link{cppr_getCompanyById}} for full list of available Company fields

For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}

Other Company functions: \code{\link{cppr_createNewCompany}},
  \code{\link{cppr_deleteCompany}},
  \code{\link{cppr_getCompanyById}},
  \code{\link{cppr_getCompanyByName}},
  \code{\link{cppr_updateCompany}}
}
