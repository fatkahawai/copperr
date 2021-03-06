% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/copperr_users.R
\name{cppr_getUsers}
\alias{cppr_getUsers}
\title{cppr_getUsers
returns a table of all Copper users}
\usage{
cppr_getUsers()
}
\value{
a data frame of users, or NULL on error.\cr
Field Type    Details\cr
id    number  Unique identifier \cr
name  string  The user name \cr
email string  email address
}
\description{
cppr_getUsers
returns a table of all Copper users
}
\examples{
\dontrun{ 
users <- cppr_getUsers()
}
}
\seealso{
For details of data structures refer to \href{https://developer.copper.com/#getting-support}{Copper (TM) Developer API Guide}

Other user functions: \code{\link{cppr_getUserId}}
}
