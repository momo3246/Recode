% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RecodeR.R
\name{check_code}
\alias{check_code}
\title{Check particular code}
\usage{
check_code(vector, code)
}
\arguments{
\item{vector}{character vector (for MA question); numeric vector (for SA question)}

\item{code}{integer}
}
\value{
logical vector
}
\description{
This function is to check if a particualr code exists
}
\examples{
check_code(c("1,2,33"), 3) #FALSE

}
