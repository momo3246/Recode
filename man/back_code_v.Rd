% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RecodeR.R
\name{back_code_v}
\alias{back_code_v}
\title{Back code OTHERS into pre-coded question (compatible with pipe operator)}
\usage{
back_code_v(raw, coded, others_code, SN_matching)
}
\arguments{
\item{raw}{dataframe with 2 columns (for backcoding with SN matching); vector (for backcoding without SN mathcing)}

\item{coded}{dataframe with 2 columns (for backcoding with SN matching); vector (for backcoding without SN mathcing)}

\item{others_code}{Single integer}

\item{SN_matching}{Logical}
}
\description{
This function is to back code verbatim into a pre-coded question
}
\details{
Inputs of this function vary upon usage; If no need for SN matching, set FALSE in the argument "SN_matching", or vice
}
\examples{
##Populate a dataframe
raw <- data.frame(SN=c(1, 2000, 3, 4),
raw_data=c("1,2,97", "1,3", "97", "1,2,97"), stringsAsFactors=FALSE)
coded <- data.frame(SN=c(2000, 1, 3, 4),
coded_data=c(NA, "2,3", "9", "97"),
stringsAsFactors=FALSE)##Populate another dataframe
back_code(raw, coded, others_code = 97, SN_matching = TRUE)
#[[1]] "1,2,3"  "1,3"    "9"      "1,2,97";
#[[2]]
#SN raw_data coded_data results
#1    1   1,2,97        2,3   1,2,3
#2 2000      1,3       <NA>     1,3
#3    3       97          9       9
#4    4   1,2,97         97  1,2,97

# For matched SN
raw1 <- data.frame(raw_data=c("1,2,97", "1,3", "97", "1,2,97"),
stringsAsFactors=FALSE)##Populate a dataframe
coded1 <- data.frame(coded_data=c("5", NA, "9", "8"), stringsAsFactors=FALSE)
##Populate another dataframe
back_code(raw1$raw_data, coded1$coded_data, others_code = 97, FALSE)
#[[1]]
#[1] "1,2,5" "1,3"   "9"     "1,2,8"

#[[2]]
#raw      coded results
#[1,] "1,2,97" "5"   "1,2,5"
#[2,] "1,3"    NA    "1,3"
#[3,] "97"     "9"   "9"
#[4,] "1,2,97" "8"   "1,2,8"

# For OTHERS code other than code 97
raw2 <- data.frame(raw_data=c("1,2,91", "1,3", "91", "1,2,91"),
stringsAsFactors=FALSE)##Populate a dataframe
coded2 <- data.frame(coded_data=c("5", NA, "9", "8"),
stringsAsFactors=FALSE)##Populate another dataframe
back_code(raw2$raw_data, coded2$coded_data, others_code = 91, FALSE)

}
