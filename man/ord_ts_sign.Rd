% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/singleOrdinal.R
\name{ord_ts_sign}
\alias{ord_ts_sign}
\title{Single Ordinal - Test - Sign
.
Performs a one-sample Sign test}
\usage{
ord_ts_sign(data, field, order = "none", hypMed = "none")
}
\arguments{
\item{data}{A dataframe to be used}

\item{field}{Specific field in the dataframe as string}

\item{order}{A vector with the labels in order, or set to 'none'}
}
\value{
pVal The p-value of the test
}
\description{
Single Ordinal - Test - Sign
.
Performs a one-sample Sign test
}
\details{
This uses a binomial test which for large samples might take some time.
}
