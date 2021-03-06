% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/singleBinary.R
\name{bi_ts_binomial}
\alias{bi_ts_binomial}
\title{Single Binary Test - One-sample binomial}
\usage{
bi_ts_binomial(data, field)
}
\arguments{
\item{data}{A dataframe}

\item{field}{The field within the dataframe to use as string}
}
\value{
pVal The two-sided significance (p-value)
}
\description{
This function will perform an exact one-sample binomial test, two-sided
It assumes the probability of success in the population to be 0.5
}
\details{
As number of success it picks the first category found.
For more info on this test with R: https://youtu.be/wUFNO7AjOGo
}
\author{
P. Stikker
Please visit: https://PeterStatistics.com
YouTube channel: https://www.youtube.com/stikpet
}
