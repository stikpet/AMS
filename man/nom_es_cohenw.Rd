% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/singleNominal.R
\name{nom_es_cohenw}
\alias{nom_es_cohenw}
\title{Single Nominal - Effect Size - Cohen's w}
\usage{
nom_es_cohenw(data, field, test = "pearson", corr = "none")
}
\arguments{
\item{data}{Dataframe to be used}

\item{field}{Specific field name (as string) from data frame}

\item{test}{Which test to use, either 'pearson' (default) or 'g'}

\item{corr}{Correction to use in test, either 'none' (default), 'yates', 'pearson', or 'williams'}
}
\value{
results A dataframe with:

qual A qualification of the effect size

es The effect size
}
\description{
This function will determine Cohen's w for a Goodness-of-Fit test
}
