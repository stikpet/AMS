
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AMS

<!-- badges: start -->
<!-- badges: end -->

The goal of AMS is to provide simple to use functions for analysing
survey data. In this first release only functions for a binary variable
are included.

Each function requires the name of a dataframe and the name of the
specific field (as a string)

## Installation

You can install the library of AMS like so:

``` r
library(devTools)
devtools::install_github("stikpet/AMS")
```

## Functions

Impression

-   **frequencyTable(data, field)**, this will generate a frequency
    table with percentages, valid percentages and cumulative percentages

Hypothesis Tests:

-   **bi_ts_binomial(data, field)**, an exact one-sample binomial test
-   **bi_ts_score(data, field, corr=FALSE)**, a one-sample score test
    with or without Yates continuity correction
-   **bi_ts_wald(data, field, corr=FALSE)**, a one-sample Wald test with
    or without Yates continuity correction
-   **bi_ts_chi2Gof_Pearson(data, field, corr=‘none’)**, a Pearson
    chi-square goodness-of-fit test, with no correction, Yates,
    Williams, or Pearson.
-   **bi_ts_chi2Gof_G(data, field, corr=None)**, a G goodness-of-fit
    test, with no correction, Yates, Williams, or Pearson.

Each function will return the two-sided p-value and other info that
might need to be reported.

Effect sizes

-   **bi_es_cohenH2(data, field)**, Cohen’s h2
-   **bi_es_coheng(data, field)**, Cohen’s g
-   **bi_es_AR(data, field)**, Alternative Ratio (a.k.a. Relative Risk)

A qualification is also shown using a rule of thumb by some author.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
respID <- c(1,2,3,4,5,6,7,8,9,10)
category <- c(1,2,2,2,1,2,1,2,2,2)

df <- data.frame(respID, category)

library(AMS)
bi_ts_binomial(df, 'category')
#> [1] 0.34375
```
