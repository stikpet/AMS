
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
library(devtools)
devtools::install_github("stikpet/AMS")
```

## Functions

### For a Single Binary Variable

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

### For a Single Nominal Variable

Impression

-   **frequencyTable(data, field)**, this will generate a frequency
    table with percentages, valid percentages and cumulative percentages

Visualisations

-   **nom_vi_bar(data, field, height=“percent”, missing=FALSE)**,
    generates a bar-chart
-   **nom_vi_dot(data, field, dotScale=30, missing=FALSE)**, generates a
    dot plot
-   **nom_vi_cleveland(data, field, missing=FALSE, dotScale=30)**,
    generates a Cleveland dot plot
-   **nom_vi_pie(data, field, missing=FALSE)**, generates a pie chart
-   **nom_vi_pareto(data, field)**, generates a Pareto chart

Statistical measures

-   **nom_ce_mode(data, field)**, determines the mode
-   **nom_di_vr(data, field)**, determines the Variation Ratio

Omnibus tests

-   **nom_ts_pearson(data, field, corr=c(‘none’, ‘williams’,
    ‘pearson’))**, performs a Pearson chi-square goodness-of-fit test
-   **nom_ts_g(data, field, corr=c(‘none’, ‘williams’, ‘pearson’))**,
    performs a G chi-square goodness-of-fit test
-   **nom_ts_multi(data, field)**, performs an exact multinomial test

Post-hoc Test

-   **bin_decider(n, nThres=1000, prefTest=‘g’, corr=‘yates’,
    es=‘cohenH2’)**, helper function to set which test to use when
-   **nom_ph_test(data, field, binDecider = c(1000, ‘g’, ‘yates’,
    ‘cohenH2’))**, performs the post hoc test pairwise

Effect Sizes

-   **nom_es_cramerv(data, field, test=‘pearson’, corr=‘none’,
    bergsma=FALSE)**, Cramer’s V for GoF

-   **nom_es_cohenw(data, field, test=‘pearson’, corr=‘none’)**, Cohen’s
    w

-   **nom_es_jbme(data, field, test=‘pearson’, corr=‘none’)**, JBM E
