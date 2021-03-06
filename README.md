---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# semverutils

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/ajwtech/semverutils.svg?branch=master)](https://travis-ci.org/ajwtech/semverutils)
<!-- badges: end -->

The goal of semverutils is to provide a way to create an object for working with Semantic versioning
that is compliant with the semantic versioning 2.0.0

## Installation

You can install the released version of semverutils from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("semverutils")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ajwtech/semverutils")
```
## Example

This is a basic example which shows you how to solve a common problem:


```r
library(semverutils)
sem <- semVer$new("3.0.0-alpha.1+build.341193")
sem$higherThanAll(c("2.0.2+build.341193", "0.1.0"))
#> [1] TRUE
sem$getMajor
#> [1] "3"
```
