
<!-- README.md is generated from README.Rmd. Please edit that file -->

# censusnz <img src='man/figures/logo.png' align="right" height="182.5" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/harmonic-analytics/censusnz.svg?branch=master)](https://travis-ci.org/harmonic-analytics/censusnz)
<!-- badges: end -->

This package provides an interface to the NZ Census data, returning
tidyverse-ready data frames. It also includes functions to plot data for
selected regions and variables.

Currently, the **censusnz** package provides data for seven geographical
areas:

-   **SA1**: Statistical Area 1
-   **SA2**: Statistical Area 2
-   **WARD**: Ward
-   **DHB**: District Health Board
-   **LBA**: Local Board Area (Auckland region only)
-   **TA**: Territorial Authority
-   **RC**: Regional Council

This package automatically installs the **db.censusnz** package.

## Installation

Install this package using the remotes package:

``` r
remotes::install_github("harmonic-analytics/censusnz")
```

## Examples

The package includes vignettes detailing the use of the provided
functions. To view these vignettes run the following after installing
the package:

``` r
browseVignettes(package = "censusnz")
```
