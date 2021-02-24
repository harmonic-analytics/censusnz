
<!-- README.md is generated from README.Rmd. Please edit that file -->

# censusnz <img src='man/figures/logo.png' align="right" height="182.5" />

<!-- badges: start -->

[![pipeline-status](https://gitlab.harmonic.co.nz/harmonic/packages/censusnz/badges/master/pipeline.svg)](https://gitlab.harmonic.co.nz/harmonic/packages/censusnz/pipelines)
[![coverage
report](https://gitlab.harmonic.co.nz/harmonic/packages/censusnz/badges/master/coverage.svg)](https://gitlab.harmonic.co.nz/harmonic/packages/censusnz/commits/master)
<!-- badges: end -->

This package provides an interface to the NZ Census data, returning
tidyverse-ready data frames.

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

To install this package from gitlab, you must first generate a Personal
Access Token; the package can then be installed using the Remotes
package:

``` r
remotes::install_gitlab(repo = 'harmonic/packages/censusnz', auth_token = <PAT>, host = 'gitlab.harmonic.co.nz')
```

## Examples

The package includes a vignettes detailing the use of the provided
functions. To view this vignette run the following after installing the
package:

``` r
vignette("Example 01", package = "censusnz")
```
