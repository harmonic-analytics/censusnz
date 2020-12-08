
<!-- README.md is generated from README.Rmd. Please edit that file -->

# censusnz <img src='man/figures/logo.png' align="right" height="182.5" />

<!-- badges: start -->

[![pipeline-status](https://gitlab.harmonic.co.nz/harmonic/packages/censusnz/badges/master/pipeline.svg)](https://gitlab.harmonic.co.nz/harmonic/packages/censusnz/pipelines)
[![coverage
report](https://gitlab.harmonic.co.nz/harmonic/packages/censusnz/badges/master/coverage.svg)](https://gitlab.harmonic.co.nz/harmonic/packages/censusnz/commits/master)
<!-- badges: end -->

# Overview

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

## Example

A basic call to get\_data() for a particular geography and variables:

``` r
library(censusnz)

get_data("RC", c("maori_descent", "smoking_status")) %>% head()
#> # A tibble: 6 x 6
#>   geoid land_type name           variable     variable_group               count
#>   <chr> <chr>     <chr>          <chr>        <chr>                        <dbl>
#> 1 01    Mixture   Northland Reg~ maori_desce~ 01_maori_descent_curp        69225
#> 2 01    Mixture   Northland Reg~ maori_desce~ 02_no_maori_descent_curp    104586
#> 3 01    Mixture   Northland Reg~ maori_desce~ 04_dont_know_curp             5268
#> 4 01    Mixture   Northland Reg~ maori_desce~ total_stated_curp           179076
#> 5 01    Mixture   Northland Reg~ maori_desce~ 99_not_elsewhere_included_~      0
#> 6 01    Mixture   Northland Reg~ maori_desce~ total_curp                  179076
```
