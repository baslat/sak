
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sak

<!-- badges: start -->

[![R-CMD-check](https://github.com/baslat/sak/workflows/R-CMD-check/badge.svg)](https://github.com/baslat/sak/actions)
<!-- badges: end -->

`sak` is my Swiss Army knife package, and it’s also where I put things.
It has a whole bunch of random helper functions to make my life a bit
easier.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("baslat/sak")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(sak)
## What are the unique values in each non-numeric dataframe column?
sg(dplyr::storms)
#> [[1]]
#> # A tibble: 214 × 1
#>    name    
#>    <chr>   
#>  1 Amy     
#>  2 Caroline
#>  3 Doris   
#>  4 Belle   
#>  5 Gloria  
#>  6 Anita   
#>  7 Clara   
#>  8 Evelyn  
#>  9 Amelia  
#> 10 Bess    
#> # … with 204 more rows
#> 
#> [[2]]
#> # A tibble: 3 × 1
#>   status             
#>   <chr>              
#> 1 tropical depression
#> 2 tropical storm     
#> 3 hurricane          
#> 
#> [[3]]
#> # A tibble: 7 × 1
#>   category
#>   <ord>   
#> 1 -1      
#> 2 0       
#> 3 1       
#> 4 3       
#> 5 2       
#> 6 5       
#> 7 4
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
