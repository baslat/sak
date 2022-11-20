
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sak

<!-- badges: start -->

[![R-CMD-check](https://github.com/baslat/sak/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/baslat/sak/actions/workflows/R-CMD-check.yaml)
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
#> $name
#>   [1] "Amy"       "Caroline"  "Doris"     "Belle"     "Gloria"    "Anita"    
#>   [7] "Clara"     "Evelyn"    "Amelia"    "Bess"      "Cora"      "Juliet"   
#>  [13] "Ana"       "Bob"       "Claudette" "David"     "Frederic"  "Henri"    
#>  [19] "Bonnie"    "Charley"   "Georges"   "Danielle"  "Hermine"   "Ivan"     
#>  [25] "Jeanne"    "Karl"      "Emily"     "Floyd"     "Gert"      "Harvey"   
#>  [31] "Katrina"   "Alberto"   "Beryl"     "Chris"     "Debby"     "Ernesto"  
#>  [37] "Alicia"    "Barry"     "Chantal"   "Dean"      "Arthur"    "Cesar"    
#>  [43] "Diana"     "Edouard"   "Gustav"    "Hortense"  "Isidore"   "Josephine"
#>  [49] "Klaus"     "Lili"      "Danny"     "Fabian"    "Isabel"    "Juan"     
#>  [55] "Kate"      "Andrew"    "AL031987"  "AL061988"  "Gilbert"   "Isaac"    
#>  [61] "Joan"      "Keith"     "Allison"   "Erin"      "Felix"     "Gabrielle"
#>  [67] "Hugo"      "Iris"      "Jerry"     "Karen"     "Marco"     "Nana"     
#>  [73] "AL041991"  "Erika"     "AL101991"  "AL121991"  "AL021992"  "AL031992" 
#>  [79] "AL081992"  "AL011993"  "AL101993"  "AL021994"  "AL051994"  "AL081994" 
#>  [85] "AL091994"  "AL101994"  "Gordon"    "AL061995"  "Humberto"  "Luis"     
#>  [91] "AL141995"  "Marilyn"   "Noel"      "Opal"      "Pablo"     "Roxanne"  
#>  [97] "Sebastien" "Tanya"     "Kyle"      "Bill"      "AL061997"  "Alex"     
#> [103] "Lisa"      "Mitch"     "Nicole"    "AL021999"  "AL071999"  "AL111999" 
#> [109] "AL121999"  "Lenny"     "AL012000"  "AL022000"  "AL042000"  "AL092000" 
#> [115] "Joyce"     "Leslie"    "Michael"   "Nadine"    "AL022001"  "AL092001" 
#> [121] "Lorenzo"   "Michelle"  "Olga"      "Cristobal" "Fay"       "AL072002" 
#> [127] "Hanna"     "AL142002"  "AL022003"  "AL062003"  "AL072003"  "AL092003" 
#> [133] "AL142003"  "Larry"     "Mindy"     "Nicholas"  "Odette"    "Peter"    
#> [139] "Gaston"    "AL102004"  "Matthew"   "Otto"      "Franklin"  "Ten"      
#> [145] "Lee"       "Maria"     "Nate"      "Ophelia"   "Philippe"  "Rita"     
#> [151] "Nineteen"  "Stan"      "Tammy"     "Vince"     "Wilma"     "Beta"     
#> [157] "Gamma"     "Epsilon"   "Zeta"      "AL022006"  "Ingrid"    "Melissa"  
#> [163] "Fifteen"   "Ike"       "Omar"      "Sixteen"   "Paloma"    "One"      
#> [169] "Fred"      "Eight"     "Ida"       "Two"       "Colin"     "Five"     
#> [175] "Fiona"     "Igor"      "Julia"     "Paula"     "Richard"   "Shary"    
#> [181] "Tomas"     "Don"       "Katia"     "Al202011"  "Rina"      "Sean"     
#> [187] "Kirk"      "Oscar"     "Patty"     "Rafael"    "Sandy"     "Tony"     
#> [193] "Andrea"    "Dorian"    "Fernand"   "Gonzalo"   "Nine"      "Joaquin"  
#> [199] "Ian"       "Four"      "Eleven"    "Three"     "Imelda"    "Nestor"   
#> [205] "Isaias"    "Paulette"  "Rene"      "Sally"     "Teddy"     "Vicky"    
#> [211] "Wilfred"   "Eta"       "Theta"     "Iota"     
#> 
#> $status
#> [1] "tropical depression" "tropical storm"      "hurricane"          
#> 
#> $category
#> [1] -1 0  1  3  2  5  4 
#> Levels: -1 < 0 < 1 < 2 < 3 < 4 < 5
```
