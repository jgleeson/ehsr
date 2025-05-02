
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ehsr

<!-- badges: start -->
<!-- badges: end -->

An R package to facilitate the importation of English Housing Survey
(EHS) data files that you have already downloaded from the UK Data
Service. When pointed to a folder where the EHS files are saved, ehsr
can identify and import the files for a given year or series of years,
join them together into a single object, standardise variable names and
convert labelled values to factors.

Your use of data downloaded from the UK Data Service must be consistent
with the requirements and conditions of the UKDS that you agreed to when
accessing the data.

## Installation

You can install the development version of ehsr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("jgleeson/ehsr")
```

## Example

`ehsr` currently contains two functions, one to import just the
‘derived’ EHS datasets and another to additionally import a defined set
of variables from the detailed datasets. If you have already downloaded
the EHS special licence data for 2022 and have assigned the path where
your EHS datasets are saved to the ‘folder’ object, the first function
is used as follows.

``` r
library(ehsr)

d <- hh_derived(folder, 2022)

head(d)
#> # A tibble: 6 × 139
#>   serial_number hweight   paired   fqtr  fmonth fyear fimonth fiqtr fiyear pqtr 
#>   <chr>         <dbl+lbl> <fct>    <fct> <fct>  <dbl> <fct>   <fct> <dbl+> <fct>
#> 1 20230000002   1739.     Not pai… Quar… <NA>   2022  October quar… 2022   <NA> 
#> 2 20230000008    829.     Not pai… Quar… Janua… 2022  January quar… 2023   <NA> 
#> 3 20230000024    807.     Not pai… Quar… Novem… 2022  Novemb… quar… 2022   quar…
#> 4 20230000026    836.     Not pai… Quar… Janua… 2022  January quar… 2023   <NA> 
#> 5 20230000031   4937.     Not pai… Quar… Janua… 2022  January quar… 2023   <NA> 
#> 6 20230000032   1388.     Not pai… Quar… Janua… 2022  January quar… 2023   quar…
#> # ℹ 129 more variables: pyear <dbl+lbl>, pmonth <fct>, tenure8x <fct>,
#> #   tenure4x <fct>, tenure2x <fct>, region <fct>, region3x <fct>,
#> #   ru11morph <fct>, ru11contxt <fct>, ru11combin <fct>, govreg1 <fct>,
#> #   imd1910 <fct>, hhcompx <fct>, hhcomp1 <fct>, hhtype11 <fct>, hhtype7 <fct>,
#> #   hhtype6 <fct>, hhsizex <dbl+lbl>, famnumx <dbl+lbl>, loncoupx <fct>,
#> #   nlpar <dbl+lbl>, ncouple <dbl+lbl>, nsing <dbl+lbl>, ndepchild <dbl+lbl>,
#> #   nxdepch <dbl+lbl>, otherfam <fct>, othfamlp <fct>, noUnits1 <fct>, …
```
