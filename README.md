
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ehsr \[under development\]

<!-- badges: start -->
<!-- badges: end -->

A work-in-progress R package to facilitate the importation of English
Housing Survey (EHS) data files that you have already downloaded from
the UK Data Service. When pointed to a folder where the EHS files are
saved, `ehsr` can identify and import the files for a given year or
series of years, join them together into a single object, standardise
variable names and convert labelled values to factors.

Your use of data downloaded from the UK Data Service must be consistent
with the requirements and conditions of the UKDS that you agreed to when
accessing the data.

## Installation

You can install the development version of `ehsr` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("jgleeson/ehsr")
```

## Example

`ehsr` currently contains three functions, which import just the
‘derived’ EHS household datasets, a defined set of variables from the
detailed household datasets, or a defined set from the detailed housing
stock datasets.

If you have already downloaded the EHS special licence data for 2022 and
have assigned the path where your EHS datasets are saved to the ‘folder’
object, the first function is used as follows.

``` r
library(ehsr)

d <- hh_derived(folder, 2022)
```
