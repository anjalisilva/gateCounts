
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gateCounts

Calculates Cumulative Gate Counts Provided Raw Daily Gate Counts

<!-- badges: start -->

[![GitHub
issues](https://img.shields.io/github/issues/anjalisilva/mixMVPLN)](https://github.com/anjalisilva/mixMVPLN/issues)
[![License](https://img.shields.io/badge/license-MIT-green)](./LICENSE)
![GitHub language
count](https://img.shields.io/github/languages/count/anjalisilva/mixMVPLN)
![GitHub commit activity
(branch)](https://img.shields.io/github/commit-activity/y/anjalisilva/mixMVPLN/master)

<!-- https://shields.io/category/license -->
<!-- badges: end -->

## Description

`gateCounts` is an R package for calculating cumulative gate counts,
provided raw daily gate counts and gate directionality. The package was
developed to improve current methodologies for calculating cumulative
gate counts.

## Installation

To install the latest version of the package:

``` r
require("devtools")
devtools::install_github("anjalisilva/gateCounts", build_vignettes = TRUE)
library("gateCounts")
```

To run the Shiny app:

``` r
gateCounts::runGateCount()
```

## Overview

To list all functions available in the package:

``` r
ls("package:gateCounts")
```

`gateCounts` package contains 2 functions. The *gateCountCumulative*
function calculates cumulative gate counts, provided a numeric vector or
a tibble containing values of raw daily gate counts. The Shiny app
employing *gateCountCumulative* could be run and results could be
visualized using *runGateCount*. For more information, see details
section below.

An overview of the package is illustrated below:

<div style="text-align:center">

<img src="inst/extdata/Overview_mixMVPLN.png" width="800" height="450"/>

<div style="text-align:left">

<div style="text-align:left">

<div style="text-align:left">


## Details

### Introduction

`gateCounts` is an R package for calculating cumulative gate counts,
provided raw daily gate counts and gate directionality. The package was
developed to improve current methodologies for calculating cumulative
gate counts. If gates are bidirectional, the cumulative sum calculated
will be divided by two. Testing has shown that this method of dividing
at the end of calculation will help reduce issues with gate counts that
result from division of daily counts by two and rounding up or down.
Further, negative counts can result during calculation if the counter
has reset or if a lower value has been entered compared to previous day.
The package attempts to correct for both scenarios. The package also
ensures that counts for empty cells, when the daily count was forgotten
to be reported, are accounted for.

## Shiny App

The Shiny app employing could be run and results could be visualized:

``` r
gateCounts::runGateCount()
```

<div style="text-align:center">

<img src="inst/extdata/ShinyApp.png" width="800" height="450"/>

<div style="text-align:left">

## Tutorials

For tutorials, refer to the vignette:

``` r
browseVignettes("gateCounts")
```

## Citation for Package

``` r
citation("gateCounts")
```

Silva, A. (2022). Calculates Cumulative Gate Counts Provided Raw Daily
Gate Counts. Unpublished.

``` r
A BibTeX entry for LaTeX users is

  @misc{,
    title = {Calculates Cumulative Gate Counts Provided Raw Daily Gate Counts},
    author = {A. Silva},
    year = {2022},
    url = {https://github.com/anjalisilva/gateCounts},
  }
```

## Package References

- [Perera, D. and J. M. Nykolaiszyn (2022). Designing an open-source
  application to record library gate counts in response to COVID-19.
  *Journal of Access Services*
  19.](https://www.tandfonline.com/doi/abs/10.1080/15367967.2022.2046006?journalCode=wjas20)

- [Phillips, J. (2016). Determining Gate Count Reliability in a Library
  Setting. *Evidence Based Library and Information Practice*
  19.](https://journals.library.ualberta.ca/eblip/index.php/EBLIP/article/view/27884/20745)

## Maintainer

- Anjali Silva (<anjali@alumni.uoguelph.ca>).

## Contributions

`gateCounts` welcomes issues, enhancement requests, and other
contributions. To submit an issue, use the [GitHub
issues](https://github.com/anjalisilva/gateCounts).

## Acknowledgments

- This work done at University of Toronto, Ontario, Canada
