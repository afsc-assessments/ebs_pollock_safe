
<!-- README.md is generated from README.Rmd. Please edit that file -->

# East Bering Sea pollock assessment repository

This repository depends mainly on the R package `ebswp` was developed
for doing stock assessments of eastern Bering Sea walleye pollock for
the AFSC. The stock assessment model was coded using the Autodif Model
Builder (`ADMB`) software. The `ebswp` package is used to run the model,
summarize the results, and plot the results. The package is designed to
be user-friendly and to provide a consistent interface for running the
model and summarizing the results.

## Getting started

``` r
git clone https://github.com/afsc-assessments/ebswp
```

Presently, the assessment has both the 2023 version (ebswp.qmd) which
executes R/pm23.R and recreates the 2023 assessment. The initial “new”
layout for the 2024 version is set up as a quarto book via the
\_quarto.yml file. We need to ensure that variables declared in R in
chunks is available throughout each of the child documents (this is why
I went with a single file in 2023).

## Installation

There are several options for installing the `ebswp` R package.

### Option 1

The `ebswp` package can be installed from within R using:

``` r
devtools::install_github(repo = "afsc-assessments/ebswp", dependencies = TRUE, 
                         build_vignettes = TRUE, auth_token = "your_PAT")
```

### Option 2

The GitHub repository can be cloned to your computer and the package
installed from the command line. From Linux this would involve:

``` r
git clone https://github.com/afsc-assessments/ebswp
R CMD INSTALL ebswp
```

### Option 3

This time from within R using:

``` r
devtools::install("ebswp")
```

## Help

Help for all `ebswp` functions and data sets can be found on the R help
pages associated with each function and data set. Help for a specific
function can be viewed using `?function_name`, for example:

``` r
?run_model
?tab_fit
?plot_sel
```

Alternatively, to see a list of all available functions and data sets
use:

``` r
help(package = "ebswp")
```

## Examples

The package vignettes are a great place to see what `ebswp` can do. You
can view the package vignettes from within R using:

``` r
browseVignettes(package = "ebswp")
vignette(topic = "ebswp", package = "ebswp")
```

## Website

All of the vignettes and the help pages for each function are bundled
together and published on the website
<https://afsc-assessments.github.io/ebswp/>.

## Developers

Developers will want to do things slightly differently. See the
`Model development` vignette.

# Acronymns

NOAA: National Oceanic and Atmospheric Administration  
NMFS: National Marine Fisheries Service  
AFSC: Alaska Fisheries Science Center  
REFM: Resource and Ecology and Fisheries Management

### NOAA README

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.

### NOAA License

Software code created by U.S. Government employees is not subject to
copyright in the United States (17 U.S.C. §105). The United
States/Department of Commerce reserve all rights to seek and obtain
copyright protection in countries other than the United States for
Software authored in its entirety by the Department of Commerce. To this
end, the Department of Commerce hereby grants to Recipient a
royalty-free, nonexclusive license to use, copy, and create derivative
works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" height="75" alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) \|
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
