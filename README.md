<!-- README.md is generated from README.Rmd. Please edit that file -->
harvestR <img src="man/figures/logo.png" align="right" />
=========================================================

Overview
--------

harvestR is a set of wrappers around the Harvest API v2 to allow the user to easily communicate with the API for extracting data, pushing data, and building applications over the API.

Installation
------------

harvestR will not be submitted to CRAN given the numerous dependencies including the Harvest API. The library can be installed from github.

``` r
devtools::install_github("propellerpdx/harvestR")
#> Downloading GitHub repo propellerpdx/harvestR@master
#> from URL https://api.github.com/repos/propellerpdx/harvestR/zipball/master
#> Installation failed: Not Found (404)
```

Extract Data
------------

### get\_harvestR

The get wrapper extracts tables from Harvest by name. It allows the user to pass parameters to limit request size, but without parameters it will pull the entire table.

### write\_harvestR

Coming soon...

Post Data
---------

### post\_harvestR

Coming soon...
