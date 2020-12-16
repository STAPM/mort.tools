
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Process Mortality Microdata <img src="logo.png" align="right" style="padding-left:10px;background-color:white;" width="100" height="100" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

## Motivation

The motivation for `mort.tools` was to develop a set of functions to
clean and do basic processing of the data. The advantage of this is that
it ensures we apply a consistent approach across years of data and
across the different projects for which we use the data.

`mort.tools` was created as part of a programme of work on the health
economics of tobacco and alcohol at the School of Health and Related
Research (ScHARR), The University of Sheffield. This programme is based
around the construction of the Sheffield Tobacco and Alcohol Policy
Model (STAPM), which aims to use comparable methodologies to evaluate
the impacts of tobacco and alcohol policies, and investigate the
consequences of clustering and interactions between tobacco and alcohol
consumption behaviours.

## Usage

See [Get started](mort-tools.html) for example workflows. We have
written a range of short [technical
descriptions](https://stapm.gitlab.io/r-packages/mort.tools/articles/index.html)
that explain our data and methods. There is a [reference list of
functions](https://stapm.gitlab.io/r-packages/mort.tools/reference/index.html)
in this package. Data processing is done on the University of Sheffield
managed secure `heta-study` virtual machine.

The suite of functions within `mort.tools` reads the data for each year,
produces cause-specific life tables (focusing on the diseases related to
tobacco and alcohol), and gives options for conducting forecasts of
cause-specific death rates.

The **inputs** are the counts of death and mid-year population size.

The **processes** applied by the functions in `mort.tools` give options
to:

1.  Read each year of data.  
2.  Group the data by the causes related to tobacco and alcohol.  
3.  Construct cause-specific life tables.  
4.  Forecast trends in cause-specific mortality.

The **output** of these processes are the trends in cause-specific death
rates and population sizes. These dataset can be saved so that you don’t
need to run the cleaning processes in `mort.tools` each time you want to
use the cleaned data.

## Installation

The `mort.tools` package is currently available only to members of the
project team (but contact Duncan Gillespie
<duncan.gillespie@sheffield.ac.uk> to discuss). To access the package
you need to [sign-up for a GitLab account](https://gitlab.com/). You
will then need to be added to the STAPM project team to gain access.

Once that is sorted, you can install either the latest version or a
specified version from GitLab with:

``` r
#install.packages("devtools")
#install.packages("getPass")

devtools::install_git(
  "https://gitlab.com/stapm/r-packages/mort.tools.git", 
  credentials = git2r::cred_user_pass("uname", getPass::getPass()),
  ref = "x.x.x",
  build_vignettes = TRUE
)

# Where uname is your Gitlab user name.
# ref = "x.x.x" is the version to install - remove this to install the latest version
# this should make a box pop up where you enter your GitLab password
```

You will also need the
[tobalcepi](https://stapm.gitlab.io/r-packages/tobalcepi/) package. Note
that the code within `mort.tools` uses the `data.table::data.table()`
syntax.

``` r
# Load the package
library(data.table)
library(mort.tools)
library(tobalcepi)


# Other useful packages
library(dplyr) # for data manipulation and summary
library(magrittr) # for pipes
library(ggplot2) # for plotting
```

## Citation

Please cite the latest version of the package using:  
“Duncan Gillespie, Laura Webster, Colin Angus and Alan Brennan (2020).
mort.tools: Read, Clean and Analyse Cause-Specific Mortality Data. R
package version x.x.x.
<https://stapm.gitlab.io/r-packages/mort.tools/>.”
