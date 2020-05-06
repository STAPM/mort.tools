
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mort.tools <img src="tools/mort.tools_hex.png" align="right" style="padding-left:10px;background-color:white;" width="100" height="100" />

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3800592.svg)](https://doi.org/10.5281/zenodo.3800592)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

**DRAFT WORKING VERSION** - The package is usable but there are still
bugs and further developments that are being worked through i.e. some
code and documentation is still incomplete or in need of being refined.
The code and documentation are still undergoing internal review by the
analyst team.

## Motivation

`mort.tools` was created as part of a programme of work on the health
economics of tobacco and alcohol at the School of Health and Related
Research (ScHARR), The University of Sheffield. This programme is based
around the construction of the Sheffield Tobacco and Alcohol Policy
Model (STAPM), which aims to use comparable methodologies to evaluate
the impacts of tobacco and alcohol policies, and investigate the
consequences of clustering and interactions between tobacco and alcohol
consumption behaviours.

The motivation for `mort.tools` was to develop a set of functions to
clean and do basic processing of the data. The advantage of this is that
it ensures we apply a consistent approach across years of data and
across the different projects for which we use the data.

## Usage

The suite of functions within `mort.tools` reads the data for each year,
produces cause-specific life tables (focusing on the diseases related to
tobacco and alcohol), and gives options for conducting forecasts of
cause-specific death rates using the Lee-Carter method.

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

We would like to ask that since the code and documentation is still
under development and is complex, that you consult with the authors
before you use it.

Please cite the latest version of the package using:  
“Duncan Gillespie, Laura Webster, Colin Angus and Alan Brennan (2020).
mort.tools: Read, Clean and Analyse Cause-Specific Mortality Data. R
package version x.x.x. <https://STAPM.github.io/mort.tools/>. DOI:
10.5281/zenodo.3800592.”

-----

You can **install the development version of `mort.tools`** from github
with:

``` r
#install.packages("devtools")
devtools::install_github("STAPM/mort.tools")
```

-----

If there is an error with `install_github()`, or for data security
reasons you need to use the package on a computer with no internet
connection, one possible work-around is

1.  Download the package “tarball” by copying this into your internet
    browser (making sure the numbers at the end indicate the latest
    version) e.g. `https://github.com/STAPM/mort.tools/tarball/1.0.0`.
    When the window pops up, choose where to save the .tar.gz file.

2.  Go to the Terminal window in R Studio (or a console window in
    Windows by searching for “cmd”) and install the package from the
    downloaded file by typing `R CMD INSTALL [file path]`.

-----

Then load the package, and some other packages that are useful. Note
that the code within `mort.tools` uses the `data.table::data.table()`
syntax.

``` r
# Load the package
library(mort.tools)

# Other useful packages
library(dplyr) # for data manipulation and summary
library(magrittr) # for pipes
library(ggplot2) # for plotting
```
