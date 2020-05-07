
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mort.tools <img src="tools/mort.tools_hex.png" align="right" style="padding-left:10px;background-color:white;" width="100" height="100" />

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

`mort.tools` is currently available only to members of the project team
(but please contact Duncan Gillespie <duncan.gillespie@sheffield.ac.uk>
to discuss). To access you need to [**sign-up for a GitLab
account**](https://gitlab.com/). You will then need to be added to the
STAPM project team to gain access.

Once that is sorted, you can **install the development version** from
GitLab with:

``` r
#install.packages("devtools")

devtools::install_git(
  "https://gitlab.com/stapm/mort.tools.git", 
  credentials = git2r::cred_user_pass("uname", getPass::getPass())
)

# Where uname is your Gitlab user name.
# this should make a box pop up where you enter your GitLab password
```

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

## Citation

Please cite the latest version of the package using:  
“Duncan Gillespie, Laura Webster, Colin Angus and Alan Brennan (2020).
mort.tools: Read, Clean and Analyse Cause-Specific Mortality Data. R
package version x.x.x. <https://stapm.gitlab.io/mort.tools/>.”
