
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Processing Mortality Microdata<img src="logo.png" align="right" style="padding-left:10px;background-color:white;" width="100" height="100" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![](https://img.shields.io/badge/doi-10.17605/OSF.IO/WN6RH-green.svg)](https://doi.org/10.17605/OSF.IO/WN6RH)

<!-- badges: end -->

## The Sheffield Tobacco and Alcohol Policy Modelling Platform

This R package was developed as part of the Sheffield Tobacco and
Alcohol Policy Modelling <https://stapm.gitlab.io/> by the [School of
Health and Related Research (ScHARR) at the University of
Sheffield](https://www.sheffield.ac.uk/scharr).

The aim of the research programme is to identify and evaluate approaches
to reducing the harm from tobacco and alcohol, with the aim of improving
commissioning in a public health policy context, i.e. providing
knowledge to support benefits achieved by policymakers.

The two objectives of the research programme are:

- To evaluate the health and economic effects of past trends, policy
  changes or interventions that have affected alcohol consumption and/or
  tobacco smoking
- To appraise the health and economic outcomes of potential future
  trends, changes to alcohol and/or tobacco policy or new interventions

The STAPM modelling is not linked to the tobacco or alcohol industry and
is conducted without industry funding or influence.

## Purpose of making the code open source

The code has been made open source for the following two reasons:

- Transparency. Open science, allowing review and feedback to the
  project team on the code and methods used.
- Methodology sharing. For people to understand the code and methods
  used so they might use aspects of it in their own work, e.g., because
  they are doing something partially related that isn’t exactly the same
  job and might like to ‘dip into’ elements of this code for
  inspiration.

## Stage of testing and development

The code is actively being used in project work. It is being reviewed
and developed all the time; more tests and checks are being added.

The repository is not intended to be maintained by an open source
community wider than the development team.

## Data access

The mortality microdata that this R package is designed to process was
obtained through data access requests to the Office for National
Statistics (for English and Welsh data) and National Records Scotland.

In ScHARR, all data is stored and processed according to the [ScHARR
Information Governance
Policy](https://www.sheffield.ac.uk/scharr/research/igov/policy00). No
mortality microdata is included within this package.

## Data checks

Data checks are brief reports that show the results of the mortality
data processing using the mort.tools package. The data checks help to
visualise the patterns in the data.

- [Scottish national death
  rates](https://stapm.shinyapps.io/scottish_death_rates_dashboard/)

## Code repositories

The code on Github (<https://github.com/STAPM/mort.tools>) is a mirror
of the code in a private Gitlab repository where the actual development
takes place (<https://gitlab.com/stapm/r-packages/mort.tools>). The code
in the Github repository is linked to a repository on the Open Science
Framework, which provides the doi for the package citation
(<https://osf.io/wn6rh/>).

## Citation

Gillespie D, Webster L, Angus C, Brennan A (\[YEAR\]). mort.tools: An R
Package for Processing Mortality Microdata. R package version \[x.x.x\].
University of Sheffield.
<https://stapm.gitlab.io/r-packages/mort.tools/>. doi:
<https://doi.org/10.17605/OSF.IO/WN6RH>

## Motivation

The motivation for `mort.tools` was to develop a set of functions to
clean and do basic processing of the data. The advantage of this is that
it ensures we apply a consistent approach across years of data, data
from different countries, and across the different projects for which we
use the data.

`mort.tools` was created as part of a programme of work on the health
economics of tobacco and alcohol at the School of Health and Related
Research (ScHARR), The University of Sheffield. This programme is based
around the construction of the Sheffield Tobacco and Alcohol Policy
Model (STAPM), which aims to use comparable methodologies to evaluate
the impacts of tobacco and alcohol policies, and investigate the
consequences of clustering and interactions between tobacco and alcohol
consumption behaviours.

## Functionality

mort.tools works with data on annual counts of deaths and mid-year
population sizes. Counts of deaths are grouped according to the set of
tobacco and alcohol related diseases considered in the modelling (see
<https://osf.io/v945r/>). Deaths not falling into one of these
classifications are categorised as ‘all other causes’. Counts of deaths
and mid-year population sizes are stratified by single years of age, sex
and Index of Multiple Deprivation quintiles. For England, further
stratification by local authority was included in the data request to
the Office for National Statistics. The mort.tools R package does not
function without this data. However no datasets are provisioned. Data
must therefore be obtained by data requests to the Office for National
Statistics or National Records Scotland.

The package is primarily designed for users at the University of
Sheffield, working off the university’s networked drives and in the
secure virtual machine data environment. This is where most of the
testing has taken place so there might be unexpected issues out of that
environment.

What the software does in general and how it relates to data is
documented in the vignettes under “Methods and data checks”.

## Usage

See [Get started](mort-tools.html) for example workflows. We have
written a range of short methods descriptions. There is a [reference
list of
functions](https://stapm.gitlab.io/r-packages/mort.tools/reference/index.html)
in this package. Data processing is done on the University of Sheffield
managed secure virtual machine data environment.

The suite of functions within `mort.tools` reads the data for each year,
produces cause-specific life tables (focusing on the diseases related to
tobacco and alcohol), and gives options for conducting forecasts of
cause-specific death rates. The methods for forecasting are still under
development and subject to review.

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

`mort.tools` is publicly available via Github.

By default the user should install the latest tagged version of the
package. Otherwise, if you want to reproduce project work and know the
version of the package used, install that version.

If on a University of Sheffield managed computer, install the R, RStudio
and Rtools bundle from the Software Centre. Install Rtools - using the
[installr](https://cran.r-project.org/web/packages/installr/index.html)
package can make this easier. Then install the latest or a specified
version of `mort.tools` from Github with:

``` r
#install.packages("devtools")

# The tobalcepi STAPM R package is also required
devtools::install_git(
  "https://github.com/stapm/tobalcepi.git", 
  ref = "x.x.x",
  build_vignettes = FALSE)


devtools::install_git(
  "https://github.com/stapm/mort.tools.git", 
  ref = "x.x.x",
  build_vignettes = FALSE)

# ref = "x.x.x" is the version to install - change to the version you want e.g. "1.2.3"
```

Or clone the package repo locally and use the ‘install and restart’
button in the Build tab of RStudio. This option is more convenient when
testing development versions.

Then load the package, and some other packages that are useful. Note
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
