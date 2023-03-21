
# The aim of this code is to install and load the packages needed 
# to calculate Scottish national mortality rates for tobacco- and alcohol related conditions

# Take care to update the package versions where necessary

# Update R ad R studio
#installr::updateR()

# change to local library as needed
project_lib <- "D:/R packages/package_library"

# Create library directory if needed
if(!dir.exists(project_lib)) {dir.create(project_lib)}

.libPaths(project_lib)

###########################
# CRAN packages

# Package names
packages <- c(
  "boot",
  "bit64",
  "bookdown",
  "crayon",
  "cowsay",
  "cowplot",
  "dvmisc",
  "DiagrammeR",
  "data.table",
  "demography",
  "dplyr",
  "devtools",
  "flextable",
  "fastmatch",
  "forecast",
  "ggplot2",
  "ggthemes",
  "git2r",
  "getPass",
  "Hmisc",
  "here",
  "knitr",
  "lifecycle",
  "magrittr",
  "openxlsx",
  "praise",
  "plyr",
  "readxl",
  "roxygen2",
  "readr",
  "Rdpack",
  "Rfast",
  "rmarkdown",
  "raster",
  "RColorBrewer",
  "stringr",
  "testthat",
  "tidyverse",
  "TTR",
  "usethis",
  "VGAM",
  "viridis",
  "writexl")


# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  #install.packages(packages[!installed_packages], type = "source", INSTALL_opts = "--byte-compile")
  install.packages(packages[!installed_packages], lib = project_lib)
  #install.packages(packages[!installed_packages])
}

# Install the STAPM packages
devtools::install_git(
  "https://github.com/stapm/hseclean.git",
  build_vignettes = FALSE, quiet = TRUE)

devtools::install_git(
  "https://github.com/stapm/tobalcepi.git",
  build_vignettes = FALSE, quiet = TRUE)

# The main package is our own mort.tools
# https://stapm.gitlab.io/r-packages/mort.tools/

# STAPM packages are currently installed from the build tab in the package repo on the VM
# for ease of tracking package versions, set the library file path for package installation to D:\R packages\package_library
# the .Rprofile file in this repo points to that path

uname <- "dosgillespie"

devtools::install_git(
  "https://gitlab.com/stapm/r-packages/mort.tools.git",
  credentials = git2r::cred_user_pass(uname, getPass::getPass()),
  ref = "1.6.0",
  build_vignettes = TRUE, quiet = TRUE)

# Load packages
library(mort.tools)
library(stringr)
library(data.table)
library(tobalcepi)




