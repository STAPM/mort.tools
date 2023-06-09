% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReadPopData.R
\name{ReadPopData}
\alias{ReadPopData}
\title{Load the population counts data}
\usage{
ReadPopData(
  path,
  last_year = 2019,
  country = c("England", "Scotland", "Wales")
)
}
\arguments{
\item{path}{Character - the path to the folder in which the data are stored.}

\item{last_year}{Integer - the last year for which data is available.}

\item{country}{Character string - the country: "England", "Scotland", "Wales. Defaults to England.}
}
\value{
Returns a data table containing the data for all years for the country specified.
}
\description{
Reads and cleans the population count data by age, year, sex and IMD quintile.
}
\details{
Population count data for England and Wales are supplied by the Office for National Statistics. 
Population counts for Scotland are supplied by National Records Scotland.   

This function takes the data in the format supplied by the data provider and converts into a standard format 
used in the STAPM modelling. This function is therefore specific to the format of the data sheets 
provided to the University of Sheffield to inform the STAPM modelling.   

The data supplied are stored on a secure virtual machine. The code that uses this function to 
read and process the population count data is run in the virtual machine environment.
}
\examples{

\dontrun{

# England 
data <- ReadPopData(path = "D:/Death and population data",
  last_year = 2019, country = "England")
  
# Scotland
pop_data <- ReadPopData(path = "D:/Scottish death and population data", country = "Scotland")

}

}
