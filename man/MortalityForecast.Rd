% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MortalityForecast.R
\name{MortalityForecast}
\alias{MortalityForecast}
\title{Lee-Carter forecast of mortality \lifecycle{experimental}}
\usage{
MortalityForecast(
  mort_data_cause,
  folder_path = getwd(),
  cause_names = c("Chronic_obstructive_pulmonary_disease"),
  sex_focus = c("Female", "Male"),
  folder_label = NULL,
  year_range = 2001:2016,
  age_range = 11:89,
  smooth = FALSE,
  smooth_n_age = 3,
  smooth_n_year = 3,
  n_years = 50,
  standard_pop = mort.tools::esp2013,
  interpolate = TRUE
)
}
\arguments{
\item{mort_data_cause}{Data table containing our processed cause-specific mortality rates}

\item{folder_path}{where to put the folder of diagnostic files}

\item{cause_names}{names of the causes to include}

\item{sex_focus}{Female, Male or both}

\item{folder_label}{how to identify the forecast folder}

\item{year_range}{the range of years to use as the basis for forecasting}

\item{age_range}{the ages for which to conduct the forecast}

\item{smooth}{TRUE or FALSE}

\item{smooth_n_age}{the age range for moving average - must be odd number}

\item{smooth_n_year}{the year range for moving average - must be odd number}

\item{n_years}{how far in future to project}

\item{standard_pop}{Data table - the population structure for standardisation.}

\item{interpolate}{TRUE or FALSE - should any remaining zeros in the data after smoothing
be interpolated based on neighbouring values. This only works if sufficient data exists to inform the interpolation
i.e. for diseases with few deaths it might have to be turned off.}
}
\value{
\item{mort_data}{Observed mortality}
\item{mort_data_smooth}{Smoothed observed mortality}
\item{mx_forecast}{Forecast mortality}
\item{year_plot}{Age-standardised mortality trends over time}
\item{age_plot}{Age pattern of mortality by year}
}
\description{
Implements a Lee-Carter forecast separately for mortality from different causes within 10 subgroups defined by
combinations of sex and IMD quintile.
}
\details{
Uses the functions from the R package 'demography':
\itemize{
\item smooths the trends in mortality rates with a 2d moving average over age and year
\item fits the Lee-Carter model (singular value decomposition of trends)
\item conducts the forecast up to \code{n_years} from baseline
}
}
\examples{

\dontrun{

mx_forecast <- MortalityForecast(
  mort_data_cause = fread("tob_death_rates_national_2019-05-06_mort.tools_1.0.0.csv")
)

}


}
