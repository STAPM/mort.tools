

#' Run cause-specific forecasts
#'
#' A wrapper that runs the forecast for each cause and groups the outputs together.
#'
#' @param data Data table containing our processed cause-specific mortality rates
#' @param forecast_params Data table containing the control parameters for the cause specific forecasts
#' @param n_years Integer - the number of years to forecast into the future
#' @param folder_path where to save the outputs - defaults to the current working directory
#' @importFrom data.table := setDT setnames fread rbindlist
#' @return
#' \item{mx_data_cause}{Data table containing the observed and forecast mortality rates by age, sex, IMD quintile and cause.}
#' \item{mx_data_all}{Data table containing the observed and forecast mortality rates for all causes combined.}
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' cforecast <- CombinedForecast(
#'   data = fread("tob_death_rates_national_2019-05-06_mort.tools_1.0.0.csv"),
#'   forecast_params = setDT(readxl::read_xlsx("disease specific mortality forecasting (for function input).xlsx"))
#' )
#'
#' }
#'
#'
CombinedForecast <- function(
  data,
  forecast_params,
  n_years = 2060 - 2016,
  folder_path = getwd()
) {

  jump_year <- max(data$year)

  main_folder_name <- paste0(folder_path, "/MortalityForecast_", format(Sys.time(),'%d_%m__%H_%M'))
  dir.create(main_folder_name)

  cause_names <- unique(forecast_params$Condition)

  # Fill zeros for causes with no deaths
  domain <- data.frame(expand.grid(
    age = 0:90,
    sex = c("Female", "Male"),
    year = 2001:2016,
    imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived"),
    cause = cause_names[cause_names != "all_causes"]
  ))
  setDT(domain)

  data <- merge(data, domain, by = c("age", "sex", "year", "imd_quintile", "cause"), all = T)

  data[is.na(n_deaths), n_deaths := 0]
  data[is.na(mx_cause), mx_cause := 0]
  data[ , pops := unique(pops[!is.na(pops)]), by = c("age", "sex", "year", "imd_quintile")]

  # For each cause
  for(c_name in cause_names) {

    message("\t", c_name)
    #c_name <- cause_names[54]

    if(c_name != "all_causes") {

      # Calculate the average rate by age and subgroup and smooth
      rate_summary <- data[cause == c_name & age %in% 12:89, .(mx_mean = mean(mx_cause)),
                           by = c("age", "sex", "imd_quintile")]

      # Smooth over age
      rate_summary[ , mx_smooth := TTR::SMA(mx_mean, n = 7), by = c("sex", "imd_quintile")]

      # Put zero death rates where missing
      rate_summary[is.na(mx_smooth), mx_smooth := 0]

      rate_summary[ , mx_mean := NULL]

    }

    # Select the forecast parameters for the focal cause
    c_params <- forecast_params[Condition == c_name]

    # Whether to forecast for this cause or not
    do_forecast <- c_params$do_forecast

    # If this cause is being forecast
    if(do_forecast == "yes") {

      # Grab the required parameters
      year_start <- c_params$year_start
      year_end <- c_params$year_end

      age_start <- c_params$age_start
      age_end <- c_params$age_end

      smooth_age <- c_params$smooth_age
      smooth_year <- c_params$smooth_year

      sex_focus <- c("Female", "Male")
      if(c_params$sex_focus == "Female") sex_focus <- "Female"
      if(c_params$sex_focus == "Male") sex_focus <- "Male"

      if(c_name == "all_causes") {
        c_name1 <- cause_names[cause_names != "all_causes"]
      } else {
        c_name1 <- c_name
      }

      # and run the forecast
      # this will also write a subfolder containing diagnostic plots
      c_forecast <- mort.tools::MortalityForecast(
        mort_data_cause = data, # death rates for all causes
        folder_path = main_folder_name, # the folder in which to store all outputs
        cause_names = c_name1, # the cause name
        folder_label = c_name,
        sex_focus = sex_focus,
        year_range = year_start:year_end, # the range of years to use to inform the forecast
        age_range = age_start:age_end, # the ages for which to forecast
        smooth = T,
        smooth_n_age = smooth_age, # the size of the age window to use in smoothing
        smooth_n_year = smooth_year, # the size of the year window to use in smoothing
        n_years = n_years # the number of years into the future to forecast
      )

      domain <- data.frame(expand.grid(
        age = 12:89,
        sex = c("Female", "Male"),
        year = (jump_year + 1):(jump_year + n_years),
        imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived"),
        cause = c_name
      ))
      setDT(domain)

      # Merge in the forecast rates
      domain <- merge(domain, c_forecast$mx_forecast, by = c("age", "sex", "imd_quintile", "year"), all.x = T)

      if(c_name != "all_causes") {

        # Merge in the average observed rates
        domain <- merge(domain, rate_summary, by = c("age", "sex", "imd_quintile"), all.x = T)

        # Where forecast rates are missing, fill with average rates
        domain[is.na(mx), mx := mx_smooth]
        domain[ , mx_smooth := NULL]

      }

      # Append forecast to observed rates
      obs_data <- copy(c_forecast$mort_data_smooth)
      obs_data[ , cause := c_name]

      domain <- rbindlist(list(obs_data, domain), use.names = T)

    }

    if(do_forecast == "no") {

      domain <- data.frame(expand.grid(
        age = 12:89,
        sex = c("Female", "Male"),
        year = 2001:(jump_year + n_years),
        imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived"),
        cause = c_name
      ))
      setDT(domain)

      # Merge in the forecast rates
      domain <- merge(domain, rate_summary, by = c("age", "sex", "imd_quintile"), all.x = T)

      setnames(domain, "mx_smooth", "mx")

    }

    if(c_name == cause_names[1]) {
      data_store <- copy(domain)
    } else{
      data_store <- rbindlist(list(data_store, domain), use.names = T)
    }

  }

  return(list(
    mx_data_cause = data_store[cause != "all_causes"],
    mx_data_all = data_store[cause == "all_causes"]
  ))
}











