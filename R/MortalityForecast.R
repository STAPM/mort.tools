
#' Lee-Carter forecast of mortality
#'
#' Implements a Lee-Carter forecast separately for mortality from different causes within 10 subgroups defined by
#' combinations of sex and IMD quintile.
#'
#' Uses the functions from the R package 'demography':
#' \itemize{
#' \item smooths the trends in mortality rates with a 2d moving average over age and year
#' \item fits the Lee-Carter model (singular value decomposition of trends)
#' \item conducts the forecast up to \code{n_years} from baseline
#' }
#'
#' \lifecycle{maturing}
#'
#' @param mort_data_cause Data table containing our processed cause-specific mortality rates
#' @param folder_path where to put the folder of diagnostic files
#' @param cause_names names of the causes to include
#' @param sex_focus Female, Male or both
#' @param folder_label how to identify the forecast folder
#' @param year_range the range of years to use as the basis for forecasting
#' @param age_range the ages for which to conduct the forecast
#' @param smooth TRUE or FALSE
#' @param smooth_n_age the age range for moving average - must be odd number
#' @param smooth_n_year the year range for moving average - must be odd number
#' @param n_years how far in future to project
#' @param standard_pop Data table - the population structure for standardisation.  
#' @param interpolate TRUE or FALSE - should any remaining zeros in the data after smoothing
#' be interpolated based on neighbouring values. This only works if sufficient data exists to inform the interpolation
#' i.e. for diseases with few deaths it might have to be turned off.
#' @importFrom data.table := setDT setnames fread rbindlist dcast copy
#' @importFrom demography read.demogdata lca
#' @importFrom forecast forecast
#' @importFrom grDevices png dev.off
#' @importFrom graphics plot
#' @return
#' \item{mort_data}{Observed mortality}
#' \item{mort_data_smooth}{Smoothed observed mortality}
#' \item{mx_forecast}{Forecast mortality}
#' \item{year_plot}{Age-standardised mortality trends over time}
#' \item{age_plot}{Age pattern of mortality by year}
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' mx_forecast <- MortalityForecast(
#'   mort_data_cause = fread("tob_death_rates_national_2019-05-06_mort.tools_1.0.0.csv")
#' )
#'
#' }
#'
#'
MortalityForecast <- function(
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
) {

  n <- length(age_range)

  # Create new folder to store files
  folder_name <- paste0(folder_path, "/MortalityForecast_", folder_label, "_", format(Sys.time(),'%d_%m__%H_%M'))
  dir.create(folder_name)

  # Tidy mortality and population data
  keep_vars <- c("age", "sex", "imd_quintile", "year", "cause", "n_deaths", "pops")
  mort_data_cause <- copy(mort_data_cause[!is.na(cause) & age %in% 11:89, ..keep_vars])

  # Collapse data to remove stratification by cause
  mort_data <- mort_data_cause[cause %in% cause_names, .(
    n_deaths = sum(n_deaths, na.rm = T),
    pops = unique(pops)
  ), by = c("age", "sex", "imd_quintile", "year")]

  # Population numbers
  pop_counts <- copy(mort_data[age %in% age_range & year %in% year_range & sex %in% sex_focus])
  pop_counts[ , n_deaths := NULL]
  pop_counts <- dcast(pop_counts, year + age ~ sex + imd_quintile,
    value.var = "pops", sep = "x")

  write.table(pop_counts, paste0(folder_name, "/popdata.txt"), sep = "\t", row.names = F)

  # Calculate the central death rates
  mort_data[ , mx := n_deaths / pops]

  mort_data[ , `:=`(n_deaths = NULL, pops = NULL)]

  #######################

  # Loop through subgroups

  counter <- 1

  for(sex_i in sex_focus) {

    #sex_i <- "Female"

    for(imd_quintile_i in c("1_least_deprived", "2", "3", "4", "5_most_deprived")) {

      #imd_quintile_i <- "1_least_deprived"

      # Select the data for one subgroup

      subdata <- copy(mort_data[sex == sex_i & imd_quintile == imd_quintile_i])

      subdata[ , `:=`(sex = NULL, imd_quintile = NULL)]


      # Reshape to wide form with years as columns
      # then make into a matrix

      qdat <- dcast(subdata, age ~ year, value.var = "mx")

      qdat[ , age := NULL]

      qdat <- as.matrix(qdat)

      # Transpose matrix data and get deaths and logrates
      # replace extreme quit probabilities with NA - then fill with approx trend
      # then logit transform

      if(smooth) {

        qdat[is.na(qdat)] <- 0

        #qdat[qdat == 0] <- NA
        #qdat[qdat > .4] <- .4

        # smooth values in a sliding window
        r <- raster::raster(qdat) # convert to rasterLayer
        qdat <- raster::as.matrix(raster::focal(r, matrix(1, smooth_n_age, smooth_n_year), mean, pad = T, padValue = NA, na.rm = T))

        # Fill any remaining missing values

        #qdat[is.na(qdat)] <- 0

      }

      #for(i in 1:n) {
      #  qdat[i,] <- fill.zero(qdat[i,])
      #}

      colnames(qdat) <- 2001:2016
      rownames(qdat) <- 11:89

      qdat <- as.data.frame(qdat)
      qdat$age <- 11:89

      setDT(qdat)

      qdat <- melt(qdat, id.vars = "age", variable.name = "year", value.name = "mx")

      qdat[ , year := as.numeric(as.vector(year))]

      qdat[ , sex := as.character(sex_i)]
      qdat[ , imd_quintile := as.character(imd_quintile_i)]


      if(counter == 1) {
        mort_data_smooth <- copy(qdat)
      } else {
        mort_data_smooth <- rbindlist(list(mort_data_smooth, copy(qdat)), use.names = T)
      }

      counter <- counter + 1
    }
  }


  #######################

  # need mortality rates for each subgroup in wide form in a tab deliminated text file
  mort_data_temp <- dcast(mort_data_smooth[age %in% age_range & year %in% year_range & sex %in% sex_focus],
    year + age ~ sex + imd_quintile,
    value.var = "mx", sep = "x")

  series <- tolower(colnames(mort_data_temp)[3:ncol(mort_data_temp)])

  write.table(mort_data_temp, paste0(folder_name, "/mortalitydata.txt"), sep = "\t", row.names = F)

  # Create demogdata object
  data <- read.demogdata(
    file = paste0(folder_name, "/mortalitydata.txt"),
    popfile = paste0(folder_name, "/popdata.txt"),
    type = "mortality", label = "England", skip = 0)


  # Fit the Lee-Carter model to each data series
  # and implement the forecast
  for(i in series) {

    # Fit Lee-Carter model
    lca_i <- lca(data, series = i, years = year_range, ages = age_range,
      max.age = 90, adjust = "dt", restype = "logrates", interpolate = interpolate)

    png(paste0(folder_name, "/lca_", i, ".png"))
      plot(lca_i)
    dev.off()

    # Implement the forecast
    forecast_i <- forecast(object = lca_i, h = n_years,
      se = "innovdrift", jumpchoice = "actual", level = 95)

    png(paste0(folder_name, "/forecast_", i, ".png"))
      plot(forecast_i)
    dev.off()

    png(paste0(folder_name, "/forecast_components_", i, ".png"))
      plot(forecast_i, "c")
    dev.off()

    # Store the results
    mx_forecast_i <- forecast_i$rate[[1]]
    colnames(mx_forecast_i) <- forecast_i$year

    mx_forecast_i <- melt(mx_forecast_i)
    colnames(mx_forecast_i) <- c("age", "year", "mx")

    setDT(mx_forecast_i)

    mx_forecast_i[ , sex := stringr::str_split_fixed(i, "x", 2)[1]]
    mx_forecast_i[ , imd_quintile := stringr::str_split_fixed(i, "x", 2)[2]]

    if(i == series[1]) {
      mx_forecast <- copy(mx_forecast_i)
    } else{
      mx_forecast <- rbindlist(list(mx_forecast, copy(mx_forecast_i)), use.names = T)
    }

  }

  mx_forecast[sex == "female", sex := "Female"]
  mx_forecast[sex == "male", sex := "Male"]

  # Plots
  data_p <- copy(mort_data)
  data_p[ , agegroup := c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                          "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90+")[findInterval(age, c(-10, seq(5, 90, 5)))]]
  data_p <- merge(data_p, standard_pop, by = "agegroup")
  data_p1 <- data_p[ , .(mx = sum(mx * ESP2013, na.rm = T) / sum(ESP2013)), by = c("year", "sex", "imd_quintile")]

  data_s <- copy(mort_data_smooth)
  data_s[ , agegroup := c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                          "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90+")[findInterval(age, c(-10, seq(5, 90, 5)))]]
  data_s <- merge(data_s, standard_pop, by = "agegroup")
  data_s1 <- data_s[ , .(mx = sum(mx * ESP2013, na.rm = T) / sum(ESP2013)), by = c("year", "sex", "imd_quintile")]

  data_f <- copy(mx_forecast)
  data_f[ , agegroup := c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                          "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90+")[findInterval(age, c(-10, seq(5, 90, 5)))]]
  data_f <- merge(data_f, standard_pop, by = "agegroup")
  data_f1 <- data_f[ , .(mx = sum(mx * ESP2013, na.rm = T) / sum(ESP2013)), by = c("year", "sex", "imd_quintile")]

  year_plot <- ggplot2::ggplot() +
    ggplot2::geom_point(data = data_p1, ggplot2::aes(x = year, y = mx, colour = sex)) +
    ggplot2::geom_line(data = data_s1, ggplot2::aes(x = year, y = mx, colour = sex), linetype = 1) +
    ggplot2::geom_line(data = data_f1, ggplot2::aes(x = year, y = mx, colour = sex), linetype = 2) +
    ggplot2::facet_wrap(~imd_quintile, ncol = 5)

  temp <- data_f[ , .(mx = sum(mx * ESP2013, na.rm = T) / sum(ESP2013)), by = c("year", "age", "sex")]
  age_plot <- ggplot2::ggplot(temp) +
    ggplot2::geom_line(ggplot2::aes(x = age, y = mx, colour = as.factor(year))) +
    ggplot2::facet_wrap(~ sex)

  mort_data[mx == 0, mx := NA]

return(list(
  mort_data = mort_data,
  mort_data_smooth = mort_data_smooth,
  mx_forecast = mx_forecast,
  year_plot = year_plot,
  age_plot = age_plot
  ))
}





















































































