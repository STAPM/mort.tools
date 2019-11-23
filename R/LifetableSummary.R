
#' Calculate lifetable summary statistics
#'
#' Calculates key lifetable summary statistics from mortality rates and population numbers,
#' remaining life expectancy at each age and the expected number of years of life lost to death from each cause.
#'
#' The function assumes that it is dealing with age and period in single years, so adjusting the size
#' of the time interval is not given as an option. The open age interval is 90+.
#'
#' @param mort_data Data table - central rates of death in single years of age and period, stratified by cause.
#' Cause-specific death rates are "mix", stratified by subgroups of "age", "sex", "imd_quintile" and "year".
#' @param pop_data Data table - mid-year population sizes in single years of age and period. 
#' Counts must be in a variable named "N_pop".
#' @param strat_vars Character vector - the variables by which to stratify the summary.
#' @param label Character - the label to append to the calculated variables. Defaults to NULL.
#'
#' @return Returns a data table of the number of deaths and expected years of life lost by cause and subgroup.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' LifetableSummary(mort_data, pop_data)
#' }
#'
#'
LifetableSummary <- function(
  mort_data,
  pop_data,
  strat_vars = c("age_cat", "sex", "imd_quintile", "year", "condition"),
  label = NULL
) {

  # Calculate the probability of death from each cause during and age interval
  mort_data[ , qix := 1 - exp(-mix)]

  # Remove the cause-specific splits by summing the probabilities of death for each subgroup
  ex_data <- mort_data[, .(qx = sum(qix, na.rm = T)), by = c("age", "sex", "imd_quintile", "year")]

  # Add the open age interval, 90+, giving it a probability of death of 1
  ex_data <- rbindlist(list(ex_data, copy(ex_data[age == 89])[, `:=`(age = 90, qx = 1)]), use.names = T)

  # Calculate the probability of survival through the 1 year age interval
  ex_data[, px := 1 - qx]

  # Calculate the survivorship function
  # as the cumulative product of the probability of survival to the start of each age interval
  ex_data[, lx := cumprod(c(1, px[1:(length(px) - 1)])), by = c("sex", "imd_quintile", "year")]

  # Calculate the age distribution of deaths
  ex_data[, phix := qx * lx]

  # Calculate the equivalent of the survivorship function for survival to the middle of each age class
  ex_data[, Lx := lx - .5 * phix]

  # Calculate the expected years of life lived
  ex_data[, Tx := rev(cumsum(rev(Lx))), by = c("sex", "imd_quintile", "year")]

  # Calculate remaining life expectancy
  ex_data[, ex := Tx / lx]

  # Select required variables
  ex_data <- ex_data[, c("age", "sex", "imd_quintile", "year", "ex")]

  # Merge the population level with the cause specific data
  mort_data <- merge(mort_data, ex_data, by = c("age", "sex", "imd_quintile", "year"), all.x = T, all.y = F)

  # Merge with the population data
  mort_data <- merge(mort_data, pop_data, by = c("age", "sex", "imd_quintile", "year"), all.x = T, all.y = F)

  # Calculate and summarise the years of life lost to death from each cause
  data_YLL <- mort_data[ , .(

    n_deaths = sum(N_pop * mix),
    yll = sum(N_pop * mix * ex),
    ex = mean(ex)

  ), by = strat_vars]


  setnames(data_YLL,
           c("n_deaths", "yll", "ex"),
           c(paste0("n_deaths_", label), paste0("yll_", label), paste0("ex_", label))
  )

return(data_YLL)
}










