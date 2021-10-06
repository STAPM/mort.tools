
#' Split oesophageal (C15) ICD-10 codes in to SCC or AC
#'
#' Using the data provided to us by CRUK, split deaths marked as due to Oesophageal cancer into
#' deaths from the subtypes SCC and AC.
#'
#' @param data Data table containing counts of death by cause.
#' @param cruk_splits Data table containing the percentages of oesophageal cancer that are SCC and AC.
#' @param n_deaths_var Character - the name of the variable containing the death counts.
#'
#' @return Returns a data table that is an updated version of data with extra rows added to reflect
#' the splitting of oesophageal cancer into two diseases.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data <- SplitOesoph(data)
#'
#' }
#'
SplitOesoph <- function(
  data,
  cruk_splits = oesoph_splits,
  n_deaths_var = "n_deaths"
) {

  # Create age bands to match those used by cruk
  data[, cruk_ageband := c(
    "00-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
    "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")[
      findInterval(age, c(0, seq(5, 85, 5)))]]

  # Separate out oesophagus deaths
  oesophagus_deaths <- data[cause == "Oesophageal"]

  # Merge with cruk splits
  oesophagus_deaths <- merge(oesophagus_deaths, cruk_splits, by = c("cruk_ageband", "sex"), all.x = T, all.y = F)

  # Create SCC deaths
  scc_deaths <- copy(oesophagus_deaths)
  scc_deaths[ , (n_deaths_var) := get(n_deaths_var) * proportion_scc]
  scc_deaths[ , proportion_scc := NULL]
  scc_deaths[ , cause := "Oesophageal_SCC"]

  # Create AC deaths
  ac_deaths <- copy(oesophagus_deaths)
  ac_deaths[ , (n_deaths_var) := get(n_deaths_var) * (1 - proportion_scc)]
  ac_deaths[ , proportion_scc := NULL]
  ac_deaths[ , cause := "Oesophageal_AC"]

  # Strip Oesophagus deaths out of deaths data
  data <- data[cause != "Oesophageal"]

  # Join on the split oesophagus deaths
  data <- rbindlist(list(data, scc_deaths, ac_deaths), use.names = T)
  data[ , cruk_ageband := NULL]


  return(data)
}
