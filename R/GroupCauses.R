

#' Aggregate ICD-10 codes by cause group \lifecycle{stable}
#'
#' Groups ICD-10 codes based on the groups of causes specified in our lists of diseases attributable
#' to tobacco, alcohol or both.
#'
#' Identifies the ICD-10 codes that fall within each cause group.
#' For conditions that we consider only to occur in females (Breast, Cervix cancer, and hip fracture)
#' assign any occurance in males as 'other causes'. Finally, collapse the data by summing the death counts
#' within each cause group.
#'
#' Before running this function you will need to run ExpandCodes() from the tobalcepi package.
#'
#' 
#'
#' @param data_file Data table containing death counts ("n_deaths") by individual ICD10 codes.
#' @param lookup_file Data table showing how ICD-10 codes are grouped by cause.
#' @param strat_vars Character vector - the variables by which the dataset output should be stratified.
#' @importFrom data.table :=
#'
#' @return Returns a data table of death counts by cause group and stratification variables.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data <- GroupCauses(
#'   data_file = copy(data),
#'   lookup_file = copy(lookups),
#'   strat_vars = c("year", "age", "sex", "imd_quintile")
#' )
#'
#' }
#'
GroupCauses <- function(
  data_file,
  lookup_file,
  strat_vars = c("year", "age", "sex", "imd_quintile", "laua")
) {

  # Begin by searching for matches based on just the first three characters of the ICD-10 code.
  # Many of the ICD-10 codes just have 3 characters e.g. X14 but some have 4 characters.
  # Matching on 4 characters will be done after this.

  # 3 character ICD-10 matching.


  data_file[ , cause3 := lookup_file[match(substr(data_file$icd10, 1, 3), lookup_file$icd10_lookup), "condition"]]

  # 4 character ICD-10 matching.
  data_file[ , cause4 := lookup_file[match(substr(data_file$icd10, 1, 4), lookup_file$icd10_lookup), "condition"]]

  # Indicate if the disease code matches with a cause on the list.
  data_file[ , cause3n := as.numeric(!is.na(cause3))]
  data_file[ , cause4n := as.numeric(!is.na(cause4))]

  # If there is a match, then label that with the cause name, otherwise label as "Other_cause".
  data_file[ , cause := "other_causes"]
  data_file[cause3n > 0, cause := as.character(cause3)]
  data_file[cause4n > 0, cause := as.character(cause4)]

  # Check that for the conditions we consider only to occur in females
  # where it is recorded for males, then it is marked as other causes
  data_file[sex == "Male" & cause == "Breast", cause := "other_causes"]
  data_file[sex == "Male" & cause == "Cervical", cause := "other_causes"]
  data_file[sex == "Male" & cause == "Hip_fracture", cause := "other_causes"]

  # Collapse the deaths data_file into cause groups
  data_file <- data_file[ , .(n_deaths = sum(n_deaths, na.rm = T)), by = c(strat_vars, "cause")]

  # Split deaths from oesophageal cancer into two subtypes
  data_file <- mort.tools::SplitOesoph(data_file)

return(data_file)
}
