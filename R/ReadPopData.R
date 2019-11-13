

#' Load the population counts data
#'
#' Reads and cleans the population count data by age, year, sex and IMD quintile.
#'
#' @param path Character - the path to the folder in which the data are stored.
#'
#' @return Returns a data table containing the data for all years.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data <- ReadPopData("D:/Death and population data")
#'
#' }
#'
ReadPopData <- function(path) {

  for(ky in 2001:2016) {

    ###################################
    # 2001-2014
    if(ky %in% 2001:2014) {

      data_y <- readxl::read_excel(paste0(path, "/2001-2014/LApops_", ky, ".xlsx"),
        col_types = c("numeric", "numeric", "numeric", "text", "text", "text"))

      setDT(data_y)

      setnames(data_y, c("age", "IMD_quintile"), c("ageinyrs", "imd_quintile"))
    }

    ###################################
    # 2015
    if(ky == 2015) {

      data_y <- readxl::read_excel(paste0(path, "/2015/pops_2015.xlsx"),
        col_types = c("numeric", "numeric", "numeric", "text", "text", "text"))

      setDT(data_y)

      setnames(data_y, c("age", "IMD_quintile"), c("ageinyrs", "imd_quintile"))
    }

    ###################################
    # 2016
    if(ky == 2016) {

      data_y <- fread(paste0(path, "/2016/Pops_Eng_Laua16.csv"))

      data_y[ , "laua name" := NULL]

      setnames(data_y, c("Pops", "Year", "Age", "Sex", "IMD QUINTILE"), c("pops", "year", "ageinyrs", "sex", "imd_quintile"))
    }

    ###################################
    # Clean

    data_y[ , ageinyrs := as.character(ageinyrs)]
    data_y[ageinyrs == "90+", ageinyrs := "90"]
    data_y[ , ageinyrs := as.vector(as.numeric(ageinyrs))]
    setnames(data_y, "ageinyrs", "age")

    data_y[ , sex := as.character(c("Male", "Female")[sex])]

    data_y[ , imd_quintile := plyr::revalue(as.character(imd_quintile),
                                    c("1" = "5_most_deprived",
                                      "2" = "4",
                                      "4" = "2",
                                      "5" = "1_least_deprived"))]

    ###################################
    # Stick years together

    if(ky == 2001) {
      data <- copy(data_y)
    }

    if(ky > 2001) {
      data <- rbindlist(list(data, copy(data_y)), use.names = T)
    }

    # Keep track of progress
    cat(ky, "\r")
    flush.console()
  }

return(data)
}

