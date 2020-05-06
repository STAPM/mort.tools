

#' Read ONS mortality microdata
#'
#' Loads and cleans the microdata provided by the ONS into a clean form ready for further processing.
#'
#' The ONS data was provided originally for years 2001-2014, with subsequent annual updates for 2015 and 2016.
#' The data is processed by an ONS analyst initially before being transferred to us.
#'
#' @param path Character - the path to the folder in which the data are stored.
#' @importFrom data.table := setDT setnames fread rbindlist dcast copy
#' @return Returns a data table containing the data for all years.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data <- ReadData("D:/Death and population data")
#'
#' }
#'
ReadData <- function(path) {

  #############################################
  # 2001-2014

  # Read deaths data (2002 - 2014)
  # 2001 and 2015 don't have deaths pooled into an open age interval of 90+ - so read and process these data separately.
  # 2015 was provided in a data update and is in a separate file, as is the latest data update for 2016
  sheet_names <- c("2001", "2002-2003", "2004-2005", "2006-2007", "2008-2009", "2010-2011", "2012-2013", "2014")

  for(sn in sheet_names) {

    # The deaths data for each year is stored within a different workbook in the same excel spreadsheet.
    # This function reads in the data for the year specified.
    data_y <- readxl::read_excel(
      path = paste0(path, "/2001-2014/deaths_2001_2014.xlsx"),
      sheet = sn,
      col_types = c("numeric", "numeric", "text", "numeric", "text", "numeric", "text", "text")
    )

    setDT(data_y)

    setnames(data_y, colnames(data_y), tolower(colnames(data_y)))

    # Clean age variable
    data_y[ageinyrs == "90+", ageinyrs := "90"]

    # Keep track of progress
    cat(sn, "                 \r")
    flush.console()

    # Bind the years of data together
    if(sn == "2001") {
      data <- data_y
    } else {
      data <- rbindlist(list(data, data_y), use.names = TRUE)
    }
  }

  #############################################
  # 2015
  data_y <- readxl::read_excel(
    path = paste0(path, "/2015/deaths_2015.xlsx"),
    sheet = "Death2015",
    col_types = c("numeric", "numeric", "text", "numeric", "text", "numeric", "text", "text")
  )

  setDT(data_y)

  setnames(data_y, colnames(data_y), tolower(colnames(data_y)))

  data <- rbindlist(list(data, data_y), use.names = TRUE)

  # Keep track of progress
  cat("2015", "\r")
  flush.console()

  #############################################
  # 2016
  data_y <- fread(paste0(path, "/2016/Deaths_Eng_2016.csv"),
                  colClasses = c("numeric", "numeric", "numeric", "numeric", "character", "character", "numeric", "character", "character"))

  setnames(data_y, colnames(data_y), tolower(colnames(data_y)))
  setnames(data_y, c("year of registration", "age", "imd quintile", "icd"), c("regyr", "ageinyrs", "imd_quintile", "icd10u"))

  data_y[ , ("laua name") := NULL]

  data <- rbindlist(list(data, data_y), use.names = TRUE)

  # Keep track of progress
  cat("2016", "\r")
  utils::flush.console()

  # Clean age variable
  data[ , age := as.vector(as.numeric(ageinyrs))]
  data[ , ageinyrs := NULL]
  data[age > 90, age := 90]

  # sex 1 = male, 2 = female
  data[ , sex := as.character(c("Male", "Female")[sex])]

  # The ONS code IMD quintile levels the opposite to us, so flip.
  data[ , imd_quintile := plyr::revalue(as.character(imd_quintile),
                                  c("1" = "5_most_deprived",
                                    "2" = "4",
                                    "4" = "2",
                                    "5" = "1_least_deprived"))]

  data[ , cause := NULL]

  setnames(data, c("deaths", "regyr", "icd10u"), c("n_deaths", "year", "icd10"))

return(data)
}
