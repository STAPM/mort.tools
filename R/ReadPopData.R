
#' Load the population counts data
#'
#' Reads and cleans the population count data by age, year, sex and IMD quintile.
#' 
#' \lifecycle{stable}
#'
#' @param path Character - the path to the folder in which the data are stored.
#' @param last_year Integer - the last year for which data is available.
#'
#' @return Returns a data table containing the data for all years.
#' @importFrom data.table := setDT setnames rbindlist copy
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
ReadPopData <- function(
  path,
  last_year = 2018
  ) {

  for(ky in 2001:last_year) {
    
    # Keep track of progress
    cat(ky, "\r")
    utils::flush.console()

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

      setnames(data_y, c("Pops", "Year", "Age", "Sex", "IMD QUINTILE"), 
               c("pops", "year", "ageinyrs", "sex", "imd_quintile"))
    }

    ###################################
    # 2017
    if(ky == 2017) {
      
      data_y <- readxl::read_excel(paste0(path, "/2017-2018/Deaths_Pops_England_2017_IMD2015.xlsx"),
                                   sheet = "ENG_POPS_2017_IMD15",
                                   col_types = c("text", "numeric", "numeric", "text", "text", "numeric"))
      
      setDT(data_y)
      
      setnames(data_y, 
               c("Age", "IMD Quintile", "Sex", "LA code", "LA name"), 
               c("ageinyrs", "imd_quintile", "sex", "laua", "laua name"))
      
      data_y[ , "laua name" := NULL]
      data_y[ , year := 2017]
      
    }
    
    ###################################
    # 2018
    
    # Note that the Health Survey for England 2018 used the IMD 2015
    # The ONS have also supplied us with the 2018 mortality and pop data for the new IMD version
    # (so we can test what difference this makes)
    # but for the standard processing we will use the 2015 version 
    # until the Health Survey for England changes
    
    if(ky == 2018) {
      
      # age variable includes 90+ so is initially treated as text
      
      data_y <- readxl::read_excel(paste0(path, "/2017-2018/Deaths_Pops_England_2018_IMD2015.xlsx"),
                                   sheet = "ENG_POPS_2018_IMD15",
                                   col_types = c("text", "numeric", "numeric", "text", "text", "numeric"))
      
      setDT(data_y)
      
      setnames(data_y, 
               c("Age", "IMD Quintile", "Sex", "LA code", "LA name"), 
               c("ageinyrs", "imd_quintile", "sex", "laua", "laua name"))
      
      data_y[ , "laua name" := NULL]
      data_y[ , year := 2018]
      
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


  }

return(data[])
}

