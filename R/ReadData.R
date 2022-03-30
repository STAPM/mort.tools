
#' Read data on counts of deaths
#'
#' Loads and cleans the data on counts of deaths provided by the data owner, e.g. the Office for National Statistics, 
#' clean these data and make them ready for calculating cause-specific rates of death.
#'
#' The counts of death provided to inform the STAPM modelling 
#' come in a time series of many years so that ongoing trends in death rates can be estimated 
#' and input into the model micro-simulation.   
#' 
#' The data on counts of death are processed into a filtered and summarised form by an analyst 
#' at the data owner before being transferred to the STAPM team for further processing.  
#' 
#' The counts of death are all stratified by single years of age and year, sex (m/f) and quintiles of the index of multiple deprivation. 
#' Depending on the project for which the data is required, the death counts might also be stratified by local authority.   
#'
#'
#' @param path Character - the path to the folder in which the data are stored.
#' @param country Character string - the country for which the death rates are being calculated - "England", "Wales", "Scotland". 
#' Defaults to England.  
#' @param supp_data Optional input of supplementary information to support processing. Defaults to NULL.
#'
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
ReadData <- function(path, 
                     country = "England",
                     supp_data = NULL) {
  
  
  ##############################################################################
  ########################### England ##########################################
  ##############################################################################
  
  if(country == "England") {
    
    #############################################
    # 2001-2014
    
    # Read deaths data (2002 - 2014)
    # 2001 and 2015 don't have deaths pooled into an open age interval of 90+ -
    # so read and process these data separately.
    # 2015 was provided in a data update and is in a separate file,
    # as is the latest data update for 2016
    sheet_names <- c("2001", "2002-2003", "2004-2005", "2006-2007",
                     "2008-2009", "2010-2011", "2012-2013", "2014")
    
    for(sn in sheet_names) {
      
      # The deaths data for each year is stored within a different workbook in the
      # same excel spreadsheet.
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
      
      # Select only English local authorities
      data_y <- data_y[stringr::str_detect(laua, "E")]
      
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
    
    # Select only English local authorities
    data_y <- data_y[stringr::str_detect(laua, "E")]
    
    data <- rbindlist(list(data, data_y), use.names = TRUE)
    
    # Keep track of progress
    cat("2015", "\r")
    flush.console()
    
    #############################################
    # 2016
    data_y <- fread(paste0(path, "/2016/Deaths_Eng_2016.csv"),
                    colClasses = c("numeric", "numeric", "numeric", "numeric",
                                   "character", "character", "numeric", "character", "character"))
    
    setnames(data_y, colnames(data_y), tolower(colnames(data_y)))
    setnames(data_y, c("year of registration", "age", "imd quintile", "icd"),
             c("regyr", "ageinyrs", "imd_quintile", "icd10u"))
    
    data_y[ , ("laua name") := NULL]
    
    data <- rbindlist(list(data, data_y), use.names = TRUE)
    
    # Keep track of progress
    cat("2016", "\r")
    utils::flush.console()
    
    #############################################
    # 2017
    data_y <- readxl::read_excel(paste0(path, "/2017-2018/Deaths_Pops_England_2017_IMD2015.xlsx"),
                                 sheet = "ENG_DEATHS_2017_IMD15",
                                 col_types = c("numeric", "numeric", "numeric", "text",
                                               "numeric", "text", "text", "numeric"))
    
    setDT(data_y)
    
    setnames(data_y, colnames(data_y), tolower(colnames(data_y)))
    
    setnames(data_y,
             c("registration year", "age", "imd quintile", "icd-10 code", "la code",
               "underlying cause"),
             c("regyr", "ageinyrs", "imd_quintile", "icd10u", "laua", "cause"))
    
    data <- rbindlist(list(data, data_y), use.names = TRUE)
    
    # Keep track of progress
    cat("2017", "\r")
    utils::flush.console()
    
    #############################################
    # 2018
    
    # Note that the Health Survey for England 2018 used the IMD 2015
    # The ONS have also supplied us with the 2018 mortality and pop data for the new IMD version
    # (so we can test what difference this makes)
    # but for the standard processing we will use the 2015 version
    # until the Health Survey for England changes
    
    data_y <- readxl::read_excel(paste0(path, "/2017-2018/Deaths_Pops_England_2018_IMD2015.xlsx"),
                                 sheet = "ENG_DTHS_2018_IMD15",
                                 col_types = c("numeric", "numeric", "numeric", "text", "numeric",
                                               "text", "text", "numeric"))
    
    setDT(data_y)
    
    setnames(data_y, colnames(data_y), tolower(colnames(data_y)))
    
    setnames(data_y,
             c("registration year", "age", "imd quintile", "icd-10 code", "la code",
               "underlying cause"),
             c("regyr", "ageinyrs", "imd_quintile", "icd10u", "laua", "cause"))
    
    data <- rbindlist(list(data, data_y), use.names = TRUE)
    
    # Keep track of progress
    cat("2018", "\r")
    utils::flush.console()
    
    #############################################
    # 2019
    
    data_y <- readxl::read_excel(paste0(path, "/2019/Eng_Dths_2019_Final.xlsx"),
                                 sheet = "England_Deaths_2019",
                                 col_types = c("numeric", "text", "text", "numeric", "numeric",
                                               "text", "text", "text", "numeric"))
    
    setDT(data_y)
    
    setnames(data_y, colnames(data_y), tolower(colnames(data_y)))
    
    setnames(data_y,
             c("imd quintile", "fic10und"),
             c("imd_quintile", "icd10u"))
    
    data_y[ , lad20nm := NULL]
    
    data <- rbindlist(list(data, data_y), use.names = TRUE)
    
    # Keep track of progress
    cat("2019", "\r")
    utils::flush.console()
    
    #############################################
    
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
    
    
  }
  
  
  ##############################################################################
  ########################### Wales ############################################
  ##############################################################################
  
  if(country == "Wales") {
    
    #############################################
    # 2001-2014
    
    # Read deaths data (2002 - 2014)
    # 2001 and 2015 don't have deaths pooled into an open age interval of 90+ -
    # so read and process these data separately.
    # 2015 was provided in a data update and is in a separate file,
    # as is the latest data update for 2016
    sheet_names <- c("2001", "2002-2003", "2004-2005", "2006-2007",
                     "2008-2009", "2010-2011", "2012-2013", "2014")
    
    for(sn in sheet_names) {
      
      # The deaths data for each year is stored within a different workbook in the
      # same excel spreadsheet.
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
      
      # Select only English local authorities
      data_y <- data_y[stringr::str_detect(laua, "W")]
      
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
    
    # Select only English local authorities
    data_y <- data_y[stringr::str_detect(laua, "W")]
    
    data <- rbindlist(list(data, data_y), use.names = TRUE)
    
    # Keep track of progress
    cat("2015", "\r")
    flush.console()
    
    #############################################
    # 2016
    data_y <- fread(paste0(path, "/2016/Deaths_Wal_2016.csv"),
                    colClasses = c("numeric", "numeric", "numeric", "numeric",
                                   "character", "character", "numeric", "character", "character"))
    
    setnames(data_y, colnames(data_y), tolower(colnames(data_y)))
    setnames(data_y, c("year of registration", "age", "imd quintile", "icd"),
             c("regyr", "ageinyrs", "imd_quintile", "icd10u"))
    
    data_y[ , ("laua name") := NULL]
    
    data <- rbindlist(list(data, data_y), use.names = TRUE)
    
    # Keep track of progress
    cat("2016", "\r")
    utils::flush.console()
    
    #############################################
    # 2017
    data_y <- readxl::read_excel(paste0(path, "/2017-2018/Deaths_Pops_Wales_2017_IMD2014.xlsx"),
                                 sheet = "DTHS_2017_WIMD_14",
                                 col_types = c("numeric", "numeric", "numeric", "text",
                                               "numeric", "text", "text", "numeric"))
    
    setnames(data_y, colnames(data_y), tolower(colnames(data_y)))
    
    setnames(data_y,
             c("registration year", "age", "imd quintile", "icd-10 code", "la code",
               "underlying cause"),
             c("regyr", "ageinyrs", "imd_quintile", "icd10u", "laua", "cause"))
    
    data <- rbindlist(list(data, data_y), use.names = TRUE)
    
    # Keep track of progress
    cat("2017", "\r")
    utils::flush.console()
    
    #############################################
    # 2018
    
    # Note that the Health Survey for England 2018 used the IMD 2015
    # The ONS have also supplied us with the 2018 mortality and pop data for the new IMD version
    # (so we can test what difference this makes)
    # but for the standard processing we will use the 2015 version
    # until the Health Survey for England changes
    
    data_y <- readxl::read_excel(paste0(path, "/2017-2018/Deaths_Pops_Wales_2018_IMD2014.xlsx"),
                                 sheet = "DTHS_2018_IMD_2014",
                                 col_types = c("numeric", "numeric", "numeric", "text", "numeric",
                                               "text", "text", "numeric"))
    
    setDT(data_y)
    
    setnames(data_y, colnames(data_y), tolower(colnames(data_y)))
    
    setnames(data_y,
             c("registration year", "age", "imd quintile", "icd-10 code", "la code",
               "underlying cause"),
             c("regyr", "ageinyrs", "imd_quintile", "icd10u", "laua", "cause"))
    
    data <- rbindlist(list(data, data_y), use.names = TRUE)
    
    # Keep track of progress
    cat("2018", "\r")
    utils::flush.console()
    
    #############################################
    # 2019
    
    data_y <- readxl::read_excel(paste0(path, "/2019/Wal_Dths_2019_Final.xlsx"),
                                 sheet = "Wales_Deaths_2019",
                                 col_types = c("numeric", "text", "text", "numeric", "numeric",
                                               "text", "text", "text", "numeric"))
    
    setDT(data_y)
    
    setnames(data_y, colnames(data_y), tolower(colnames(data_y)))
    
    setnames(data_y,
             c("imd quintile", "fic10und"),
             c("imd_quintile", "icd10u"))
    
    data_y[ , lad20nm := NULL]
    
    data <- rbindlist(list(data, data_y), use.names = TRUE)
    
    # Keep track of progress
    cat("2019", "\r")
    utils::flush.console()
    
    #############################################
    
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
    
    
  }
  
  ##############################################################################
  ########################### Scotland #########################################
  ##############################################################################
  
  if(country == "Scotland") {
    
    #path <- "D:/Scottish death and population data"
    
    data <- read.csv(paste0(path, "/Sheffield Univ - Duncan Gillespie - Tobacco-related deaths - CSV file.csv"))
    
    setDT(data)
    
    data[ , sex := as.character(plyr::revalue(sex, c("F" = "Female", "M" = "Male")))]
    
    data[ , SIMD16quintile := plyr::revalue(as.character(SIMD16quintile),
                                            c("1" = "5_most_deprived",
                                              "2" = "4",
                                              "4" = "2",
                                              "5" = "1_least_deprived"))]
    
    # Remove ages below 11 years and records where age not known
    data[ , `:=`(AllAges = NULL, age0to10 = NULL, ageNK = NULL)]
    
    # Cause of death
    
    # Data supplied with numeric codes indicating cause
    # the file tobacco_icd10_lookups should be supplied to this processing code as supplementary data (as a data.table)
    
    #supp_data <- read.csv(paste0(path, "/tobacco_icd10_lookups.csv"))
    #setDT(supp_data)
    
    data <- merge.data.table(data, supp_data, all.x = T, all.y = F, by = "CauseOfDeath")
    
    data[ , `:=`(icd10_lookups = NULL, CauseOfDeath = NULL)]
    
    data <- melt.data.table(data, id.vars = c("yr", "sex", "SIMD16quintile", "condition"), value.name = "n_deaths", variable.name = "age")
    
    data[ , age := stringr::str_remove_all(age, "age")]
    data[ , age := stringr::str_remove_all(age, "plus")]
    data[ , age := as.vector(as.numeric(age))]
    
    setnames(data, c("yr", "SIMD16quintile", "condition"), c("year", "imd_quintile", "cause"))
    
  }
  
  
  data[ , country := country]
  
  
  
  return(data)
}
