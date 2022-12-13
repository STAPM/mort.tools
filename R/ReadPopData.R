
#' Load the population counts data
#'
#' Reads and cleans the population count data by age, year, sex and IMD quintile.    
#' 
#' Population count data for England and Wales are supplied by the Office for National Statistics. 
#' Population counts for Scotland are supplied by National Records Scotland.   
#' 
#' This function takes the data in the format supplied by the data provider and converts into a standard format 
#' used in the STAPM modelling. This function is therefore specific to the format of the data sheets 
#' provided to the University of Sheffield to inform the STAPM modelling.   
#' 
#' The data supplied are stored on a secure virtual machine. The code that uses this function to 
#' read and process the population count data is run in the virtual machine environment.  
#'
#' @param path Character - the path to the folder in which the data are stored.
#' @param last_year Integer - the last year for which data is available.
#' @param country Character string - the country: "England", "Scotland", "Wales. Defaults to England.
#'
#' @return Returns a data table containing the data for all years for the country specified.  
#' 
#' @importFrom data.table := setDT setnames rbindlist copy
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # England 
#' data <- ReadPopData(path = "D:/Death and population data",
#'   last_year = 2019, country = "England")
#'   
#' # Scotland
#' pop_data <- ReadPopData(path = "D:/Scottish death and population data", country = "Scotland")
#' 
#' }
#'
ReadPopData <- function(
  path,
  last_year = 2019,
  country = c("England", "Scotland", "Wales")
) {
  
  ##############################################################################
  ########################### England ##########################################
  ##############################################################################
  
  if(country == "England") {
    
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
        
        # Select only English local authorities
        #pop_data <- pop_data[str_sub(laua, 1, 1) == "E"]
        data_y <- data_y[stringr::str_detect(laua, "E")]
        
        setnames(data_y, c("age", "IMD_quintile"), c("ageinyrs", "imd_quintile"))
      }
      
      ###################################
      # 2015
      if(ky == 2015) {
        
        data_y <- readxl::read_excel(paste0(path, "/2015/pops_2015.xlsx"),
                                     col_types = c("numeric", "numeric", "numeric", "text", "text", "text"))
        
        setDT(data_y)
        
        # Select only English local authorities
        data_y <- data_y[stringr::str_detect(laua, "E")]
        
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
      # 2019
      
      if(ky == 2019) {
        
        # age variable includes 90+ so is initially treated as text
        
        # path <- "D:/Death and population data"
        
        data_y <- readxl::read_excel(paste0(path, "/2019/Eng_Pops_2019_Final.xlsx"),
                                     sheet = "England_Pops_2019",
                                     col_types = c("text", "text", "numeric", "numeric", "text", "numeric"))
        
        setDT(data_y)
        
        setnames(data_y,
                 c("age", "IMD Quintile", "lad20nm"),
                 c("ageinyrs", "imd_quintile", "laua name"))
        
        data_y[ , "laua name" := NULL]
        data_y[ , year := 2019]
        
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
    
  }
  
  
  ##############################################################################
  ########################### Wales ############################################
  ##############################################################################
  
  if(country == "Wales") {
    
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
        
        # Select only Welsh local authorities
        data_y <- data_y[stringr::str_detect(laua, "W")]
        
        setnames(data_y, c("age", "IMD_quintile"), c("ageinyrs", "imd_quintile"))
      }
      
      ###################################
      # 2015
      if(ky == 2015) {
        
        data_y <- readxl::read_excel(paste0(path, "/2015/pops_2015.xlsx"),
                                     col_types = c("numeric", "numeric", "numeric", "text", "text", "text"))
        
        setDT(data_y)
        
        # Select only Welsh local authorities
        data_y <- data_y[stringr::str_detect(laua, "W")]
        
        setnames(data_y, c("age", "IMD_quintile"), c("ageinyrs", "imd_quintile"))
      }
      
      ###################################
      # 2016
      if(ky == 2016) {
        
        data_y <- fread(paste0(path, "/2016/Pops_Wal_Laua16.csv"))
        
        data_y[ , "laua name" := NULL]
        
        setnames(data_y, c("Pops", "Year", "Age", "Sex", "IMD Quintile"),
                 c("pops", "year", "ageinyrs", "sex", "imd_quintile"))
      }
      
      ###################################
      # 2017
      if(ky == 2017) {
        
        data_y <- readxl::read_excel(paste0(path, "/2017-2018/Deaths_Pops_Wales_2017_IMD2014.xlsx"),
                                     sheet = "POPS_2017_WIMD_14",
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
        
        data_y <- readxl::read_excel(paste0(path, "/2017-2018/Deaths_Pops_Wales_2018_IMD2014.xlsx"),
                                     sheet = "POPS_2018_IMD_2014",
                                     col_types = c("text", "numeric", "numeric", "text", "text", "numeric"))
        
        setDT(data_y)
        
        setnames(data_y,
                 c("Age", "IMD Quintile", "Sex", "LA code", "LA name"),
                 c("ageinyrs", "imd_quintile", "sex", "laua", "laua name"))
        
        data_y[ , "laua name" := NULL]
        data_y[ , year := 2018]
        
      }
      
      ###################################
      # 2019
      
      if(ky == 2019) {
        
        # age variable includes 90+ so is initially treated as text
        
        # path <- "D:/Death and population data"
        
        data_y <- readxl::read_excel(paste0(path, "/2019/Wal_Pops_2019_Final.xlsx"),
                                     sheet = "Wales_Pops_2019",
                                     col_types = c("text", "text", "numeric", "numeric", "text", "numeric"))
        
        setDT(data_y)
        
        setnames(data_y,
                 c("age", "IMD Quintile", "lad20nm"),
                 c("ageinyrs", "imd_quintile", "laua name"))
        
        data_y[ , "laua name" := NULL]
        data_y[ , year := 2019]
        
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
    
  }
  
  ##############################################################################
  ########################### Scotland #########################################
  ##############################################################################
  
  if(country == "Scotland") {
    
    #path <- "D:/Scottish death and population data"
    
    #######################################
    # 2008 - 2018 population counts
    
    data1 <- readxl::read_excel(paste0(path, "/Population 2008-2018 by SIMD quintile and age.xlsx"),
                                 col_types = c("numeric", "text", rep("numeric", 83)))
    
    setDT(data1)
    
    data1[ , sex := as.character(plyr::revalue(sex, c("F" = "Female", "M" = "Male")))]
    
    data1[ , SIMD16quintile := plyr::revalue(as.character(SIMD16quintile),
                                              c("1" = "5_most_deprived",
                                                "2" = "4",
                                                "4" = "2",
                                                "5" = "1_least_deprived"))]
    
    data1[ , `:=`(AllAges = NULL, age0to10 = NULL)]
    
    data1 <- melt.data.table(data1, id.vars = c("yr", "sex", "SIMD16quintile"), value.name = "pops", variable.name = "age")
    
    data1[ , age := stringr::str_remove_all(age, "age")]
    data1[ , age := stringr::str_remove_all(age, "plus")]
    data1[ , age := as.vector(as.numeric(age))]
    
    setnames(data1, c("yr", "SIMD16quintile"), c("year", "imd_quintile"))
    
    #######################################
    # 2019 - 2021 population counts
    
    data2 <- readxl::read_excel(paste0(path, "/NRS - University of Sheffield - Duncan Gillespie - population data.xlsx"),
                               col_types = c("numeric", "text", rep("numeric", 83)))
    
    setDT(data2)
    
    data2 <- data2[sex %in% c("M", "F")]
    
    data2[ , sex := as.character(plyr::revalue(sex, c("F" = "Female", "M" = "Male")))]
    
    data2[ , SIMDquintile := plyr::revalue(as.character(quintile),
                                             c("1" = "5_most_deprived",
                                               "2" = "4",
                                               "4" = "2",
                                               "5" = "1_least_deprived"))]
    
    data2[ , `:=`(AllAges = NULL, age0to10 = NULL, quintile = NULL)]
    
    data2 <- melt.data.table(data2, id.vars = c("yr", "sex", "SIMDquintile"), value.name = "pops", variable.name = "age")
    
    data2[ , age := stringr::str_remove_all(age, "age")]
    data2[ , age := stringr::str_remove_all(age, "p")]
    data2[ , age := as.vector(as.numeric(age))]
    
    setnames(data2, c("yr", "SIMDquintile"), c("year", "imd_quintile"))
    
    # bind together
    data <- rbindlist(list(data1, data2), use.names = TRUE, fill = FALSE)
    
  }
  
  
  data[ , country := country]
  
  return(data[])
}

