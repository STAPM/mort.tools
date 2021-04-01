
#' Smooth age and period surface of rates \lifecycle{maturing}
#'
#' Applies a 2d moving average over age and period, by condition, sex and IMD quintile.
#' 
#' Applied to mortality or morbidity data.
#'
#' @param data Data table containing cause-specific mortality rates
#' @param var_name Character - the name of the variable to be smoother
#' @param condition_names names of the causes to include
#' @param smooth_n_age the age range for moving average - must be odd number
#' @param smooth_n_year the year range for moving average - must be odd number
#' 
#' @importFrom data.table := setDT setnames fread rbindlist dcast copy
#' 
#' @return
#' \item{data}{Observed data}
#' \item{data_smooth}{Smoothed observed data}
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' mx_smoothed <- MSmooth(
#'   data = test_data,
#'   var_name = "mix",
#'   condition_names = unique(test_data$condition),
#'   smooth_n_age = 5,
#'   smooth_n_year = 5
#' )
#'
#' }
#'
#'
MSmooth <- function(
  data,
  var_name,
  condition_names,
  smooth_n_age = 3,
  smooth_n_year = 3
) {
  
  ages_ref <- min(data$age):max(data$age)
  years_ref <- min(data$year):max(data$year)
  
  
  # Loop through subgroups
  
  counter <- 1
  
  for(sex_i in c("Male", "Female")) {
    
    #sex_i <- "Female"
    
    cat(sex_i, "\n")
    
    for(imd_quintile_i in c("1_least_deprived", "2", "3", "4", "5_most_deprived")) {
      
      #imd_quintile_i <- "1_least_deprived"
    
      cat("\t", imd_quintile_i, "\n")
        
      for(cond_i in condition_names) {
        
        #cond_i <- "Abdominal_aortic_aneurysm"
        
        cat("\t\t", cond_i, "\n")
        
        
        # Select the data for one subgroup
        
        subdata <- copy(data[sex == sex_i & imd_quintile == imd_quintile_i & condition == cond_i])
        
        subdata[ , `:=`(sex = NULL, imd_quintile = NULL, condition = NULL)]
        
        
        # Reshape to wide form with years as columns
        # then make into a matrix
        
        qdat <- dcast(subdata, age ~ year, value.var = var_name)
        
        qdat[ , age := NULL]
        
        qdat <- as.matrix(qdat)
        
        # Transpose matrix data and get deaths and logrates
        # replace extreme quit probabilities with NA - then fill with approx trend
        # then logit transform
        
        
        
        qdat[is.na(qdat)] <- 0
        
        #qdat[qdat == 0] <- NA
        #qdat[qdat > .4] <- .4
        
        # smooth values in a sliding window
        r <- raster::raster(qdat) # convert to rasterLayer
        qdat <- raster::as.matrix(raster::focal(r, matrix(1, smooth_n_age, smooth_n_year), mean, pad = T, padValue = NA, na.rm = T))
        
        # Fill any remaining missing values
        
        #qdat[is.na(qdat)] <- 0
        
        
        
        #for(i in 1:n) {
        #  qdat[i,] <- fill.zero(qdat[i,])
        #}
        
        colnames(qdat) <- years_ref
        rownames(qdat) <- ages_ref
        
        qdat <- as.data.frame(qdat)
        qdat$age <- ages_ref
        
        setDT(qdat)
        
        qdat <- melt(qdat, id.vars = "age", variable.name = "year", value.name = var_name)
        
        qdat[ , year := as.numeric(as.vector(year))]
        
        qdat[ , sex := as.character(sex_i)]
        qdat[ , imd_quintile := as.character(imd_quintile_i)]
        qdat[ , condition := as.character(cond_i)]
        
        
        if(counter == 1) {
          data_smooth <- copy(qdat)
        } else {
          data_smooth <- rbindlist(list(data_smooth, copy(qdat)), use.names = T)
        }
        
        counter <- counter + 1
      }
    }
  }
  
  
  return(list(
    data = data,
    data_smooth = data_smooth
  ))
}



















