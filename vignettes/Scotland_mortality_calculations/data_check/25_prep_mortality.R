
# The aim of this code is to prepare the mortality data 
# for use in the simulation model

# this involves smoothing the rates of death

library(data.table)
library(ggplot2)
library(mort.tools)

# Load the mortality data

# Load the processed mortality data
#tobalc_mort_data <- fread(paste0("vignettes/Scotland_mortality_calculations/outputs/tobalc_death_rates_scot_national_2008-2021_v1_", Sys.Date(), "_mort.tools_", ver, ".csv"))
tobalc_mort_data <- fread(paste0("vignettes/Scotland_mortality_calculations/outputs/tobalc_death_rates_scot_national_2008-2021_v1_", "2023-03-21", "_mort.tools_", "1.6.0", ".csv"))

# Filter data

# look at only ages 18 to 89 years
# to match the age range of the Scottish hospital data

tobalc_mort_data <- tobalc_mort_data[age %in% 18:89 & !is.na(cause), 
                                     c("age", "sex", "imd_quintile", "year", "cause", "n_deaths", "pops"), with = F]

setnames(tobalc_mort_data, "cause", "condition")

# standardise combinations in the data
domain <- data.frame(expand.grid(
  age = 18:89,
  year = 2008:2021,
  sex = c("Male", "Female"),
  imd_quintile = unique(tobalc_mort_data$imd_quintile),
  condition = c(union(tobalcepi::alc_disease_names, tobalcepi::tob_disease_names), "other_causes")
))
setDT(domain)

domain <- merge(domain, tobalc_mort_data, all.x = T, all.y = F, 
                by = c("age", "sex", "imd_quintile", "year", "condition"))

tobalc_mort_data <- copy(domain)


# Create mx column 
tobalc_mort_data[ , mix := n_deaths / pops]


tobalc_mort_data[is.na(mix), mix := 0]


# check observed numbers of alcohol specific deaths
#data_check <- merge(copy(tobalc_mort_data), tobalcepi::disease_groups, by = "condition")
#data_check[age %in% 18:89 & disease_type == "Wholly attributable to alcohol", .(N = sum(n_deaths, na.rm = T)), by = c("year")]


# For conditions with less than 10 deaths recorded in a year, 
# the trend over data years is replaced with the average rate over all years of available data

small_n_cond <- unique(tobalc_mort_data[condition != "Acute_intoxication", 
                            .(n = sum(n_deaths)), 
                            by = c("condition", "year")][n < 10]$condition)

# [1] "Age_related_macular_degeneration"                                  "Alcohol_induced_pseudoCushings_syndrome"                          
# [3] "Alcohol_poisoning"                                                 "Alcoholic_gastritis"                                              
# [5] "Alcoholic_myopathy"                                                "Alcoholic_polyneuropathy"                                         
# [7] "Bulimia"                                                           "Chronic_pancreatitis_alcohol_induced"                             
# [9] "Degeneration"                                                      "Depression"                                                       
# [11] "Evidence_of_alcohol_involvement_determined_by_blood_alcohol_level" "Excessive_Blood_Level_of_Alcohol"                                 
# [13] "Hearing_loss"                                                      "Hip_fracture"                                                     
# [15] "Influenza_clinically_diagnosed"                                    "Influenza_microbiologically_confirmed"                            
# [17] "Low_back_pain"                                                     "Maternal_care_for_suspected_damage_to_foetus_from_alcohol"        
# [19] "Obstructive_sleep_apnoea"                                          "Other_intentional_injuries"                                       
# [21] "Psoriasis"                                                         "Psychosis"                                                        
# [23] "Schizophrenia"                                                     "Senile_cataract"                                                  
# [25] "Systematic_lupus_erythematosis"                                    "Toxic_effect_of_alcohol"                                          
# [27] "Mechanical_forces"                                                 "Acute_pancreatitis_alcohol_induced"                               
# [29] "Chronic_Pancreatitis"                                              "Ulcerative_colitis"                                               
# [31] "Alcoholic_cardiomyopathy"                                          "Tuberculosis"   

tobalc_mort_data[ , `:=` (death_rate_av = mean(mix)), by = c("sex", "imd_quintile", "condition")]

tobalc_mort_data[condition %in% small_n_cond, `:=` (mix = death_rate_av)]

tobalc_mort_data[ , death_rate_av := NULL]


# from 2011 onwards, no acute intoxication deaths recorded
#data[condition == "Acute_intoxication" & year >= 2011], mix := NA]

#tobalc_mort_data[condition == "Acute_intoxication", .(n = sum(n_deaths)), by = c("condition", "year")]


# check observed numbers of alcohol specific deaths
#data_check <- merge(copy(tobalc_mort_data), tobalcepi::disease_groups, by = "condition")
#data_check[age %in% 18:89 & disease_type == "Wholly attributable to alcohol", .(N = sum(pops * mix, na.rm = T)), by = c("year")]



# Smooth mortality rates

data <- copy(tobalc_mort_data)

#check <- data[ , .(n = sum(n_deaths, na.rm = T)), by = c("year", "condition")]
#ggplot(check) + geom_line(aes(x = year, y = n)) + facet_wrap(~ condition, scales = "free_y")



mx_smoothed <- mort.tools::MSmooth(
  data = data,
  var_name = "mix",
  condition_names = unique(data$condition),
  smooth_n_age = 7,
  smooth_n_year = 3)

tobalc_mort_data <- copy(mx_smoothed$data_smooth)


tobalc_mort_data[is.na(mix), mix := 0]



# replace smoothed rates with unsmoothed rates for this condition

setnames(data, "mix", "mix_raw")

tobalc_mort_data <- merge(tobalc_mort_data, data, by = c("age", "sex", "imd_quintile", "year", "condition"), all.x = T, all.y = F)



# check observed numbers of alcohol specific deaths
#data_check <- merge(copy(tobalc_mort_data), tobalcepi::disease_groups, by = "condition")
#data_check[age %in% 18:89 & disease_type == "Wholly attributable to alcohol", .(N = sum(pops * mix, na.rm = T)), by = c("year")]

tobalc_mort_data[condition == "Acute_intoxication", mix := mix_raw]

# check observed numbers of alcohol specific deaths
#data_check <- merge(copy(tobalc_mort_data), tobalcepi::disease_groups, by = "condition")
#data_check[age %in% 18:89 & disease_type == "Wholly attributable to alcohol", .(N = sum(pops * mix, na.rm = T)), by = c("year")]


tobalc_mort_data[ , pops := NULL]



# Save the data for use in the simulation model

# note the package version so that the data can be tagged with it
ver <- packageVersion("mort.tools")

write.table(tobalc_mort_data, paste0("vignettes/Scotland_mortality_calculations/outputs/tobalc_death_rates_scot_national_2008-2021_v1_", Sys.Date(), "_mort.tools_", ver, "_smoothed.csv"),
            row.names = F, sep = ",")

