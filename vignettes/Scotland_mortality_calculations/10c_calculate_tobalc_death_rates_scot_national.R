
# The aim of this code is to read the data on deaths
# and to use the data to calculate Scottish national death rates for tobacco and/or alcohol related diseases

# Prepare the death rates from tobacco and/or alcohol related diseases for Scotland

library(data.table)
library(mort.tools)
library(tobalcepi)
library(stringr)


path <- "D:/Scottish death and population data"

# Load the tobacco ICD-10 disease code lookups

# Data supplied with numeric codes indicating cause
# the file tobacco_icd10_lookups should be supplied to this processing code as supplementary data (as a data.table)

tobalc_icd10_data <- read.csv(paste0(path, "/tobalc_icd10_lookups.csv"))
setDT(tobalc_icd10_data)

# Load the death counts by age, sex, IMD quintile, year and cause
deaths_data <- mort.tools::ReadData(path, country = "Scotland", supp_data = tobalc_icd10_data)

deaths_data[ , country := NULL]

# Split deaths from oesophageal cancer into two subtypes
deaths_data <- mort.tools::SplitOesoph(deaths_data)

# Remove records that have no IMD quintile information
deaths_data <- deaths_data[imd_quintile != "9"]


# # problem to be solved - you have a list of deaths by all tob and alc connditions
# # you only want the deaths for tob conditions - 
# #tobalcepi::tob_disease_names
# # the deaths for alc only conditions need to be pooled with deaths from 'all other causes'
# 
# deaths_data[ , tob_cause := 0]
# deaths_data[cause %in% tobalcepi::tob_disease_names , tob_cause := 1]
# 
# deaths_data[tob_cause == 0, cause := "other_causes"]
# 
# deaths_data <- deaths_data[ , .(n_deaths = sum(n_deaths)), by = c("year", "sex", "imd_quintile", "cause", "age")]


# Where there have been no deaths from a particular cause within a particular subgroup,
# add new rows that have n_deaths = 0
domain <- data.frame(expand.grid(
  year = unique(deaths_data$year),
  age = unique(deaths_data$age),
  sex = unique(deaths_data$sex),
  imd_quintile = unique(deaths_data$imd_quintile),
  cause = c(union(tobalcepi::alc_disease_names, tobalcepi::tob_disease_names), "other_causes")
))
setDT(domain)

deaths_data <- merge(domain, deaths_data, by = c("year", "age", "sex", "imd_quintile", "cause"), all.x = T, all.y = F)

deaths_data[is.na(n_deaths), n_deaths := 0]


######## Calculate the central death rates

# Read the population data
pop_data <- fread(paste0("vignettes/Scotland_mortality_calculations/outputs/pop_sizes_scotland_national_2008-2021_v1_", Sys.Date(), "_mort.tools_", ver, ".csv"))

# Merge the deaths and population counts
deaths_data <- merge(deaths_data, pop_data, by = c("year", "age", "sex", "imd_quintile"), all = T)

# Calculate the central death rates
deaths_data[ , mx_cause := n_deaths / pops]

# Check looks ok
deaths_data[ , .(n_deaths = sum(n_deaths)), by = "cause"]


######## Write the data

# note the package version so that the data can be tagged with it
ver <- packageVersion("mort.tools")


write.table(deaths_data, paste0("vignettes/Scotland_mortality_calculations/outputs/tobalc_death_rates_scot_national_2008-2021_v1_", Sys.Date(), "_mort.tools_", ver, ".csv"),
  row.names = F, sep = ",")




