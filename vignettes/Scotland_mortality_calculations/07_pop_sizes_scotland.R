
# The aim of this code is to calculate the mid-year population sizes
# for Scotland
# at a national level 

# Using data provided by National Records Scotland

library(mort.tools)
library(stringr)

# Load the population counts by age, sex, IMD quintile, local authority, year
pop_data <- ReadPopData(path = "D:/Scottish death and population data", country = "Scotland")

# write data
ver <- packageVersion("mort.tools")

write.table(pop_data, paste0("vignettes/Scotland_mortality_calculations/outputs/pop_sizes_scotland_national_2008-2021_v1_", Sys.Date(), "_mort.tools_", ver, ".csv"), row.names = F, sep = ",")

