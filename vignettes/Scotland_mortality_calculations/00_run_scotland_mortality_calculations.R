
# This code runs the scripts in order to produce the workflow that prepares and checks mortality data for Scotland

source("vignettes/Scotland_mortality_calculations/07_pop_sizes_scotland.R")
source("vignettes/Scotland_mortality_calculations/10a_calculate_tob_death_rates_scot_national.R")
source("vignettes/Scotland_mortality_calculations/10b_calculate_alc_death_rates_scot_national.R")
source("vignettes/Scotland_mortality_calculations/10c_calculate_tobalc_death_rates_scot_national.R")
source("vignettes/Scotland_mortality_calculations/20_run_data_check.R")

