
library(readxl)

esp2013 <- read_xlsx("X:/ScHARR/PR_Mortality_data_TA/general/ESP2013.xlsx")

usethis::use_data(esp2013)
