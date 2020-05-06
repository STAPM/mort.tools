
library(data.table)

# load info from CRUK paper (they use 2014 data to inform the split)

oesoph_splits <- readxl::read_excel("data-raw/Oesophagous splits/Oesophageal AC and SCC data v2.xlsx",
  sheet = "Sheet1", range = "D5:E23")

ages <- readxl::read_excel("data-raw/Oesophagous splits/Oesophageal AC and SCC data v2.xlsx",
  sheet = "Sheet1", range = "A5:A23")

oesoph_splits <- cbind(ages, oesoph_splits)

colnames(oesoph_splits) <- c("cruk_ageband", "Male", "Female")

setDT(oesoph_splits)

oesoph_splits <- melt(oesoph_splits, id.vars = "cruk_ageband", variable.name = "sex", value.name = "proportion_scc")

usethis::use_data(oesoph_splits)
