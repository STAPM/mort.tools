

library(data.table)


##########################################
# Tobacco related diseases

tob_icd10_lookups <- readxl::read_excel("data-raw/Disease lists/16102018 tobacco and alcohol Disease List and Risk Functions.xlsx", sheet = "Tobacco")

setDT(tob_icd10_lookups)

setnames(tob_icd10_lookups, colnames(tob_icd10_lookups), tolower(colnames(tob_icd10_lookups)))

tob_icd10_lookups <- tob_icd10_lookups[version == "Current", c("condition", "icd10_lookups")]

tob_icd10_lookups[stringr::str_detect(condition, "Oesoph"), condition := "Oesophageal"]

tob_icd10_lookups <- unique(tob_icd10_lookups, by = "icd10_lookups")


##########################################
# Alcohol related diseases

alc_icd10_lookups <- readxl::read_excel("data-raw/Disease lists/16102018 tobacco and alcohol Disease List and Risk Functions.xlsx", sheet = "Alcohol")

setDT(alc_icd10_lookups)

setnames(alc_icd10_lookups, colnames(alc_icd10_lookups), tolower(colnames(alc_icd10_lookups)))

alc_icd10_lookups <- alc_icd10_lookups[ , c("condition", "icd10_lookups")]

alc_icd10_lookups[stringr::str_detect(condition, "Oesoph"), condition := "Oesophageal"]

alc_icd10_lookups <- unique(alc_icd10_lookups, by = "icd10_lookups")


##########################################
# Tobacco and alcohol related diseases

tobalc_icd10_lookups <- rbindlist(list(tob_icd10_lookups, alc_icd10_lookups))
tobalc_icd10_lookups <- unique(tobalc_icd10_lookups, by = "icd10_lookups")


##########################################
# Embed the data within the package
usethis::use_data(tob_icd10_lookups)
usethis::use_data(alc_icd10_lookups)
usethis::use_data(tobalc_icd10_lookups)

