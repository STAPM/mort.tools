
# The aim of this code is to make some plots to check the morbidity inputs

# focused on checking the smoothed morbidity rates

library(ggplot2)
library(viridis)
library(data.table)
library(stringr)
library(tobalcepi)

mort_data <- fread(paste0("vignettes/Scotland_mortality_calculations/outputs/tobalc_death_rates_scot_national_2008-2021_v1_", Sys.Date(), "_mort.tools_", ver, "_smoothed.csv"))

# Read the population data
pop_data <- fread(paste0("vignettes/Scotland_mortality_calculations/outputs/pop_sizes_scotland_national_2008-2021_v1_", Sys.Date(), "_mort.tools_", ver, ".csv"))

# trends from 2008 to 2021

# calculate age standardised morbidity rates based on 2019 population structure

pop2019 <- copy(pop_data[year == 2019])
pop2019[ , year := NULL]


data_av <- merge(mort_data, pop2019, all.x = T, all.y = F, sort = F,
                      by = c("age", "sex", "imd_quintile"))

data_av <- data_av[ , .(mix = sum(mix * pops) / sum(pops)), by= c("sex", "imd_quintile", "condition", "year")]

condition_names <- union(tob_disease_names, alc_disease_names)

# merge in formatted names

data_av <- merge(data_av, disease_groups, by = "condition", all.x = T, all.y = T)

for(cond in condition_names) {
  
  #cond <- condition_names[1]
  
  cat(cond, "\n")
  
  plot_data <- data_av[condition == cond]
  
  if(nrow(plot_data) > 0) {
    
    p <- ggplot(plot_data) +
      geom_line(aes(x = year, y = 1e5 * mix, colour = imd_quintile)) +
      facet_wrap(~ sex, nrow = 1) +
      theme_minimal() +
      ylab("Age-standardised death rate") +
      scale_colour_manual(name = "IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))
    
    disease_type <- unique(plot_data$disease_type)
    disease_type2 <- unique(plot_data$disease_type2)
    name_formatted <- unique(plot_data$name_formatted)
    
    cond <- str_replace_all(name_formatted, " ", "_")
    cond <- str_replace_all(cond, "'", "")
    
    png(paste0("vignettes/Scotland_mortality_calculations/plots/", cond, "_mix_av.png"), units="in", width=14/1.5, height=7/1.5, res=600)
    print(p +
            labs(title = paste0(name_formatted, ": death rates for Scotland")))
    dev.off()
    
  }
  
}

# knit the plots to a html

rmarkdown::render(
  input = "vignettes/Scotland_mortality_calculations/age_standardised_death_rates.Rmd", 
  output_file = here::here("public/articles/age_standardised_death_rates"),
  quiet = TRUE)

