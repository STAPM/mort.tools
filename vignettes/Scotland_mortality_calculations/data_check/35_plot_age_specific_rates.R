
# The aim of this code is to make some plots to check the morbidity inputs

# focused on checking the smoothed morbidity rates

library(ggplot2)
library(viridis)
library(data.table)
library(stringr)
library(tobalcepi)

mort_data <- fread(paste0("vignettes/Scotland_mortality_calculations/outputs/tobalc_death_rates_scot_national_2008-2021_v1_", Sys.Date(), "_mort.tools_", ver, "_smoothed.csv"))

# Age-specific rates averaged over last 5 years
# 2017 to 2021

# unweighted average across selected years

data_av <- mort_data[year %in% 2017:2021, .(mix = mean(mix, na.rm = T)), 
                     by = c("age", "sex", "imd_quintile", "condition")]


condition_names <- union(tob_disease_names, alc_disease_names)

# merge in formatted names

data_av <- merge(data_av, disease_groups, by = "condition", all.x = T, all.y = T)

# loop through conditions making plots

for(cond in condition_names) {
  
  #cond <- condition_names[1]
  
  cat(cond, "\n")
  
  plot_data <- data_av[condition == cond]
  
  if(nrow(plot_data) > 0) {
    
    p <- ggplot(plot_data) +
      geom_line(aes(x = age, y = 1e5 * mix, colour = imd_quintile)) +
      facet_wrap(~ sex, nrow = 1) +
      theme_minimal() +
      ylab("Age-specific death rate") +
      scale_colour_manual(name = "IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))
    
    disease_type <- unique(plot_data$disease_type)
    disease_type2 <- unique(plot_data$disease_type2)
    name_formatted <- unique(plot_data$name_formatted)
    
    cond <- str_replace_all(name_formatted, " ", "_")
    cond <- str_replace_all(cond, "'", "")
    
    png(paste0("vignettes/Scotland_mortality_calculations/plots/", cond, "_mix_age.png"), units="in", width=14/1.5, height=7/1.5, res=600)
    print(p +
            labs(title = paste0(name_formatted, " death rates for Scotland")))
    dev.off()
    
  }
  
}

