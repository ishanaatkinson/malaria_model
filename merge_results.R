
##### READ IN PMC IMPACT AND MODEL OUTPUTS #####

PMC_impact_ppy_rural <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_rural.csv"))
PMC_impact_ppy_urban <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_urban.csv"))

PMC_impact_ppy_rural_additional <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_additional_rural.csv"))
PMC_impact_ppy_urban_additional <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_additional_urban.csv"))

population_df_rural <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/population_df_rural.csv"))
population_df_urban <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/population_df_urban.csv"))

incidence_ppy_df_rural <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/incidence_ppy_df_rural.csv"))
incidence_ppy_df_urban <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/incidence_ppy_df_urban.csv"))

area_names <- unique(c(PMC_impact_ppy_rural$area,PMC_impact_ppy_urban$area))


##### MERGE PMC IMPACT PPY ##### 

# merge rural and urban dataframes by area, age and infection class 
PMC_impact_ppy <- bind_rows(PMC_impact_ppy_rural, PMC_impact_ppy_urban) %>% 
  group_by(area, infection_class, age_in_days_midpoint)

infection_classes <- c("clinical", "severe", "total", "asymptomatic")
age_midpoint <- age_in_days_midpoint

# merge rural and urban dataframes using weighted mean (population size = weights)
merged_PMC_impact_ppy_df <- data.frame()

for (i in 1:length(area_names)) {
  current_PMC_impact_ppy <- PMC_impact_ppy %>% filter(area==area_names[i])
  for (j in 1:length(infection_classes)) {
    current_PMC_impact_ppy_by_infection <- current_PMC_impact_ppy %>% filter(infection_class == infection_classes[j])
    for (k in 1:length(age_in_days_midpoint)) {
      current_PMC_impact_ppy_by_infection_and_age <- current_PMC_impact_ppy_by_infection %>% filter(age_in_days_midpoint == age_midpoint[k])
      
      if (dim(current_PMC_impact_ppy_by_infection_and_age)[1] == 2){
        rural_df <- (current_PMC_impact_ppy_by_infection_and_age %>% filter(rur_or_urb == "rural"))$value
        urban_df <- (current_PMC_impact_ppy_by_infection_and_age %>% filter(rur_or_urb == "urban"))$value
        
        # current year data
        rural_pop <- (full_data$population %>% filter(year == 2023, name_2 == area_names[i], urban_rural == "rural"))$pop
        urban_pop <- (full_data$population %>% filter(year == 2023, name_2 == area_names[i], urban_rural == "urban"))$pop
        
        rural_weight <- rural_pop / (sum(rural_pop, urban_pop))
        urban_weight <- urban_pop / (sum(rural_pop, urban_pop))
        
        weighted_means <- (rural_weight * rural_df) + (urban_weight * urban_df)
        
      } else {
        weighted_means <- current_PMC_impact_ppy_by_infection_and_age$value
      }
      merged_PMC_impact_ppy_df <- rbind(merged_PMC_impact_ppy_df, weighted_means)
    }
    
  }
}

# assign location, infection class, age data 
colnames(merged_PMC_impact_ppy_df) = "value"
merged_PMC_impact_ppy_df$area <- rep(area_names, each = length(infection_classes) * length(age_in_days_midpoint))
merged_PMC_impact_ppy_df$infection_class <- rep(rep(infection_classes, each = length(age_in_days_midpoint)), times=length(area_names))
merged_PMC_impact_ppy_df$iso_code <- rep(country_code, times=dim(merged_PMC_impact_ppy_df)[1])
merged_PMC_impact_ppy_df$rur_or_urb <- rep("rural/urban merged", dim(merged_PMC_impact_ppy_df)[1])
merged_PMC_impact_ppy_df$age_group <- rep(age_group_names, times = length(infection_classes) * length(area_names))
merged_PMC_impact_ppy_df$age_in_days_midpoint <- rep(age_in_days_midpoint, times = length(infection_classes) * length(area_names) )
merged_PMC_impact_ppy_df$units <- rep("ppy", times=dim(merged_PMC_impact_ppy_df)[1])
merged_PMC_impact_ppy_df <- merged_PMC_impact_ppy_df %>%
  relocate(iso_code, area, rur_or_urb, age_group, age_in_days_midpoint, infection_class, units, value)


##### MERGE PMC IMPACT PPY FOR additional PMC SCHEDULE ##### 

# merge rural and urban dataframes by area, age and infection class 
PMC_impact_ppy_additional <- bind_rows(PMC_impact_ppy_rural_additional, PMC_impact_ppy_urban_additional) %>% 
  group_by(area, infection_class, age_in_days_midpoint)

infection_classes <- c("clinical", "severe", "total", "asymptomatic")
age_midpoint <- age_in_days_midpoint

# merge rural and urban dataframes using weighted mean (population size = weights)
merged_PMC_impact_ppy_df_additional <- data.frame()

for (i in 1:length(area_names)) {
  current_PMC_impact_ppy_additional <- PMC_impact_ppy_additional %>% filter(area==area_names[i])
  for (j in 1:length(infection_classes)) {
    current_PMC_impact_ppy_by_infection_additional <- current_PMC_impact_ppy_additional %>% filter(infection_class == infection_classes[j])
    for (k in 1:length(age_in_days_midpoint)) {
      current_PMC_impact_ppy_by_infection_and_age_additional <- current_PMC_impact_ppy_by_infection_additional %>% filter(age_in_days_midpoint == age_midpoint[k])
      
      if (dim(current_PMC_impact_ppy_by_infection_and_age_additional)[1] == 2){
        rural_df <- (current_PMC_impact_ppy_by_infection_and_age_additional %>% filter(rur_or_urb == "rural"))$value
        urban_df <- (current_PMC_impact_ppy_by_infection_and_age_additional %>% filter(rur_or_urb == "urban"))$value
        
        # current year data
        rural_pop <- (full_data$population %>% filter(year == 2023, name_2 == area_names[i], urban_rural == "rural"))$pop
        urban_pop <- (full_data$population %>% filter(year == 2023, name_2 == area_names[i], urban_rural == "urban"))$pop
        
        rural_weight <- rural_pop / (sum(rural_pop, urban_pop))
        urban_weight <- urban_pop / (sum(rural_pop, urban_pop))
        
        weighted_means <- (rural_weight * rural_df) + (urban_weight * urban_df)
        
      } else {
        weighted_means <- current_PMC_impact_ppy_by_infection_and_age_additional$value
      }
      merged_PMC_impact_ppy_df_additional <- rbind(merged_PMC_impact_ppy_df_additional, weighted_means)
    }
    
  }
}

# assign location, infection class, age data 
colnames(merged_PMC_impact_ppy_df_additional) = "value"
merged_PMC_impact_ppy_df_additional$area <- rep(area_names, each = length(infection_classes) * length(age_in_days_midpoint))
merged_PMC_impact_ppy_df_additional$infection_class <- rep(rep(infection_classes, each = length(age_in_days_midpoint)), times=length(area_names))
merged_PMC_impact_ppy_df_additional$iso_code <- rep(country_code, times=dim(merged_PMC_impact_ppy_df_additional)[1])
merged_PMC_impact_ppy_df_additional$rur_or_urb <- rep("rural/urban merged", dim(merged_PMC_impact_ppy_df_additional)[1])
merged_PMC_impact_ppy_df_additional$age_group <- rep(age_group_names, times = length(infection_classes) * length(area_names))
merged_PMC_impact_ppy_df_additional$age_in_days_midpoint <- rep(age_in_days_midpoint, times = length(infection_classes) * length(area_names) )
merged_PMC_impact_ppy_df_additional$units <- rep("ppy", times=dim(merged_PMC_impact_ppy_df_additional)[1])
merged_PMC_impact_ppy_df_additional <- merged_PMC_impact_ppy_df_additional %>%
  relocate(iso_code, area, rur_or_urb, age_group, age_in_days_midpoint, infection_class, units, value)


##### MERGE BASELINE INCIDENCE PPY (NO PMC) ##### 

# merge rural and urban dataframes by area, infection class and age
merge_incidence_ppy <- bind_rows(incidence_ppy_df_rural, incidence_ppy_df_urban) %>% 
  group_by(area, infection_class, age_in_days_midpoint)

infection_classes <- c("clinical", "severe", "total", "asymptomatic")
age_midpoint <- age_in_days_midpoint

# merge rural and urban dataframes using weighted mean (population size = weights)
merged_incidence_ppy_df <- data.frame()

for (i in 1:length(area_names)) {
  current_merge_incidence_ppy <- merge_incidence_ppy %>% filter(area==area_names[i])
  for (j in 1:length(infection_classes)) {
    current_merge_incidence_ppy_by_infection <- current_merge_incidence_ppy %>% filter(infection_class == infection_classes[j])
    for (k in 1:length(age_in_days_midpoint)) {
      current_merge_incidence_ppy_by_infection_and_age <- current_merge_incidence_ppy_by_infection %>% filter(age_in_days_midpoint == age_midpoint[k])
      
      if (dim(current_merge_incidence_ppy_by_infection_and_age)[1] == 2){
        rural_df <- (current_merge_incidence_ppy_by_infection_and_age %>% filter(rur_or_urb == "rural"))$value
        urban_df <- (current_merge_incidence_ppy_by_infection_and_age %>% filter(rur_or_urb == "urban"))$value
        
        # current year data
        rural_pop <- (full_data$population %>% filter(year == 2023, name_2 == area_names[i], urban_rural == "rural"))$pop
        urban_pop <- (full_data$population %>% filter(year == 2023, name_2 == area_names[i], urban_rural == "urban"))$pop
        
        rural_weight <- rural_pop / (sum(rural_pop, urban_pop))
        urban_weight <- urban_pop / (sum(rural_pop, urban_pop))
        
        weighted_means <- (rural_weight * rural_df) + (urban_weight * urban_df)
        
      } else {
        weighted_means <- current_merge_incidence_ppy_by_infection_and_age$value
      }
      merged_incidence_ppy_df <- rbind(merged_incidence_ppy_df, weighted_means)
    }
    
  }
}

# assign location, infection class, age data 
colnames(merged_incidence_ppy_df) = "value"
merged_incidence_ppy_df$area <- rep(area_names, each = length(infection_classes) * length(age_in_days_midpoint))
merged_incidence_ppy_df$infection_class <- rep(rep(infection_classes, each = length(age_in_days_midpoint)), times=length(area_names))
merged_incidence_ppy_df$iso_code <- rep(country_code, times=dim(merged_incidence_ppy_df)[1])
merged_incidence_ppy_df$rur_or_urb <- rep("rural/urban merged", dim(merged_incidence_ppy_df)[1])
merged_incidence_ppy_df$age_group <- rep(age_group_names, times = length(infection_classes) * length(area_names))
merged_incidence_ppy_df$age_in_days_midpoint <- rep(age_in_days_midpoint, times = length(infection_classes) * length(area_names) )
merged_incidence_ppy_df$units <- rep("ppy", times=dim(merged_incidence_ppy_df)[1])
merged_incidence_ppy_df <- merged_incidence_ppy_df %>%
  relocate(iso_code, area, rur_or_urb, age_group, age_in_days_midpoint, infection_class, units, value)


##### MERGE AGE STRUCTURE/POPULATION DATA ##### 

# merge rural and urban dataframes by area, infection class and age
merge_population <- bind_rows(population_df_rural, population_df_urban) %>% 
  group_by(area, timestep)

# merge rural and urban dataframes using weighted mean (population size = weights)
merged_population_df <- data.frame()

for (i in 1:length(area_names)) {
  current_merge_population <- merge_population %>% filter(area==area_names[i])
  print(i)
  for (k in 1:sim_length) {
      current_merge_population_by_time <- current_merge_population %>% filter(timestep == k)
      
      if (dim(current_merge_population_by_time)[1] == 2){
        rural_df <- (current_merge_population_by_time %>% filter(rur_or_urb == "rural"))[,5:dim(current_merge_population_by_time)[2]]
        urban_df <- (current_merge_population_by_time %>% filter(rur_or_urb == "urban"))[,5:dim(current_merge_population_by_time)[2]]
        
        # current year data
        rural_pop <- (full_data$population %>% filter(year == 2023, name_2 == area_names[i], urban_rural == "rural"))$pop
        urban_pop <- (full_data$population %>% filter(year == 2023, name_2 == area_names[i], urban_rural == "urban"))$pop
        
        rural_weight <- rural_pop / (sum(rural_pop, urban_pop))
        urban_weight <- urban_pop / (sum(rural_pop, urban_pop))
        
        weighted_means <- (rural_weight * rural_df) + (urban_weight * urban_df)
        
      } else {
        weighted_means <- current_merge_population_by_time[,5:dim(current_merge_population_by_time)[2]]
      }
      merged_population_df <- rbind(merged_population_df, weighted_means)
    }
    
  }

# assign location, infection class, age data 
merged_population_df$timestep <- rep(1:sim_length, times = length(area_names))
merged_population_df$area <- rep(area_names, each = sim_length)
merged_population_df$iso_code <- rep(country_code, times=dim(merged_population_df)[1])
merged_population_df$rur_or_urb <- rep("rural/urban merged", dim(merged_population_df)[1])
merged_population_df <- merged_population_df %>%
  relocate(timestep, iso_code, area, rur_or_urb)


##### STORE DATAFRAMES IN CSV FORMAT #####

# average incidence per person per year for each site (WITH PMC APPLIED)
write.csv(merged_PMC_impact_ppy_df, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_merged.csv"), row.names=FALSE)

# average incidence per person per year for each site (WITH additional PMC SCHEDULE APPLIED)
write.csv(merged_PMC_impact_ppy_df_additional, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_additional_merged.csv"), row.names=FALSE)

# population sizes in each age group at each timestep, for each site 
write.csv(merged_population_df, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/population_df_merged.csv"), row.names=FALSE)

# average incidence per person per year for each site
write.csv(merged_incidence_ppy_df, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/incidence_ppy_df_merged.csv"), row.names=FALSE)




