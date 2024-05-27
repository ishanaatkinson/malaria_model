#source("running_model.R")
#source("PMC_post_processing.R")
#source("merge_results.R")

##### GET RESULTS FOR RURAL/URBAN IN A LOOP #####

rural_urban <- c("rural", "urban")

for (x in 1:length(rural_urban)) {
  ##### READ IN MERGED INCIDENCE AND PMC IMPACT OUTPUTS #####
  
  rur_or_urb <- rural_urban[x]
  
  incidence_ppy_df <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/incidence_ppy_df_", rur_or_urb, ".csv"))
  population_df <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/population_df_", rur_or_urb, ".csv"))
  PMC_impact_ppy<-read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_", rur_or_urb, ".csv"))
  PMC_impact_ppy_additional_doses<-read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_additional_", rur_or_urb, ".csv"))
  
  
  area_names <- unique(PMC_impact_ppy$area)
  
  ##### SET UP CALCULATION DATAFRAMES  #####
  
  # raw baseline cases (no PMC) for every age group
  cases_no_PMC<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                            NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                            units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))
  
  # average baseline annual cases (no PMC) for whole age group
  annual_cases_no_PMC<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                   NAME_2 = area_names,
                                   units=rep("raw cases", length(area_names)))
  
  # raw cases (with PMC) for every age group
  cases_with_PMC<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                              NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                              units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))
  
  # average annual cases (with PMC) for whole age group
  annual_cases_with_PMC<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                     NAME_2 = area_names,
                                     units=rep("raw cases", length(area_names)))
  
  # raw cases (with additional PMC) for every age group
  cases_with_additional_PMC<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                                         NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                                         units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))
  
  # average annual cases (with additional PMC) for whole age group
  annual_cases_with_additional_PMC<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                                NAME_2 = area_names,
                                                units=rep("raw cases", length(area_names)))
  
  # baseline cases (no PMC) for every age group (per 1000 children in that age group)
  cases_no_PMC_per1000<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                                    NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                                    units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))
  
  # average baseline annual cases (no PMC) for whole age group (per 1000 children in that age group)
  annual_cases_no_PMC_per1000<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                           NAME_2 = area_names,
                                           units=rep("raw cases", length(area_names)))
  
  # cases (with PMC) for every age group (per 1000 children in that age group)
  cases_with_PMC_per1000<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                                      NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                                      units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))
  
  # average annual cases (with PMC) for whole age group (per 1000 children in that age group)
  annual_cases_with_PMC_per1000<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                             NAME_2 = area_names,
                                             units=rep("raw cases", length(area_names)))
  
  # raw cases averted (with PMC) for every age group
  cases_averted_with_PMC<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                                      NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                                      units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))
  
  # average annual cases averted (with PMC) for whole age group
  annual_cases_averted_with_PMC<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                             NAME_2 = area_names,
                                             units=rep("raw cases", length(area_names)))
  
  
  # cases averted (with PMC) for every age group (per 1000 children in that age group)
  cases_averted_with_PMC_per1000<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                                              NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                                              units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))
  
  # average annual cases averted (with PMC) for whole age group (per 1000 children in that age group)
  annual_cases_averted_with_PMC_per1000<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                                     NAME_2 = area_names,
                                                     units=rep("raw cases", length(area_names)))
  
  # average % reduction in cases (with PMC) for whole age group
  cases_reduction_with_PMC <- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                         units=rep("average reduction (%)", length(area_names)), NAME_2 = area_names)
  
  
  # cases (with PMC) for every age group (per 1000 children in that age group)
  cases_with_additional_PMC_per1000<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                                                 NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                                                 units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))
  
  # average annual cases (with PMC) for whole age group (per 1000 children in that age group)
  annual_cases_with_additional_PMC_per1000<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                                        NAME_2 = area_names,
                                                        units=rep("raw cases", length(area_names)))
  
  # raw cases averted (with PMC) for every age group
  cases_averted_with_additional_PMC<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                                                 NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                                                 units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))
  
  # average annual cases averted (with PMC) for whole age group
  annual_cases_averted_with_additional_PMC<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                                        NAME_2 = area_names,
                                                        units=rep("raw cases", length(area_names)))
  
  
  # cases averted (with PMC) for every age group (per 1000 children in that age group)
  cases_averted_with_additional_PMC_per1000<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                                                         NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                                                         units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))
  
  # average annual cases averted (with PMC) for whole age group (per 1000 children in that age group)
  annual_cases_averted_with_additional_PMC_per1000<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                                                NAME_2 = area_names,
                                                                units=rep("raw cases", length(area_names)))
  
  # average % reduction in cases (with PMC) for whole age group
  cases_reduction_with_additional_PMC <- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                                    units=rep("average reduction (%)", length(area_names)), NAME_2 = area_names)
  
  
  ##### INITIALISE VECTORS FOR CALCULATIONS #####
  
  # raw cases
  current_cases_no_PMC_clin <- current_cases_no_PMC_sev <- current_cases_no_PMC_tot <- current_cases_no_PMC_asym <- c()
  current_cases_averted_with_PMC_clin <- current_cases_averted_with_PMC_sev <- current_cases_averted_with_PMC_tot <- current_cases_averted_with_PMC_asym <- c()
  current_cases_with_PMC_clin <- current_cases_with_PMC_sev <- current_cases_with_PMC_tot <- current_cases_with_PMC_asym <- c()
  current_cases_averted_with_additional_PMC_clin <- current_cases_averted_with_additional_PMC_sev <- current_cases_averted_with_additional_PMC_tot <- current_cases_averted_with_additional_PMC_asym <- c()
  current_cases_with_additional_PMC_clin <- current_cases_with_additional_PMC_sev <- current_cases_with_additional_PMC_tot <- current_cases_with_additional_PMC_asym <- c()
  
  # cases per 1000 children in that age group
  current_cases_no_PMC_clin_per1000 <- current_cases_no_PMC_sev_per1000 <- current_cases_no_PMC_tot_per1000 <- current_cases_no_PMC_asym_per1000 <- c()
  current_cases_averted_with_PMC_clin_per1000 <- current_cases_averted_with_PMC_sev_per1000 <- current_cases_averted_with_PMC_tot_per1000 <- current_cases_averted_with_PMC_asym_per1000 <- c()
  current_cases_with_PMC_clin_per1000 <- current_cases_with_PMC_sev_per1000 <- current_cases_with_PMC_tot_per1000 <- current_cases_with_PMC_asym_per1000 <- c()
  current_cases_averted_with_additional_PMC_clin_per1000 <- current_cases_averted_with_additional_PMC_sev_per1000 <- current_cases_averted_with_additional_PMC_tot_per1000 <- current_cases_averted_with_additional_PMC_asym_per1000 <- c()
  current_cases_with_additional_PMC_clin_per1000 <- current_cases_with_additional_PMC_sev_per1000 <- current_cases_with_additional_PMC_tot_per1000 <- current_cases_with_additional_PMC_asym_per1000 <- c()
  
  # % reduction with PMC 
  current_cases_reduction_with_PMC_clin <- current_cases_reduction_with_PMC_sev <- current_cases_reduction_with_PMC_tot <- current_cases_reduction_with_PMC_asym <- c()
  current_cases_reduction_with_additional_PMC_clin <- current_cases_reduction_with_additional_PMC_sev <- current_cases_reduction_with_additional_PMC_tot <- current_cases_reduction_with_additional_PMC_asym <- c()
  
  # annual cases
  annual_current_cases_no_PMC_clin <- annual_current_cases_no_PMC_sev <- annual_current_cases_no_PMC_tot <- annual_current_cases_no_PMC_asym <- c()
  annual_current_cases_with_PMC_clin <- annual_current_cases_with_PMC_sev <- annual_current_cases_with_PMC_tot <- annual_current_cases_with_PMC_asym <- c()
  annual_current_cases_averted_with_PMC_clin <- annual_current_cases_averted_with_PMC_sev <- annual_current_cases_averted_with_PMC_tot <- annual_current_cases_averted_with_PMC_asym <- c()
  annual_current_cases_with_additional_PMC_clin <- annual_current_cases_with_additional_PMC_sev <- annual_current_cases_with_additional_PMC_tot <- annual_current_cases_with_additional_PMC_asym <- c()
  annual_current_cases_averted_with_additional_PMC_clin <- annual_current_cases_averted_with_additional_PMC_sev <- annual_current_cases_averted_with_additional_PMC_tot <- annual_current_cases_averted_with_additional_PMC_asym <- c()
  
  # annual cases (per 1000 children in that age group)
  annual_current_cases_no_PMC_clin_per1000 <- annual_current_cases_no_PMC_sev_per1000 <- annual_current_cases_no_PMC_tot_per1000 <- annual_current_cases_no_PMC_asym_per1000 <- c()
  annual_current_cases_with_PMC_clin_per1000 <- annual_current_cases_with_PMC_sev_per1000 <- annual_current_cases_with_PMC_tot_per1000 <- annual_current_cases_with_PMC_asym_per1000 <- c()
  annual_current_cases_averted_with_PMC_clin_per1000 <- annual_current_cases_averted_with_PMC_sev_per1000 <- annual_current_cases_averted_with_PMC_tot_per1000 <- annual_current_cases_averted_with_PMC_asym_per1000 <- c()
  annual_current_cases_with_additional_PMC_clin_per1000 <- annual_current_cases_with_additional_PMC_sev_per1000 <- annual_current_cases_with_additional_PMC_tot_per1000 <- annual_current_cases_with_additional_PMC_asym_per1000 <- c()
  annual_current_cases_averted_with_additional_PMC_clin_per1000 <- annual_current_cases_averted_with_additional_PMC_sev_per1000 <- annual_current_cases_averted_with_additional_PMC_tot_per1000 <- annual_current_cases_averted_with_additional_PMC_asym_per1000 <- c()
  
  ##### RUN CALCULATIONS ACROSS WHOLE COUNTRY #####
  
  for (i in 1:length(area_names)){
    
    # extracting average age group proportions from the imperial model age distributions 
    age_distribution <- rep(colMeans((population_df %>% 
                                        filter(area == area_names[i]))[, 5:(dim(population_df)[2])] / human_population), 4)
    
    # sum of rural and urban areas as weighted mean is used 
    
    if (rur_or_urb != "merged") {
      area_population <- sum((full_data$population %>% 
                                filter(name_2 == area_names[i], year==2023, urban_rural==rur_or_urb))$pop)
    } else {
      area_population <- sum((full_data$population %>% 
                                filter(name_2 == area_names[i], year==2023))$pop)
    }
    
    # number of cases in each age group per year (no PMC)
    current_cases_no_PMC <- (incidence_ppy_df %>% filter(area==area_names[i]))$value * age_distribution * area_population
    
    # number of cases in each age group per year (no PMC) by infection class 
    current_cases_no_PMC_clin <- c(current_cases_no_PMC_clin, current_cases_no_PMC[1:max(age_max)])
    current_cases_no_PMC_sev <- c(current_cases_no_PMC_sev, current_cases_no_PMC[(1+max(age_max)):(2*max(age_max))])
    current_cases_no_PMC_tot <- c(current_cases_no_PMC_tot, current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))])
    current_cases_no_PMC_asym <- c(current_cases_no_PMC_asym, current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))])
    
    # annual number of cases in each age group per year (no PMC) by infection class 
    annual_current_cases_no_PMC_clin <- c(annual_current_cases_no_PMC_clin, sum(current_cases_no_PMC[1:max(age_max)]))
    annual_current_cases_no_PMC_sev <- c(annual_current_cases_no_PMC_sev, sum(current_cases_no_PMC[(1+max(age_max)):(2*max(age_max))]))
    annual_current_cases_no_PMC_tot <- c(annual_current_cases_no_PMC_tot, sum(current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))]))
    annual_current_cases_no_PMC_asym <- c(annual_current_cases_no_PMC_asym, sum(current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))]))
    
    # number of cases in each age group per year (no PMC) (per 1000 children in that age group)
    current_cases_no_PMC_per1000 <- (incidence_ppy_df %>% filter(area==area_names[i]))$value * 1000
    
    # number of cases in each age group per year (no PMC) by infection class (per 1000 children in that age group)
    current_cases_no_PMC_clin_per1000 <- c(current_cases_no_PMC_clin_per1000, current_cases_no_PMC_per1000[1:max(age_max)])
    current_cases_no_PMC_sev_per1000 <- c(current_cases_no_PMC_sev_per1000, current_cases_no_PMC_per1000[(1+max(age_max)):(2*max(age_max))])
    current_cases_no_PMC_tot_per1000 <- c(current_cases_no_PMC_tot_per1000, current_cases_no_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))])
    current_cases_no_PMC_asym_per1000 <- c(current_cases_no_PMC_asym_per1000, current_cases_no_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))])
    
    # annual number of cases in each age group per year (no PMC) by infection class (per 1000 children in that age group)
    annual_current_cases_no_PMC_clin_per1000 <- c(annual_current_cases_no_PMC_clin_per1000, mean(current_cases_no_PMC_per1000[1:max(age_max)]))
    annual_current_cases_no_PMC_sev_per1000 <- c(annual_current_cases_no_PMC_sev_per1000, mean(current_cases_no_PMC_per1000[(1+max(age_max)):(2*max(age_max))]))
    annual_current_cases_no_PMC_tot_per1000 <- c(annual_current_cases_no_PMC_tot_per1000, mean(current_cases_no_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))]))
    annual_current_cases_no_PMC_asym_per1000 <- c(annual_current_cases_no_PMC_asym_per1000, mean(current_cases_no_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))]))
    
    # number of cases in each age group per year (with PMC)
    current_cases_with_PMC <- (PMC_impact_ppy %>% filter(area==area_names[i]))$value * age_distribution * area_population
    current_cases_with_additional_PMC <- (PMC_impact_ppy_additional_doses %>% filter(area==area_names[i]))$value * age_distribution * area_population
    
    # number of cases in each age group per year (with PMC) (per 1000 children in that age group)
    current_cases_with_PMC_per1000 <- (PMC_impact_ppy %>% filter(area==area_names[i]))$value * 1000
    current_cases_with_additional_PMC_per1000 <- (PMC_impact_ppy_additional_doses %>% filter(area==area_names[i]))$value * 1000
    
    # number of cases in each age group per year (with PMC) by infection class 
    current_cases_with_PMC_clin <- c(current_cases_with_PMC_clin, current_cases_with_PMC[1:max(age_max)])
    current_cases_with_PMC_sev <- c(current_cases_with_PMC_sev, current_cases_with_PMC[(1+max(age_max)):(2*max(age_max))])
    current_cases_with_PMC_tot <- c(current_cases_with_PMC_tot, current_cases_with_PMC[(1+2*max(age_max)):(3*max(age_max))])
    current_cases_with_PMC_asym <- c(current_cases_with_PMC_asym, current_cases_with_PMC[(1+3*max(age_max)):(4*max(age_max))])
    
    current_cases_with_additional_PMC_clin <- c(current_cases_with_additional_PMC_clin, current_cases_with_additional_PMC[1:max(age_max)])
    current_cases_with_additional_PMC_sev <- c(current_cases_with_additional_PMC_sev, current_cases_with_additional_PMC[(1+max(age_max)):(2*max(age_max))])
    current_cases_with_additional_PMC_tot <- c(current_cases_with_additional_PMC_tot, current_cases_with_additional_PMC[(1+2*max(age_max)):(3*max(age_max))])
    current_cases_with_additional_PMC_asym <- c(current_cases_with_additional_PMC_asym, current_cases_with_additional_PMC[(1+3*max(age_max)):(4*max(age_max))])
    
    # number of cases in each age group per year (with PMC) by infection class (per 1000 children in that age group)
    current_cases_with_PMC_clin_per1000 <- c(current_cases_with_PMC_clin_per1000, current_cases_with_PMC_per1000[1:max(age_max)])
    current_cases_with_PMC_sev_per1000 <- c(current_cases_with_PMC_sev_per1000, current_cases_with_PMC_per1000[(1+max(age_max)):(2*max(age_max))])
    current_cases_with_PMC_tot_per1000 <- c(current_cases_with_PMC_tot_per1000, current_cases_with_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))])
    current_cases_with_PMC_asym_per1000 <- c(current_cases_with_PMC_asym_per1000, current_cases_with_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))])
    
    current_cases_with_additional_PMC_clin_per1000 <- c(current_cases_with_additional_PMC_clin_per1000, current_cases_with_additional_PMC_per1000[1:max(age_max)])
    current_cases_with_additional_PMC_sev_per1000 <- c(current_cases_with_additional_PMC_sev_per1000, current_cases_with_additional_PMC_per1000[(1+max(age_max)):(2*max(age_max))])
    current_cases_with_additional_PMC_tot_per1000 <- c(current_cases_with_additional_PMC_tot_per1000, current_cases_with_additional_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))])
    current_cases_with_additional_PMC_asym_per1000 <- c(current_cases_with_additional_PMC_asym_per1000, current_cases_with_additional_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))])
    
    # annual number of cases in each age group per year (with PMC) by infection class 
    annual_current_cases_with_PMC_clin <- c(annual_current_cases_with_PMC_clin, sum(current_cases_with_PMC[1:max(age_max)]))
    annual_current_cases_with_PMC_sev <- c(annual_current_cases_with_PMC_sev, sum(current_cases_with_PMC[(1+max(age_max)):(2*max(age_max))]))
    annual_current_cases_with_PMC_tot <- c(annual_current_cases_with_PMC_tot, sum(current_cases_with_PMC[(1+2*max(age_max)):(3*max(age_max))]))
    annual_current_cases_with_PMC_asym <- c(annual_current_cases_with_PMC_asym, sum(current_cases_with_PMC[(1+3*max(age_max)):(4*max(age_max))]))
    
    annual_current_cases_with_additional_PMC_clin <- c(annual_current_cases_with_additional_PMC_clin, sum(current_cases_with_additional_PMC[1:max(age_max)]))
    annual_current_cases_with_additional_PMC_sev <- c(annual_current_cases_with_additional_PMC_sev, sum(current_cases_with_additional_PMC[(1+max(age_max)):(2*max(age_max))]))
    annual_current_cases_with_additional_PMC_tot <- c(annual_current_cases_with_additional_PMC_tot, sum(current_cases_with_additional_PMC[(1+2*max(age_max)):(3*max(age_max))]))
    annual_current_cases_with_additional_PMC_asym <- c(annual_current_cases_with_additional_PMC_asym, sum(current_cases_with_additional_PMC[(1+3*max(age_max)):(4*max(age_max))]))
    
    # annual number of cases in each age group per year (with PMC) by infection class (per 1000 children in that age group)
    annual_current_cases_with_PMC_clin_per1000 <- c(annual_current_cases_with_PMC_clin_per1000, mean(current_cases_with_PMC_per1000[1:max(age_max)]))
    annual_current_cases_with_PMC_sev_per1000 <- c(annual_current_cases_with_PMC_sev_per1000, mean(current_cases_with_PMC_per1000[(1+max(age_max)):(2*max(age_max))]))
    annual_current_cases_with_PMC_tot_per1000 <- c(annual_current_cases_with_PMC_tot_per1000, mean(current_cases_with_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))]))
    annual_current_cases_with_PMC_asym_per1000 <- c(annual_current_cases_with_PMC_asym_per1000, mean(current_cases_with_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))]))
    
    annual_current_cases_with_additional_PMC_clin_per1000 <- c(annual_current_cases_with_additional_PMC_clin_per1000, mean(current_cases_with_additional_PMC_per1000[1:max(age_max)]))
    annual_current_cases_with_additional_PMC_sev_per1000 <- c(annual_current_cases_with_additional_PMC_sev_per1000, mean(current_cases_with_additional_PMC_per1000[(1+max(age_max)):(2*max(age_max))]))
    annual_current_cases_with_additional_PMC_tot_per1000 <- c(annual_current_cases_with_additional_PMC_tot_per1000, mean(current_cases_with_additional_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))]))
    annual_current_cases_with_additional_PMC_asym_per1000 <- c(annual_current_cases_with_additional_PMC_asym_per1000, mean(current_cases_with_additional_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))]))
    
    # average reduction in cases (with PMC) by infection class 
    current_cases_reduction_with_PMC_clin <- c(current_cases_reduction_with_PMC_clin, (mean(current_cases_no_PMC[1:max(age_max)]) - mean(current_cases_with_PMC[1:max(age_max)])) / mean(current_cases_no_PMC[1:max(age_max)]) * 100)
    current_cases_reduction_with_PMC_sev <- c(current_cases_reduction_with_PMC_sev, (mean(current_cases_no_PMC[(1+max(age_max)):(2*max(age_max))]) - mean(current_cases_with_PMC[(1+max(age_max)):(2*max(age_max))])) / mean(current_cases_no_PMC[(1+max(age_max)):(2*max(age_max))]) * 100)
    current_cases_reduction_with_PMC_tot <- c(current_cases_reduction_with_PMC_tot, (mean(current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))]) - mean(current_cases_with_PMC[(1+2*max(age_max)):(3*max(age_max))])) / mean(current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))]) * 100)
    current_cases_reduction_with_PMC_asym <- c(current_cases_reduction_with_PMC_asym, (mean(current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))]) - mean(current_cases_with_PMC[(1+3*max(age_max)):(4*max(age_max))])) / mean(current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))]) * 100)
    
    current_cases_reduction_with_additional_PMC_clin <- c(current_cases_reduction_with_additional_PMC_clin, (mean(current_cases_no_PMC[1:max(age_max)]) - mean(current_cases_with_additional_PMC[1:max(age_max)])) / mean(current_cases_no_PMC[1:max(age_max)]) * 100)
    current_cases_reduction_with_additional_PMC_sev <- c(current_cases_reduction_with_additional_PMC_sev, (mean(current_cases_no_PMC[(1+max(age_max)):(2*max(age_max))]) - mean(current_cases_with_additional_PMC[(1+max(age_max)):(2*max(age_max))])) / mean(current_cases_no_PMC[(1+max(age_max)):(2*max(age_max))]) * 100)
    current_cases_reduction_with_additional_PMC_tot <- c(current_cases_reduction_with_additional_PMC_tot, (mean(current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))]) - mean(current_cases_with_additional_PMC[(1+2*max(age_max)):(3*max(age_max))])) / mean(current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))]) * 100)
    current_cases_reduction_with_additional_PMC_asym <- c(current_cases_reduction_with_additional_PMC_asym, (mean(current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))]) - mean(current_cases_with_additional_PMC[(1+3*max(age_max)):(4*max(age_max))])) / mean(current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))]) * 100)
    
  }
  
  # raw cases averted (with PMC) by infection class 
  cases_averted_with_PMC_clin <- (current_cases_no_PMC_clin - current_cases_with_PMC_clin)
  cases_averted_with_PMC_sev <- (current_cases_no_PMC_sev - current_cases_with_PMC_sev)
  cases_averted_with_PMC_tot <- (current_cases_no_PMC_tot - current_cases_with_PMC_tot)
  cases_averted_with_PMC_asym <- (current_cases_no_PMC_asym - current_cases_with_PMC_asym)
  
  cases_averted_with_additional_PMC_clin <- (current_cases_no_PMC_clin - current_cases_with_additional_PMC_clin)
  cases_averted_with_additional_PMC_sev <- (current_cases_no_PMC_sev - current_cases_with_additional_PMC_sev)
  cases_averted_with_additional_PMC_tot <- (current_cases_no_PMC_tot - current_cases_with_additional_PMC_tot)
  cases_averted_with_additional_PMC_asym <- (current_cases_no_PMC_asym - current_cases_with_additional_PMC_asym)
  
  # raw cases averted (with PMC) by infection class (per 1000 children in that age group)
  cases_averted_with_PMC_clin_per1000 <- (current_cases_no_PMC_clin_per1000 - current_cases_with_PMC_clin_per1000)
  cases_averted_with_PMC_sev_per1000 <- (current_cases_no_PMC_sev_per1000 - current_cases_with_PMC_sev_per1000)
  cases_averted_with_PMC_tot_per1000 <- (current_cases_no_PMC_tot_per1000 - current_cases_with_PMC_tot_per1000)
  cases_averted_with_PMC_asym_per1000 <- (current_cases_no_PMC_asym_per1000 - current_cases_with_PMC_asym_per1000)
  
  cases_averted_with_additional_PMC_clin_per1000 <- (current_cases_no_PMC_clin_per1000 - current_cases_with_additional_PMC_clin_per1000)
  cases_averted_with_additional_PMC_sev_per1000 <- (current_cases_no_PMC_sev_per1000 - current_cases_with_additional_PMC_sev_per1000)
  cases_averted_with_additional_PMC_tot_per1000 <- (current_cases_no_PMC_tot_per1000 - current_cases_with_additional_PMC_tot_per1000)
  cases_averted_with_additional_PMC_asym_per1000 <- (current_cases_no_PMC_asym_per1000 - current_cases_with_additional_PMC_asym_per1000)
  
  # annual cases averted (with PMC) by infection class 
  annual_cases_averted_with_PMC_clin <- (annual_current_cases_no_PMC_clin - annual_current_cases_with_PMC_clin)
  annual_cases_averted_with_PMC_sev <- (annual_current_cases_no_PMC_sev - annual_current_cases_with_PMC_sev)
  annual_cases_averted_with_PMC_tot <- (annual_current_cases_no_PMC_tot - annual_current_cases_with_PMC_tot)
  annual_cases_averted_with_PMC_asym <- (annual_current_cases_no_PMC_asym - annual_current_cases_with_PMC_asym)
  
  annual_cases_averted_with_additional_PMC_clin <- (annual_current_cases_no_PMC_clin - annual_current_cases_with_additional_PMC_clin)
  annual_cases_averted_with_additional_PMC_sev <- (annual_current_cases_no_PMC_sev - annual_current_cases_with_additional_PMC_sev)
  annual_cases_averted_with_additional_PMC_tot <- (annual_current_cases_no_PMC_tot - annual_current_cases_with_additional_PMC_tot)
  annual_cases_averted_with_additional_PMC_asym <- (annual_current_cases_no_PMC_asym - annual_current_cases_with_additional_PMC_asym)
  
  # annual cases averted (with PMC) by infection class (per 1000 children in that age group)
  annual_cases_averted_with_PMC_clin_per1000 <- (annual_current_cases_no_PMC_clin_per1000 - annual_current_cases_with_PMC_clin_per1000)
  annual_cases_averted_with_PMC_sev_per1000 <- (annual_current_cases_no_PMC_sev_per1000 - annual_current_cases_with_PMC_sev_per1000)
  annual_cases_averted_with_PMC_tot_per1000 <- (annual_current_cases_no_PMC_tot_per1000 - annual_current_cases_with_PMC_tot_per1000)
  annual_cases_averted_with_PMC_asym_per1000 <- (annual_current_cases_no_PMC_asym_per1000 - annual_current_cases_with_PMC_asym_per1000)
  
  annual_cases_averted_with_additional_PMC_clin_per1000 <- (annual_current_cases_no_PMC_clin_per1000 - annual_current_cases_with_additional_PMC_clin_per1000)
  annual_cases_averted_with_additional_PMC_sev_per1000 <- (annual_current_cases_no_PMC_sev_per1000 - annual_current_cases_with_additional_PMC_sev_per1000)
  annual_cases_averted_with_additional_PMC_tot_per1000 <- (annual_current_cases_no_PMC_tot_per1000 - annual_current_cases_with_additional_PMC_tot_per1000)
  annual_cases_averted_with_additional_PMC_asym_per1000 <- (annual_current_cases_no_PMC_asym_per1000 - annual_current_cases_with_additional_PMC_asym_per1000)
  
  # average reduction in cases by infection class 
  cases_reduction_with_PMC_clin <- (current_cases_no_PMC_clin - current_cases_with_PMC_clin)/current_cases_no_PMC_clin * 100
  cases_reduction_with_PMC_sev <- (current_cases_no_PMC_sev - current_cases_with_PMC_sev)/current_cases_no_PMC_sev * 100
  cases_reduction_with_PMC_tot <- (current_cases_no_PMC_tot - current_cases_with_PMC_tot)/current_cases_no_PMC_tot * 100
  cases_reduction_with_PMC_asym <- (current_cases_no_PMC_asym - current_cases_with_PMC_asym)/current_cases_no_PMC_asym * 100
  
  cases_reduction_with_additional_PMC_clin <- (current_cases_no_PMC_clin - current_cases_with_additional_PMC_clin)/current_cases_no_PMC_clin * 100
  cases_reduction_with_additional_PMC_sev <- (current_cases_no_PMC_sev - current_cases_with_additional_PMC_sev)/current_cases_no_PMC_sev * 100
  cases_reduction_with_additional_PMC_tot <- (current_cases_no_PMC_tot - current_cases_with_additional_PMC_tot)/current_cases_no_PMC_tot * 100
  cases_reduction_with_additional_PMC_asym <- (current_cases_no_PMC_asym - current_cases_with_additional_PMC_asym)/current_cases_no_PMC_asym * 100
  
  ##### APPEND VECTORS TO CALCULATION DATAFRAMES #####
  
  cases_no_PMC$clinical <- current_cases_no_PMC_clin
  cases_no_PMC$severe <- current_cases_no_PMC_sev
  cases_no_PMC$total <- current_cases_no_PMC_tot
  cases_no_PMC$asymptomatic <- current_cases_no_PMC_asym
  
  cases_no_PMC_per1000$clinical <- current_cases_no_PMC_clin_per1000
  cases_no_PMC_per1000$severe <- current_cases_no_PMC_sev_per1000
  cases_no_PMC_per1000$total <- current_cases_no_PMC_tot_per1000
  cases_no_PMC_per1000$asymptomatic <- current_cases_no_PMC_asym_per1000
  
  annual_cases_no_PMC$clinical <- annual_current_cases_no_PMC_clin
  annual_cases_no_PMC$severe <- annual_current_cases_no_PMC_sev
  annual_cases_no_PMC$total <- annual_current_cases_no_PMC_tot
  annual_cases_no_PMC$asymptomatic <- annual_current_cases_no_PMC_asym
  
  annual_cases_no_PMC_per1000$clinical <- annual_current_cases_no_PMC_clin_per1000
  annual_cases_no_PMC_per1000$severe <- annual_current_cases_no_PMC_sev_per1000
  annual_cases_no_PMC_per1000$total <- annual_current_cases_no_PMC_tot_per1000
  annual_cases_no_PMC_per1000$asymptomatic <- annual_current_cases_no_PMC_asym_per1000
  
  cases_averted_with_PMC$clinical <- cases_averted_with_PMC_clin
  cases_averted_with_PMC$severe <- cases_averted_with_PMC_sev
  cases_averted_with_PMC$total <- cases_averted_with_PMC_tot
  cases_averted_with_PMC$asymptomatic <- cases_averted_with_PMC_asym
  
  cases_averted_with_additional_PMC$clinical <- cases_averted_with_additional_PMC_clin
  cases_averted_with_additional_PMC$severe <- cases_averted_with_additional_PMC_sev
  cases_averted_with_additional_PMC$total <- cases_averted_with_additional_PMC_tot
  cases_averted_with_additional_PMC$asymptomatic <- cases_averted_with_additional_PMC_asym
  
  cases_averted_with_PMC_per1000$clinical <- cases_averted_with_PMC_clin_per1000
  cases_averted_with_PMC_per1000$severe <- cases_averted_with_PMC_sev_per1000
  cases_averted_with_PMC_per1000$total <- cases_averted_with_PMC_tot_per1000
  cases_averted_with_PMC_per1000$asymptomatic <- cases_averted_with_PMC_asym_per1000
  
  cases_averted_with_additional_PMC_per1000$clinical <- cases_averted_with_additional_PMC_clin_per1000
  cases_averted_with_additional_PMC_per1000$severe <- cases_averted_with_additional_PMC_sev_per1000
  cases_averted_with_additional_PMC_per1000$total <- cases_averted_with_additional_PMC_tot_per1000
  cases_averted_with_additional_PMC_per1000$asymptomatic <- cases_averted_with_additional_PMC_asym_per1000
  
  annual_cases_averted_with_PMC$clinical <- annual_cases_averted_with_PMC_clin
  annual_cases_averted_with_PMC$severe <- annual_cases_averted_with_PMC_sev
  annual_cases_averted_with_PMC$total <- annual_cases_averted_with_PMC_tot
  annual_cases_averted_with_PMC$asymptomatic <- annual_cases_averted_with_PMC_asym
  
  annual_cases_averted_with_additional_PMC$clinical <- annual_cases_averted_with_additional_PMC_clin
  annual_cases_averted_with_additional_PMC$severe <- annual_cases_averted_with_additional_PMC_sev
  annual_cases_averted_with_additional_PMC$total <- annual_cases_averted_with_additional_PMC_tot
  annual_cases_averted_with_additional_PMC$asymptomatic <- annual_cases_averted_with_additional_PMC_asym
  
  annual_cases_averted_with_PMC_per1000$clinical <- annual_cases_averted_with_PMC_clin_per1000
  annual_cases_averted_with_PMC_per1000$severe <- annual_cases_averted_with_PMC_sev_per1000
  annual_cases_averted_with_PMC_per1000$total <- annual_cases_averted_with_PMC_tot_per1000
  annual_cases_averted_with_PMC_per1000$asymptomatic <- annual_cases_averted_with_PMC_asym_per1000
  
  annual_cases_averted_with_additional_PMC_per1000$clinical <- annual_cases_averted_with_additional_PMC_clin_per1000
  annual_cases_averted_with_additional_PMC_per1000$severe <- annual_cases_averted_with_additional_PMC_sev_per1000
  annual_cases_averted_with_additional_PMC_per1000$total <- annual_cases_averted_with_additional_PMC_tot_per1000
  annual_cases_averted_with_additional_PMC_per1000$asymptomatic <- annual_cases_averted_with_additional_PMC_asym_per1000
  
  cases_with_PMC$clinical <- current_cases_with_PMC_clin
  cases_with_PMC$severe <- current_cases_with_PMC_sev
  cases_with_PMC$total <- current_cases_with_PMC_tot
  cases_with_PMC$asymptomatic <- current_cases_with_PMC_asym
  
  cases_with_additional_PMC$clinical <- current_cases_with_additional_PMC_clin
  cases_with_additional_PMC$severe <- current_cases_with_additional_PMC_sev
  cases_with_additional_PMC$total <- current_cases_with_additional_PMC_tot
  cases_with_additional_PMC$asymptomatic <- current_cases_with_additional_PMC_asym
  
  cases_with_PMC_per1000$clinical <- current_cases_with_PMC_clin_per1000
  cases_with_PMC_per1000$severe <- current_cases_with_PMC_sev_per1000
  cases_with_PMC_per1000$total <- current_cases_with_PMC_tot_per1000
  cases_with_PMC_per1000$asymptomatic <- current_cases_with_PMC_asym_per1000
  
  cases_with_additional_PMC_per1000$clinical <- current_cases_with_additional_PMC_clin_per1000
  cases_with_additional_PMC_per1000$severe <- current_cases_with_additional_PMC_sev_per1000
  cases_with_additional_PMC_per1000$total <- current_cases_with_additional_PMC_tot_per1000
  cases_with_additional_PMC_per1000$asymptomatic <- current_cases_with_additional_PMC_asym_per1000
  
  annual_cases_with_PMC$clinical <- annual_current_cases_with_PMC_clin
  annual_cases_with_PMC$severe <- annual_current_cases_with_PMC_sev
  annual_cases_with_PMC$total <- annual_current_cases_with_PMC_tot
  annual_cases_with_PMC$asymptomatic <- annual_current_cases_with_PMC_asym
  
  annual_cases_with_additional_PMC$clinical <- annual_current_cases_with_additional_PMC_clin
  annual_cases_with_additional_PMC$severe <- annual_current_cases_with_additional_PMC_sev
  annual_cases_with_additional_PMC$total <- annual_current_cases_with_additional_PMC_tot
  annual_cases_with_additional_PMC$asymptomatic <- annual_current_cases_with_additional_PMC_asym
  
  annual_cases_with_PMC_per1000$clinical <- annual_current_cases_with_PMC_clin_per1000
  annual_cases_with_PMC_per1000$severe <- annual_current_cases_with_PMC_sev_per1000
  annual_cases_with_PMC_per1000$total <- annual_current_cases_with_PMC_tot_per1000
  annual_cases_with_PMC_per1000$asymptomatic <- annual_current_cases_with_PMC_asym_per1000
  
  annual_cases_with_additional_PMC_per1000$clinical <- annual_current_cases_with_additional_PMC_clin_per1000
  annual_cases_with_additional_PMC_per1000$severe <- annual_current_cases_with_additional_PMC_sev_per1000
  annual_cases_with_additional_PMC_per1000$total <- annual_current_cases_with_additional_PMC_tot_per1000
  annual_cases_with_additional_PMC_per1000$asymptomatic <- annual_current_cases_with_additional_PMC_asym_per1000
  
  cases_reduction_with_PMC$clinical <- current_cases_reduction_with_PMC_clin
  cases_reduction_with_PMC$severe <- current_cases_reduction_with_PMC_sev
  cases_reduction_with_PMC$total <- current_cases_reduction_with_PMC_tot
  cases_reduction_with_PMC$asymptomatic <- current_cases_reduction_with_PMC_asym
  
  cases_reduction_with_additional_PMC$clinical <- current_cases_reduction_with_additional_PMC_clin
  cases_reduction_with_additional_PMC$severe <- current_cases_reduction_with_additional_PMC_sev
  cases_reduction_with_additional_PMC$total <- current_cases_reduction_with_additional_PMC_tot
  cases_reduction_with_additional_PMC$asymptomatic <- current_cases_reduction_with_additional_PMC_asym
  
  
  ##### SET UP CALCULATION DATAFRAMES (6 MONTH AGE GROUP INTERVALS) #####
  
  # raw baseline cases (no PMC)
  cases_no_PMC_sixmonth<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                     age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                     NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                     units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))
  
  # baseline cases (no PMC) for every age group (per 1000 children in that age group)
  cases_no_PMC_sixmonth_per1000<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                             age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                             NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                             units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))
  
  # cases (with PMC) 
  cases_with_PMC_sixmonth<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                       age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                       NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                       units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))
  
  # cases (with PMC) (per 1000 children in that age group)
  cases_with_PMC_sixmonth_per1000<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                               age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                               NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                               units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))
  
  # raw cases averted (with PMC) 
  cases_averted_with_PMC_sixmonth<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                               age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                               NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                               units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))
  
  # raw cases averted (with PMC) (per 1000 children in that age group)
  cases_averted_with_PMC_sixmonth_per1000<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                                       age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                                       NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                                       units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))
  
  # average % reduction in cases (with PMC)
  cases_reduction_with_PMC_sixmonth <- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                                  age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                                  NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                                  units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))
  
  # cases (with PMC) 
  cases_with_additional_PMC_sixmonth<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                                  age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                                  NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                                  units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))
  
  # cases (with PMC) (per 1000 children in that age group)
  cases_with_additional_PMC_sixmonth_per1000<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                                          age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                                          NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                                          units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))
  
  # raw cases averted (with PMC) 
  cases_averted_with_additional_PMC_sixmonth<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                                          age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                                          NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                                          units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))
  
  # raw cases averted (with PMC) (per 1000 children in that age group)
  cases_averted_with_additional_PMC_sixmonth_per1000<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                                                  age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                                                  NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                                                  units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))
  
  # average % reduction in cases (with PMC)
  cases_reduction_with_additional_PMC_sixmonth <- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                                             age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                                             NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                                             units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))
  
  ##### INITIALISE VECTORS FOR CALCULATIONS (6 MONTH AGE GROUP INTERVALS) #####
  
  # raw cases
  cases_no_PMC_clin <- cases_no_PMC_sev <- cases_no_PMC_tot <- cases_no_PMC_asym <- c()
  cases_with_PMC_clin <- cases_with_PMC_sev <- cases_with_PMC_tot <- cases_with_PMC_asym <- c()
  cases_with_additional_PMC_clin <- cases_with_additional_PMC_sev <- cases_with_additional_PMC_tot <- cases_with_additional_PMC_asym <- c()
  
  # cases per 1000 children in that age group
  cases_no_PMC_clin_per1000 <- cases_no_PMC_sev_per1000 <- cases_no_PMC_tot_per1000 <- cases_no_PMC_asym_per1000 <- c()
  cases_with_PMC_clin_per1000 <- cases_with_PMC_sev_per1000 <- cases_with_PMC_tot_per1000 <- cases_with_PMC_asym_per1000 <- c()
  cases_with_additional_PMC_clin_per1000 <- cases_with_additional_PMC_sev_per1000 <- cases_with_additional_PMC_tot_per1000 <- cases_with_additional_PMC_asym_per1000 <- c()
  
  # cases averted (with PMC) 
  cases_averted_with_PMC_clin <- cases_averted_with_PMC_sev <- cases_averted_with_PMC_tot <- cases_averted_with_PMC_asym <- c()
  cases_averted_with_additional_PMC_clin <- cases_averted_with_additional_PMC_sev <- cases_averted_with_additional_PMC_tot <- cases_averted_with_additional_PMC_asym <- c()
  
  # cases averted (per 1000 children in that age group)
  cases_averted_with_PMC_clin_per1000 <- cases_averted_with_PMC_sev_per1000 <- cases_averted_with_PMC_tot_per1000 <- cases_averted_with_PMC_asym_per1000 <- c()
  cases_averted_with_additional_PMC_clin_per1000 <- cases_averted_with_additional_PMC_sev_per1000 <- cases_averted_with_additional_PMC_tot_per1000 <- cases_averted_with_additional_PMC_asym_per1000 <- c()
  
  # cases reduction 
  cases_reduction_with_PMC_clin <- cases_reduction_with_PMC_sev <- cases_reduction_with_PMC_tot <- cases_reduction_with_PMC_asym <- c()
  cases_reduction_with_additional_PMC_clin <- cases_reduction_with_additional_PMC_sev <- cases_reduction_with_additional_PMC_tot <- cases_reduction_with_additional_PMC_asym <- c()
  
  ##### RUN CALCULATIONS ACROSS WHOLE COUNTRY (6 MONTH AGE GROUP INTERVALS) #####
  
  for (i in 1:length(area_names)) {
    
    # raw cases (no PMC) by infection class
    current_cases_no_PMC_clin <- (cases_no_PMC %>% filter(NAME_2 == area_names[i]))$clinical
    current_cases_no_PMC_sev <- (cases_no_PMC %>% filter(NAME_2 == area_names[i]))$severe
    current_cases_no_PMC_tot <- (cases_no_PMC %>% filter(NAME_2 == area_names[i]))$total
    current_cases_no_PMC_asym <- (cases_no_PMC %>% filter(NAME_2 == area_names[i]))$asymptomatic
    
    # raw cases (with PMC) by infection class
    current_cases_with_PMC_clin <- (cases_with_PMC %>% filter(NAME_2 == area_names[i]))$clinical
    current_cases_with_PMC_sev <- (cases_with_PMC %>% filter(NAME_2 == area_names[i]))$severe
    current_cases_with_PMC_tot <- (cases_with_PMC %>% filter(NAME_2 == area_names[i]))$total
    current_cases_with_PMC_asym <- (cases_with_PMC %>% filter(NAME_2 == area_names[i]))$asymptomatic
    
    current_cases_with_additional_PMC_clin <- (cases_with_additional_PMC %>% filter(NAME_2 == area_names[i]))$clinical
    current_cases_with_additional_PMC_sev <- (cases_with_additional_PMC %>% filter(NAME_2 == area_names[i]))$severe
    current_cases_with_additional_PMC_tot <- (cases_with_additional_PMC %>% filter(NAME_2 == area_names[i]))$total
    current_cases_with_additional_PMC_asym <- (cases_with_additional_PMC %>% filter(NAME_2 == area_names[i]))$asymptomatic
    
    # cases (no PMC) by infection class (per 1000 children in that age group)
    current_cases_no_PMC_clin_per1000 <- (incidence_ppy_df %>% filter(area == area_names[i], infection_class == "clinical"))$value * 1000
    current_cases_no_PMC_sev_per1000 <- (incidence_ppy_df %>% filter(area == area_names[i], infection_class == "severe"))$value * 1000
    current_cases_no_PMC_tot_per1000 <- (incidence_ppy_df %>% filter(area == area_names[i], infection_class == "total"))$value * 1000
    current_cases_no_PMC_asym_per1000 <- (incidence_ppy_df %>% filter(area == area_names[i], infection_class == "asymptomatic"))$value * 1000
    
    # cases (with PMC) by infection class (per 1000 children in that age group)
    current_cases_with_PMC_clin_per1000 <- (PMC_impact_ppy %>% filter(area == area_names[i], infection_class == "clinical"))$value * 1000
    current_cases_with_PMC_sev_per1000 <- (PMC_impact_ppy %>% filter(area == area_names[i], infection_class == "severe"))$value * 1000
    current_cases_with_PMC_tot_per1000 <- (PMC_impact_ppy %>% filter(area == area_names[i], infection_class == "total"))$value * 1000
    current_cases_with_PMC_asym_per1000 <- (PMC_impact_ppy %>% filter(area == area_names[i], infection_class == "asymptomatic"))$value * 1000
    
    current_cases_with_additional_PMC_clin_per1000 <- (PMC_impact_ppy_additional_doses %>% filter(area == area_names[i], infection_class == "clinical"))$value * 1000
    current_cases_with_additional_PMC_sev_per1000 <- (PMC_impact_ppy_additional_doses %>% filter(area == area_names[i], infection_class == "severe"))$value * 1000
    current_cases_with_additional_PMC_tot_per1000 <- (PMC_impact_ppy_additional_doses %>% filter(area == area_names[i], infection_class == "total"))$value * 1000
    current_cases_with_additional_PMC_asym_per1000 <- (PMC_impact_ppy_additional_doses %>% filter(area == area_names[i], infection_class == "asymptomatic"))$value * 1000
    
    
    # split data into each 6 month age group for each infection class (no PMC)
    split_current_cases_no_PMC_clin <- split(current_cases_no_PMC_clin, ceiling(seq_along(current_cases_no_PMC_clin)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_no_PMC_clin) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_no_PMC_sev <- split(current_cases_no_PMC_sev, ceiling(seq_along(current_cases_no_PMC_sev)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_no_PMC_sev) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_no_PMC_tot <- split(current_cases_no_PMC_tot, ceiling(seq_along(current_cases_no_PMC_tot)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_no_PMC_tot) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_no_PMC_asym <- split(current_cases_no_PMC_asym, ceiling(seq_along(current_cases_no_PMC_asym)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_no_PMC_asym) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    
    # split data into each 6 month age group for each infection class (with PMC)
    split_current_cases_with_PMC_clin <- split(current_cases_with_PMC_clin, ceiling(seq_along(current_cases_with_PMC_clin)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_with_PMC_clin) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_with_PMC_sev <- split(current_cases_with_PMC_sev, ceiling(seq_along(current_cases_with_PMC_sev)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_with_PMC_sev) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_with_PMC_tot <- split(current_cases_with_PMC_tot, ceiling(seq_along(current_cases_with_PMC_tot)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_with_PMC_tot) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_with_PMC_asym <- split(current_cases_with_PMC_asym, ceiling(seq_along(current_cases_with_PMC_asym)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_with_PMC_asym) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    
    split_current_cases_with_additional_PMC_clin <- split(current_cases_with_additional_PMC_clin, ceiling(seq_along(current_cases_with_additional_PMC_clin)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_with_additional_PMC_clin) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_with_additional_PMC_sev <- split(current_cases_with_additional_PMC_sev, ceiling(seq_along(current_cases_with_additional_PMC_sev)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_with_additional_PMC_sev) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_with_additional_PMC_tot <- split(current_cases_with_additional_PMC_tot, ceiling(seq_along(current_cases_with_additional_PMC_tot)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_with_additional_PMC_tot) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_with_additional_PMC_asym <- split(current_cases_with_additional_PMC_asym, ceiling(seq_along(current_cases_with_additional_PMC_asym)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_with_additional_PMC_asym) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    # split data into each 6 month age group for each infection class (no PMC) (per 1000 children in that age group)
    split_current_cases_no_PMC_clin_per1000 <- split(current_cases_no_PMC_clin_per1000, ceiling(seq_along(current_cases_no_PMC_clin_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_no_PMC_clin_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_no_PMC_sev_per1000 <- split(current_cases_no_PMC_sev_per1000, ceiling(seq_along(current_cases_no_PMC_sev_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_no_PMC_sev_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_no_PMC_tot_per1000 <- split(current_cases_no_PMC_tot_per1000, ceiling(seq_along(current_cases_no_PMC_tot_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_no_PMC_tot_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_no_PMC_asym_per1000 <- split(current_cases_no_PMC_asym_per1000, ceiling(seq_along(current_cases_no_PMC_asym_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_no_PMC_asym_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    
    # split data into each 6 month age group for each infection class (with PMC) (per 1000 children in that age group)
    split_current_cases_with_PMC_clin_per1000 <- split(current_cases_with_PMC_clin_per1000, ceiling(seq_along(current_cases_with_PMC_clin_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_with_PMC_clin_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_with_PMC_sev_per1000 <- split(current_cases_with_PMC_sev_per1000, ceiling(seq_along(current_cases_with_PMC_sev_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_with_PMC_sev_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_with_PMC_tot_per1000 <- split(current_cases_with_PMC_tot_per1000, ceiling(seq_along(current_cases_with_PMC_tot_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_with_PMC_tot_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_with_PMC_asym_per1000 <- split(current_cases_with_PMC_asym_per1000, ceiling(seq_along(current_cases_with_PMC_asym_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_with_PMC_asym_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    
    split_current_cases_with_additional_PMC_clin_per1000 <- split(current_cases_with_additional_PMC_clin_per1000, ceiling(seq_along(current_cases_with_additional_PMC_clin_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_with_additional_PMC_clin_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_with_additional_PMC_sev_per1000 <- split(current_cases_with_additional_PMC_sev_per1000, ceiling(seq_along(current_cases_with_additional_PMC_sev_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_with_additional_PMC_sev_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_with_additional_PMC_tot_per1000 <- split(current_cases_with_additional_PMC_tot_per1000, ceiling(seq_along(current_cases_with_additional_PMC_tot_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_with_additional_PMC_tot_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_with_additional_PMC_asym_per1000 <- split(current_cases_with_additional_PMC_asym_per1000, ceiling(seq_along(current_cases_with_additional_PMC_asym_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    names(split_current_cases_with_additional_PMC_asym_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    
    for (j in 1:length(sixmonth_intervals[1:no_sixmonth_intervals])) {
      
      # sum of cases (no PMC) for each 6 month age group
      cases_no_PMC_clin<- c(cases_no_PMC_clin, sum(split_current_cases_no_PMC_clin[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_no_PMC_sev<- c(cases_no_PMC_sev, sum(split_current_cases_no_PMC_sev[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_no_PMC_tot<- c(cases_no_PMC_tot, sum(split_current_cases_no_PMC_tot[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_no_PMC_asym<- c(cases_no_PMC_asym, sum(split_current_cases_no_PMC_asym[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      
      # sum of cases (with PMC) for each 6 month age group
      cases_with_PMC_clin<- c(cases_with_PMC_clin, sum(split_current_cases_with_PMC_clin[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_with_PMC_sev<- c(cases_with_PMC_sev, sum(split_current_cases_with_PMC_sev[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_with_PMC_tot<- c(cases_with_PMC_tot, sum(split_current_cases_with_PMC_tot[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_with_PMC_asym<- c(cases_with_PMC_asym, sum(split_current_cases_with_PMC_asym[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      
      cases_with_additional_PMC_clin<- c(cases_with_additional_PMC_clin, sum(split_current_cases_with_additional_PMC_clin[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_with_additional_PMC_sev<- c(cases_with_additional_PMC_sev, sum(split_current_cases_with_additional_PMC_sev[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_with_additional_PMC_tot<- c(cases_with_additional_PMC_tot, sum(split_current_cases_with_additional_PMC_tot[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_with_additional_PMC_asym<- c(cases_with_additional_PMC_asym, sum(split_current_cases_with_additional_PMC_asym[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      
      # sum of cases (no PMC) for each 6 month age group (per 1000 children in that age group)
      cases_no_PMC_clin_per1000<- c(cases_no_PMC_clin_per1000, mean(split_current_cases_no_PMC_clin_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_no_PMC_sev_per1000<- c(cases_no_PMC_sev_per1000, mean(split_current_cases_no_PMC_sev_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_no_PMC_tot_per1000<- c(cases_no_PMC_tot_per1000, mean(split_current_cases_no_PMC_tot_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_no_PMC_asym_per1000<- c(cases_no_PMC_asym_per1000, mean(split_current_cases_no_PMC_asym_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      
      # sum of cases (with PMC) for each 6 month age group (per 1000 children in that age group)
      cases_with_PMC_clin_per1000<- c(cases_with_PMC_clin_per1000, mean(split_current_cases_with_PMC_clin_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_with_PMC_sev_per1000<- c(cases_with_PMC_sev_per1000, mean(split_current_cases_with_PMC_sev_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_with_PMC_tot_per1000<- c(cases_with_PMC_tot_per1000, mean(split_current_cases_with_PMC_tot_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_with_PMC_asym_per1000<- c(cases_with_PMC_asym_per1000, mean(split_current_cases_with_PMC_asym_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      
      cases_with_additional_PMC_clin_per1000<- c(cases_with_additional_PMC_clin_per1000, mean(split_current_cases_with_additional_PMC_clin_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_with_additional_PMC_sev_per1000<- c(cases_with_additional_PMC_sev_per1000, mean(split_current_cases_with_additional_PMC_sev_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_with_additional_PMC_tot_per1000<- c(cases_with_additional_PMC_tot_per1000, mean(split_current_cases_with_additional_PMC_tot_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_with_additional_PMC_asym_per1000<- c(cases_with_additional_PMC_asym_per1000, mean(split_current_cases_with_additional_PMC_asym_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      
    }
    
  }  
  
  # raw cases averted (with PMC) by infection class 
  cases_averted_with_PMC_clin <- (cases_no_PMC_clin - cases_with_PMC_clin)
  cases_averted_with_PMC_sev <- (cases_no_PMC_sev - cases_with_PMC_sev)
  cases_averted_with_PMC_tot <- (cases_no_PMC_tot - cases_with_PMC_tot)
  cases_averted_with_PMC_asym <- (cases_no_PMC_asym - cases_with_PMC_asym)
  
  cases_averted_with_additional_PMC_clin <- (cases_no_PMC_clin - cases_with_additional_PMC_clin)
  cases_averted_with_additional_PMC_sev <- (cases_no_PMC_sev - cases_with_additional_PMC_sev)
  cases_averted_with_additional_PMC_tot <- (cases_no_PMC_tot - cases_with_additional_PMC_tot)
  cases_averted_with_additional_PMC_asym <- (cases_no_PMC_asym - cases_with_additional_PMC_asym)
  
  # cases averted (with PMC) by infection class (per 1000 children in that age group)
  cases_averted_with_PMC_clin_per1000 <- (cases_no_PMC_clin_per1000 - cases_with_PMC_clin_per1000)
  cases_averted_with_PMC_sev_per1000 <- (cases_no_PMC_sev_per1000 - cases_with_PMC_sev_per1000)
  cases_averted_with_PMC_tot_per1000 <- (cases_no_PMC_tot_per1000 - cases_with_PMC_tot_per1000)
  cases_averted_with_PMC_asym_per1000 <- (cases_no_PMC_asym_per1000 - cases_with_PMC_asym_per1000)
  
  cases_averted_with_additional_PMC_clin_per1000 <- (cases_no_PMC_clin_per1000 - cases_with_additional_PMC_clin_per1000)
  cases_averted_with_additional_PMC_sev_per1000 <- (cases_no_PMC_sev_per1000 - cases_with_additional_PMC_sev_per1000)
  cases_averted_with_additional_PMC_tot_per1000 <- (cases_no_PMC_tot_per1000 - cases_with_additional_PMC_tot_per1000)
  cases_averted_with_additional_PMC_asym_per1000 <- (cases_no_PMC_asym_per1000 - cases_with_additional_PMC_asym_per1000)
  
  # reduction in cases (with PMC) by infection class
  cases_reduction_with_PMC_clin <- (cases_no_PMC_clin - cases_with_PMC_clin)/cases_no_PMC_clin * 100
  cases_reduction_with_PMC_sev <- (cases_no_PMC_sev - cases_with_PMC_sev)/cases_no_PMC_sev * 100
  cases_reduction_with_PMC_tot <- (cases_no_PMC_tot - cases_with_PMC_tot)/cases_no_PMC_tot * 100
  cases_reduction_with_PMC_asym <- (cases_no_PMC_asym - cases_with_PMC_asym)/cases_no_PMC_asym * 100
  
  cases_reduction_with_additional_PMC_clin <- (cases_no_PMC_clin - cases_with_additional_PMC_clin)/cases_no_PMC_clin * 100
  cases_reduction_with_additional_PMC_sev <- (cases_no_PMC_sev - cases_with_additional_PMC_sev)/cases_no_PMC_sev * 100
  cases_reduction_with_additional_PMC_tot <- (cases_no_PMC_tot - cases_with_additional_PMC_tot)/cases_no_PMC_tot * 100
  cases_reduction_with_additional_PMC_asym <- (cases_no_PMC_asym - cases_with_additional_PMC_asym)/cases_no_PMC_asym * 100
  
  
  ##### APPEND VECTORS TO CALCULATION DATAFRAMES (6 MONTH AGE GROUP INTERVALS) #####
  
  cases_no_PMC_sixmonth$clinical <- cases_no_PMC_clin
  cases_no_PMC_sixmonth$severe <- cases_no_PMC_sev
  cases_no_PMC_sixmonth$total <- cases_no_PMC_tot
  cases_no_PMC_sixmonth$asymptomatic <- cases_no_PMC_asym
  
  cases_with_PMC_sixmonth$clinical<- cases_with_PMC_clin
  cases_with_PMC_sixmonth$severe<- cases_with_PMC_sev
  cases_with_PMC_sixmonth$total<- cases_with_PMC_tot
  cases_with_PMC_sixmonth$asymptomatic<- cases_with_PMC_asym
  
  cases_with_additional_PMC_sixmonth$clinical<- cases_with_additional_PMC_clin
  cases_with_additional_PMC_sixmonth$severe<- cases_with_additional_PMC_sev
  cases_with_additional_PMC_sixmonth$total<- cases_with_additional_PMC_tot
  cases_with_additional_PMC_sixmonth$asymptomatic<- cases_with_additional_PMC_asym
  
  cases_no_PMC_sixmonth_per1000$clinical <- cases_no_PMC_clin_per1000
  cases_no_PMC_sixmonth_per1000$severe <- cases_no_PMC_sev_per1000
  cases_no_PMC_sixmonth_per1000$total <- cases_no_PMC_tot_per1000
  cases_no_PMC_sixmonth_per1000$asymptomatic <- cases_no_PMC_asym_per1000
  
  cases_with_PMC_sixmonth_per1000$clinical<- cases_with_PMC_clin_per1000
  cases_with_PMC_sixmonth_per1000$severe<- cases_with_PMC_sev_per1000
  cases_with_PMC_sixmonth_per1000$total<- cases_with_PMC_tot_per1000
  cases_with_PMC_sixmonth_per1000$asymptomatic<- cases_with_PMC_asym_per1000
  
  cases_with_additional_PMC_sixmonth_per1000$clinical<- cases_with_additional_PMC_clin_per1000
  cases_with_additional_PMC_sixmonth_per1000$severe<- cases_with_additional_PMC_sev_per1000
  cases_with_additional_PMC_sixmonth_per1000$total<- cases_with_additional_PMC_tot_per1000
  cases_with_additional_PMC_sixmonth_per1000$asymptomatic<- cases_with_additional_PMC_asym_per1000
  
  cases_averted_with_PMC_sixmonth$clinical <- cases_averted_with_PMC_clin
  cases_averted_with_PMC_sixmonth$severe <- cases_averted_with_PMC_sev
  cases_averted_with_PMC_sixmonth$total <- cases_averted_with_PMC_tot
  cases_averted_with_PMC_sixmonth$asymptomatic <- cases_averted_with_PMC_asym
  
  cases_averted_with_additional_PMC_sixmonth$clinical <- cases_averted_with_additional_PMC_clin
  cases_averted_with_additional_PMC_sixmonth$severe <- cases_averted_with_additional_PMC_sev
  cases_averted_with_additional_PMC_sixmonth$total <- cases_averted_with_additional_PMC_tot
  cases_averted_with_additional_PMC_sixmonth$asymptomatic <- cases_averted_with_additional_PMC_asym
  
  cases_averted_with_PMC_sixmonth_per1000$clinical <- cases_averted_with_PMC_clin_per1000
  cases_averted_with_PMC_sixmonth_per1000$severe <- cases_averted_with_PMC_sev_per1000
  cases_averted_with_PMC_sixmonth_per1000$total <- cases_averted_with_PMC_tot_per1000
  cases_averted_with_PMC_sixmonth_per1000$asymptomatic <- cases_averted_with_PMC_asym_per1000
  
  cases_averted_with_additional_PMC_sixmonth_per1000$clinical <- cases_averted_with_additional_PMC_clin_per1000
  cases_averted_with_additional_PMC_sixmonth_per1000$severe <- cases_averted_with_additional_PMC_sev_per1000
  cases_averted_with_additional_PMC_sixmonth_per1000$total <- cases_averted_with_additional_PMC_tot_per1000
  cases_averted_with_additional_PMC_sixmonth_per1000$asymptomatic <- cases_averted_with_additional_PMC_asym_per1000
  
  cases_reduction_with_PMC_sixmonth$clinical <- cases_reduction_with_PMC_clin
  cases_reduction_with_PMC_sixmonth$severe <- cases_reduction_with_PMC_sev
  cases_reduction_with_PMC_sixmonth$total <- cases_reduction_with_PMC_tot
  cases_reduction_with_PMC_sixmonth$asymptomatic <- cases_reduction_with_PMC_asym
  
  cases_reduction_with_additional_PMC_sixmonth$clinical <- cases_reduction_with_additional_PMC_clin
  cases_reduction_with_additional_PMC_sixmonth$severe <- cases_reduction_with_additional_PMC_sev
  cases_reduction_with_additional_PMC_sixmonth$total <- cases_reduction_with_additional_PMC_tot
  cases_reduction_with_additional_PMC_sixmonth$asymptomatic <- cases_reduction_with_additional_PMC_asym
  
  
  
  ##### STORE DATAFRAMES IN CSV FORMAT #####
  
  write.csv(cases_no_PMC, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_no_PMC_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(annual_cases_no_PMC, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/annual_cases_no_PMC_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_with_PMC, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_with_PMC_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(annual_cases_with_PMC, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/annual_cases_with_PMC_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_with_additional_PMC, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_with_additional_PMC_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(annual_cases_with_additional_PMC, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/annual_cases_with_additional_PMC_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_no_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_no_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(annual_cases_no_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/annual_cases_no_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_with_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_with_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(annual_cases_with_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/annual_cases_with_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_averted_with_PMC, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_averted_with_PMC_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(annual_cases_averted_with_PMC, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/annual_cases_averted_with_PMC_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_averted_with_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_averted_with_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(annual_cases_averted_with_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/annual_cases_averted_with_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_reduction_with_PMC, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_reduction_with_PMC_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_with_additional_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_with_additional_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(annual_cases_with_additional_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/annual_cases_with_additional_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_averted_with_additional_PMC, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_averted_with_additional_PMC_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(annual_cases_averted_with_additional_PMC, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/annual_cases_averted_with_additional_PMC_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_averted_with_additional_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_averted_with_additional_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(annual_cases_averted_with_additional_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/annual_cases_averted_with_additional_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_reduction_with_additional_PMC, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_reduction_with_additional_PMC_", rur_or_urb, ".csv"), row.names=FALSE)
  
  write.csv(cases_no_PMC_sixmonth, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_no_PMC_sixmonth_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_no_PMC_sixmonth_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_no_PMC_sixmonth_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_with_PMC_sixmonth, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_with_PMC_sixmonth_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_with_PMC_sixmonth_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_with_PMC_sixmonth_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_averted_with_PMC_sixmonth, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_averted_with_PMC_sixmonth_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_averted_with_PMC_sixmonth_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_averted_with_PMC_sixmonth_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_reduction_with_PMC_sixmonth, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_reduction_with_PMC_sixmonth_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_with_additional_PMC_sixmonth, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_with_additional_PMC_sixmonth_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_with_additional_PMC_sixmonth_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_with_additional_PMC_sixmonth_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_averted_with_additional_PMC_sixmonth, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_averted_with_additional_PMC_sixmonth_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_averted_with_additional_PMC_sixmonth_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_averted_with_additional_PMC_sixmonth_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
  write.csv(cases_reduction_with_additional_PMC_sixmonth, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_reduction_with_additional_PMC_sixmonth_", rur_or_urb, ".csv"), row.names=FALSE)
  
}


##### GRAPHS #####

# shape file for country 
adm1<- sf::st_read(paste0(getwd(), "/", country_code, "/", country_code,"_shapefiles/gadm41_", country_code, "_1.shp"))
adm1$NAME_2 <- stri_trans_general(str=gsub("-", "_", adm1$NAME_1), id = "Latin-ASCII")

# CASES (per 1000 children in that age group) (NO PMC)

# link data with shape file
combined_df_no_PMC_cases <- merge(cases_no_PMC_sixmonth_per1000, adm1, by="NAME_2") 


impact_graph_with_no_PMC_0_182 <- ggplot(combined_df_no_PMC_cases %>% filter(age_group == "n_age_0_182"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
  theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
  geom_sf(data=adm1)+
  geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
  #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
  ggtitle(paste0("Age: 0-6 months")) + labs(fill="Clinical cases \nper 1000 children") +
  scale_fill_viridis_c(limits= c(0, 1000)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())



if (no_sixmonth_intervals > 1) {
  impact_graph_with_no_PMC_183_364 <- ggplot(combined_df_no_PMC_cases %>% filter(age_group == "n_age_183_364"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 6-12 months")) + labs(fill="Clinical cases \nper 1000 children") +
    scale_fill_viridis_c(limits= c(450, 4000)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 2) {
  impact_graph_with_no_PMC_365_547 <- ggplot(combined_df_no_PMC_cases %>% filter(age_group == "n_age_365_547"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 12-18 months")) + labs(fill="Clinical cases \nper 1000 children") +
    scale_fill_viridis_c(limits= c(450, 4000)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 3) {
  impact_graph_with_no_PMC_548_729 <- ggplot(combined_df_no_PMC_cases %>% filter(age_group == "n_age_548_729"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 18-24 months")) + labs(fill="Clinical cases \nper 1000 children") +
    scale_fill_viridis_c(limits= c(450, 4000)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 4) {
  impact_graph_with_no_PMC_730_912 <- ggplot(combined_df_no_PMC_cases %>% filter(age_group == "n_age_730_912"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 24-30 months")) + labs(fill="Clinical cases \nper 1000 children") +
    scale_fill_viridis_c(limits= c(450, 4000)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}


plot_impact <- ggarrange(impact_graph_with_no_PMC_0_182, impact_graph_with_no_PMC_183_364 , nrow=1, ncol=2, common.legend = TRUE, legend = "right")
annotate_figure(plot_impact) #, top = text_grob(paste0("Clinical cases per 1000 children in ", country_code, " in the absence of PMC")))

#ggsave(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/no_PMC_cases_per_1000.jpeg"), units="in", width=8, height=5, dpi=300)


# CASES (per 1000 children in that age group) (with PMC)

# link data with shape file
combined_df_with_PMC_cases <- merge(cases_with_PMC_sixmonth_per1000, adm1, by="NAME_2") 

impact_graph_with_PMC_0_182 <- ggplot(combined_df_with_PMC_cases %>% filter(age_group == "n_age_0_182"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
  theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
  geom_sf(data=adm1)+
  geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
  #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
  ggtitle(paste0("Age: 0-6 months")) + labs(fill=paste0("Clinical cases \nper 1000 children")) +
  scale_fill_viridis_c(limits=c(0,2300)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

if (no_sixmonth_intervals > 1) {
  impact_graph_with_PMC_183_364 <- ggplot(combined_df_with_PMC_cases %>% filter(age_group == "n_age_183_364"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 6-12 months")) + labs(fill=paste0("Clinical cases \nper 1000 children")) +
    scale_fill_viridis_c(limits=c(0,2300)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 2) {
  impact_graph_with_PMC_365_547 <- ggplot(combined_df_with_PMC_cases %>% filter(age_group == "n_age_365_547"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 12-18 months")) + labs(fill=paste0("Clinical cases \nper 1000 children")) +
    scale_fill_viridis_c(limits=c(0,2300)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 3) {
  impact_graph_with_PMC_548_729 <- ggplot(combined_df_with_PMC_cases %>% filter(age_group == "n_age_548_729"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 18-24 months")) + labs(fill=paste0("Clinical cases \nper 1000 children")) +
    scale_fill_viridis_c(limits=c(0,2300)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 4) {
  impact_graph_with_PMC_730_912 <- ggplot(combined_df_with_PMC_cases %>% filter(age_group == "n_age_730_912"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 24-30 months")) + labs(fill=paste0("Clinical cases \nper 1000 children")) +
    scale_fill_viridis_c(limits=c(0,2300)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}


plot_impact <- ggarrange(impact_graph_with_PMC_0_182, impact_graph_with_PMC_183_364 , nrow=1, ncol=2, common.legend = TRUE, legend = "right")
annotate_figure(plot_impact, top = text_grob(paste0("Clinical cases per 1000 children in ", country_code, " with ", length(schedule) ," dose PMC")))

#ggsave(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/with_PMC_cases_per_1000.jpeg"), units="in", width=8, height=5, dpi=300)


# CASES (per 1000 children in that age group) (with additional PMC)

# link data with shape file
combined_df_with_additional_PMC_cases <- merge(cases_with_additional_PMC_sixmonth_per1000, adm1, by="NAME_2") 

impact_graph_with_additional_PMC_0_182 <- ggplot(combined_df_with_additional_PMC_cases %>% filter(age_group == "n_age_0_182"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
  theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
  geom_sf(data=adm1)+
  geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
  #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
  ggtitle(paste0("Age: 0-6 months")) + labs(fill=paste0("Clinical cases \nper 1000 children")) +
  scale_fill_viridis_c(limits=c(0,2300)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

if (no_sixmonth_intervals > 1) {
  impact_graph_with_additional_PMC_183_364 <- ggplot(combined_df_with_additional_PMC_cases %>% filter(age_group == "n_age_183_364"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 6-12 months")) + labs(fill=paste0("Clinical cases \nper 1000 children")) +
    scale_fill_viridis_c(limits=c(0,2300)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 2) {
  impact_graph_with_additional_PMC_365_547 <- ggplot(combined_df_with_additional_PMC_cases %>% filter(age_group == "n_age_365_547"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 12-18 months")) + labs(fill=paste0("Clinical cases \nper 1000 children")) +
    scale_fill_viridis_c(limits=c(0,2300)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 3) {
  impact_graph_with_additional_PMC_548_729 <- ggplot(combined_df_with_additional_PMC_cases %>% filter(age_group == "n_age_548_729"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 18-24 months")) + labs(fill=paste0("Clinical cases \nper 1000 children")) +
    scale_fill_viridis_c(limits=c(0,2300)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 4) {
  impact_graph_with_additional_PMC_730_912 <- ggplot(combined_df_with_additional_PMC_cases %>% filter(age_group == "n_age_730_912"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 24-30 months")) + labs(fill=paste0("Clinical cases \nper 1000 children")) +
    scale_fill_viridis_c(limits=c(0,2300)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}


plot_impact <- ggarrange(impact_graph_with_PMC_0_182, impact_graph_with_PMC_183_364 , nrow=1, ncol=2, common.legend = TRUE, legend = "right")
annotate_figure(plot_impact, top = text_grob(paste0("Clinical cases per 1000 children in ", country_code, " with ", length(schedule_additional_doses)," dose PMC")))

#ggsave(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/with_additional_PMC_cases_per_1000.jpeg"), units="in", width=8, height=5, dpi=300)


# CLINICAL INCIDENCE % REDUCTION (with PMC) 

# link data with shapefile
combined_df_with_PMC_reduction <- merge(cases_reduction_with_PMC,adm1, by="NAME_2") 
combined_df_with_additional_PMC_reduction <- merge(cases_reduction_with_additional_PMC,adm1, by="NAME_2") 

impact_graph_with_PMC <- ggplot(combined_df_with_PMC_reduction) + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
  theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
  geom_sf(data=adm1)+
  geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
  #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
  ggtitle(paste0(length(schedule), " DOSE")) + labs(fill="% reduction in\nclinical cases") +
  #scale_fill_viridis_c(limits=c(min(combined_df_with_PMC_reduction$clinical),max(combined_df_with_PMC_reduction$clinical))) + 
  scale_fill_viridis_c(limits=c(5,37)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

impact_graph_with_additional_PMC <- ggplot(combined_df_with_additional_PMC_reduction) + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
  theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
  geom_sf(data=adm1)+
  geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
  #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
  ggtitle(paste0(length(schedule_additional_doses), " DOSE")) + labs(fill="% reduction in\nclinical cases") +
  #scale_fill_viridis_c(limits=c(min(combined_df_with_PMC_reduction$clinical),max(combined_df_with_PMC_reduction$clinical))) + 
  scale_fill_viridis_c(limits=c(5,37)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

plot_impact <- ggarrange(impact_graph_with_PMC,impact_graph_with_additional_PMC, nrow=1, ncol=2, common.legend = TRUE, legend = "right")
annotate_figure(plot_impact, top = text_grob(paste0("Reduction in clinical cases in ", country_code, " for ", length(schedule), " and ", length(schedule_additional_doses), " dose PMC schedules")))

#ggsave(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/program_comparison_reduction.jpeg"), units="in", width=8, height=5, dpi=600)

# CLINICAL INCIDENCE % REDUCTION (with PMC) (6 MONTH AGE GROUP INTERVAL)

# link data with shapefile
combined_df_with_PMC_reduction_sixmonth <- merge(cases_reduction_with_PMC_sixmonth,adm1, by="NAME_2") 

impact_graph_with_PMC_0_182 <- ggplot(combined_df_with_PMC_reduction_sixmonth %>% filter(age_group == "n_age_0_182"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
  theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
  geom_sf(data=adm1)+
  geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
  #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
  ggtitle(paste0("Age: 0-6 months")) + labs(fill="% reduction in\nclinical cases") +
  scale_fill_viridis_c(limits=c(0,80)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

if (no_sixmonth_intervals > 1) {
  impact_graph_with_PMC_183_364 <- ggplot(combined_df_with_PMC_reduction_sixmonth %>% filter(age_group == "n_age_183_364"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 6-12 months")) + labs(fill="% reduction in\nclinical cases") +
    scale_fill_viridis_c(limits=c(0,80)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 2) {
  impact_graph_with_PMC_365_547 <- ggplot(combined_df_with_PMC_reduction_sixmonth %>% filter(age_group == "n_age_365_547"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 12-18 months")) + labs(fill="% reduction in\nclinical cases") +
    scale_fill_viridis_c(limits=c(0,80)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 3) {
  impact_graph_with_PMC_548_729 <- ggplot(combined_df_with_PMC_reduction_sixmonth %>% filter(age_group == "n_age_548_729"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 18-24 months")) + labs(fill="% reduction in\nclinical cases") +
    scale_fill_viridis_c(limits=c(0,80)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 4) {
  impact_graph_with_PMC_730_912 <- ggplot(combined_df_with_PMC_reduction_sixmonth %>% filter(age_group == "n_age_730_912"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 24-30 months")) + labs(fill="% reduction in\nclinical cases") +
    scale_fill_viridis_c(limits=c(0,80)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}


plot_impact <- ggarrange(impact_graph_with_PMC_0_182, impact_graph_with_PMC_183_364,  nrow=1, ncol=2, common.legend = TRUE, legend = "right")
annotate_figure(plot_impact, top = text_grob(paste0("Reduction in clinical cases in ", country_code, " during ", length(schedule), " dose PMC schedule")))

#ggsave(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/with_PMC_reduction_by_age_groups.jpeg"), units="in", width=12, height=4, dpi=2000)

# CLINICAL INCIDENCE % REDUCTION (with additional PMC) (6 MONTH AGE GROUP INTERVAL)

# link data with shapefile
combined_df_with_additional_PMC_reduction_sixmonth <- merge(cases_reduction_with_additional_PMC_sixmonth,adm1, by="NAME_2") 

impact_graph_with_additional_PMC_0_182 <- ggplot(combined_df_with_additional_PMC_reduction_sixmonth %>% filter(age_group == "n_age_0_182"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
  theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
  geom_sf(data=adm1)+
  geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
  #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
  ggtitle(paste0("Age: 0-6 months")) + labs(fill="% reduction in\nclinical cases") +
  scale_fill_viridis_c(limits=c(0,80)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

if (no_sixmonth_intervals > 1) {
  impact_graph_with_additional_PMC_183_364 <- ggplot(combined_df_with_additional_PMC_reduction_sixmonth %>% filter(age_group == "n_age_183_364"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 6-12 months")) + labs(fill="% reduction in\nclinical cases") +
    scale_fill_viridis_c(limits=c(0,80)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 2) {
  impact_graph_with_additional_PMC_365_547 <- ggplot(combined_df_with_additional_PMC_reduction_sixmonth %>% filter(age_group == "n_age_365_547"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 12-18 months")) + labs(fill="% reduction in\nclinical cases") +
    scale_fill_viridis_c(limits=c(0,80)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 3) {
  impact_graph_with_additional_PMC_548_729 <- ggplot(combined_df_with_additional_PMC_reduction_sixmonth %>% filter(age_group == "n_age_548_729"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 18-24 months")) + labs(fill="% reduction in\nclinical cases") +
    scale_fill_viridis_c(limits=c(0,80)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 4) {
  impact_graph_with_additional_PMC_730_912 <- ggplot(combined_df_with_additional_PMC_reduction_sixmonth %>% filter(age_group == "n_age_730_912"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 24-30 months")) + labs(fill="% reduction in\nclinical cases") +
    scale_fill_viridis_c(limits=c(0,80)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}


plot_impact <- ggarrange(impact_graph_with_additional_PMC_0_182, impact_graph_with_additional_PMC_183_364,  nrow=1, ncol=2, common.legend = TRUE, legend = "right")
annotate_figure(plot_impact, top = text_grob(paste0("Reduction in clinical cases in ", country_code, " during ", length(schedule_additional_doses), " dose PMC schedule")))

#ggsave(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/with_additional_PMC_reduction_by_age_groups.jpeg"), units="in", width=12, height=4, dpi=2000)


# ANNUAL CLINICAL CASES AVERTED FOR WHOLE AGE GROUP (with PMC)

##link data with shapefile
combined_df_with_PMC_averted <- merge(annual_cases_averted_with_PMC_per1000,adm1, by="NAME_2") 
combined_df_with_additional_PMC_averted <- merge(annual_cases_averted_with_additional_PMC_per1000,adm1, by="NAME_2") 

impact_graph_with_PMC <- ggplot(combined_df_with_PMC_averted)  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
  theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
  geom_sf(data=adm1)+
  geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
  #geom_sf_label(aes(label = paste0(NAME_1, ": ", round(clinical, 2)), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
  ggtitle(paste0(length(schedule), " DOSE")) + labs(fill="Annual clinical\ncases averted\nper 1000 children") +
  scale_fill_viridis_c(limits=c(0,1150)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

impact_graph_with_additional_PMC <- ggplot(combined_df_with_additional_PMC_averted)  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
  theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
  geom_sf(data=adm1)+
  geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
  #geom_sf_label(aes(label = paste0(NAME_1, ": ", round(clinical, 2)), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
  ggtitle(paste0(length(schedule_additional_doses), " DOSE")) + labs(fill="Annual clinical\ncases averted\nper 1000 children") +
  scale_fill_viridis_c(limits=c(0,1150)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

plot_impact <- ggarrange(impact_graph_with_PMC,impact_graph_with_additional_PMC, nrow=1, ncol=2, common.legend = TRUE, legend = "right")
annotate_figure(plot_impact, top = text_grob(paste0("Clinical cases averted in ", country_code, " during ", length(schedule), " and ", length(schedule_additional_doses), " PMC schedule")))

#ggsave(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_averted_program_comparison.jpeg"), units="in", width=8, height=5, dpi=300)


# ANNUAL CLINICAL CASES AVERTED (6 MONTH AGE GROUP INTERVALS) (with PMC)

# link data with shapefile
combined_df_with_PMC_averted_sixmonth <- merge(cases_averted_with_PMC_sixmonth_per1000,adm1, by="NAME_2") 

impact_graph_with_PMC_0_182 <- ggplot(combined_df_with_PMC_averted_sixmonth %>% filter(age_group == "n_age_0_182"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
  theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
  geom_sf(data=adm1)+
  geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
  #geom_sf_label(aes(label = paste0(NAME_1, ": ", round(clinical, 2)), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
  ggtitle(paste0("Age: 0-6 months")) + labs(fill="Annual clinical\ncases averted\nper 1000 children") +
  scale_fill_viridis_c(limits=c(0,1900)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())


if (no_sixmonth_intervals > 1) {
  impact_graph_with_PMC_183_364 <- ggplot(combined_df_with_PMC_averted_sixmonth %>% filter(age_group == "n_age_183_364"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1, ": ", round(clinical, 2)), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 6-12 months")) + labs(fill="Annual clinical\ncases averted\nper 1000 children") +
    scale_fill_viridis_c(limits=c(0,1900)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 2) {
  impact_graph_with_PMC_365_547 <- ggplot(combined_df_with_PMC_averted_sixmonth %>% filter(age_group == "n_age_365_547"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1, ": ", round(clinical, 2)), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 12-18 months")) + labs(fill="Annual clinical\ncases averted\nper 1000 children") +
    scale_fill_viridis_c(limits=c(0,1900)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 3) {
  impact_graph_with_PMC_548_729 <- ggplot(combined_df_with_PMC_averted_sixmonth %>% filter(age_group == "n_age_548_729"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1, ": ", round(clinical, 2)), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 18-24 months")) + labs(fill="Annual clinical\ncases averted\nper 1000 children") +
    scale_fill_viridis_c(limits=c(0,1900)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 4) {
  impact_graph_with_PMC_730_912 <- ggplot(combined_df_with_PMC_averted_sixmonth %>% filter(age_group == "n_age_730_912"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1, ": ", round(clinical, 2)), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 24-30 months")) + labs(fill="Annual clinical\ncases averted\nper 1000 children") +
    scale_fill_viridis_c(limits=c(0,1900)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

plot_impact <- ggarrange(impact_graph_with_PMC_0_182, impact_graph_with_PMC_183_364 , nrow=1, ncol=2, common.legend = TRUE, legend = "right")
annotate_figure(plot_impact, top = text_grob(paste0("Clinical cases averted in ", country_code, " during ", length(schedule), " dose PMC schedule")))

#ggsave(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/with_PMC_cases_averted_by_age_groups.jpeg"), units="in", width=8, height=5, dpi=300)

# ANNUAL CLINICAL CASES AVERTED (6 MONTH AGE GROUP INTERVALS) (with additional PMC)

# link data with shapefile
combined_df_with_additional_PMC_averted_sixmonth <- merge(cases_averted_with_additional_PMC_sixmonth_per1000,adm1, by="NAME_2") 

impact_graph_with_additional_PMC_0_182 <- ggplot(combined_df_with_additional_PMC_averted_sixmonth %>% filter(age_group == "n_age_0_182"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
  theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
  geom_sf(data=adm1)+
  geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
  #geom_sf_label(aes(label = paste0(NAME_1, ": ", round(clinical, 2)), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
  ggtitle(paste0("Age: 0-6 months")) + labs(fill="Annual clinical\ncases averted\nper 1000 children") +
  scale_fill_viridis_c(limits=c(0,1900)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())


if (no_sixmonth_intervals > 1) {
  impact_graph_with_additional_PMC_183_364 <- ggplot(combined_df_with_additional_PMC_averted_sixmonth %>% filter(age_group == "n_age_183_364"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1, ": ", round(clinical, 2)), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 6-12 months")) + labs(fill="Annual clinical\ncases averted\nper 1000 children") +
    scale_fill_viridis_c(limits=c(0,1900)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 2) {
  impact_graph_with_additional_PMC_365_547 <- ggplot(combined_df_with_additional_PMC_averted_sixmonth %>% filter(age_group == "n_age_365_547"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1, ": ", round(clinical, 2)), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 12-18 months")) + labs(fill="Annual clinical\ncases averted\nper 1000 children") +
    scale_fill_viridis_c(limits=c(0,1900)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 3) {
  impact_graph_with_additional_PMC_548_729 <- ggplot(combined_df_with_additional_PMC_averted_sixmonth %>% filter(age_group == "n_age_548_729"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1, ": ", round(clinical, 2)), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 18-24 months")) + labs(fill="Annual clinical\ncases averted\nper 1000 children") +
    scale_fill_viridis_c(limits=c(0,1900)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

if (no_sixmonth_intervals > 4) {
  impact_graph_with_additional_PMC_730_912 <- ggplot(combined_df_with_additional_PMC_averted_sixmonth %>% filter(age_group == "n_age_730_912"))  + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
    theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
    geom_sf(data=adm1)+
    geom_sf(aes(fill=clinical, geometry=geometry), size=0.5)  +
    #geom_sf_label(aes(label = paste0(NAME_1, ": ", round(clinical, 2)), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
    ggtitle(paste0("Age: 24-30 months")) + labs(fill="Annual clinical\ncases averted\nper 1000 children") +
    scale_fill_viridis_c(limits=c(0,1900)) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

plot_impact <- ggarrange(impact_graph_with_additional_PMC_0_182, impact_graph_with_additional_PMC_183_364 , nrow=1, ncol=2, common.legend = TRUE, legend = "right")
annotate_figure(plot_impact, top = text_grob(paste0("Clinical cases averted in ", country_code, " during ", length(schedule_additional_doses), " dose PMC schedule")))

#ggsave(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/with_additional_PMC_cases_averted_by_age_groups.jpeg"), units="in", width=8, height=5, dpi=300)


##### MERGED RESULTSS #####

rur_or_urb <- "merged"

incidence_ppy_df <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/incidence_ppy_df_", rur_or_urb, ".csv"))
population_df <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/population_df_", rur_or_urb, ".csv"))
PMC_impact_ppy<-read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_", rur_or_urb, ".csv"))
PMC_impact_ppy_additional_doses<-read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_additional_", rur_or_urb, ".csv"))


area_names <- unique(PMC_impact_ppy$area)

##### SET UP CALCULATION DATAFRAMES  #####

# raw baseline cases (no PMC) for every age group
cases_no_PMC<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                          NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                          units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))

# average baseline annual cases (no PMC) for whole age group
annual_cases_no_PMC<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                 NAME_2 = area_names,
                                 units=rep("raw cases", length(area_names)))

# raw cases (with PMC) for every age group
cases_with_PMC<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                            NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                            units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))

# average annual cases (with PMC) for whole age group
annual_cases_with_PMC<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                   NAME_2 = area_names,
                                   units=rep("raw cases", length(area_names)))

# raw cases (with additional PMC) for every age group
cases_with_additional_PMC<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                                       NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                                       units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))

# average annual cases (with additional PMC) for whole age group
annual_cases_with_additional_PMC<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                              NAME_2 = area_names,
                                              units=rep("raw cases", length(area_names)))

# baseline cases (no PMC) for every age group (per 1000 children in that age group)
cases_no_PMC_per1000<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                                  NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                                  units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))

# average baseline annual cases (no PMC) for whole age group (per 1000 children in that age group)
annual_cases_no_PMC_per1000<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                         NAME_2 = area_names,
                                         units=rep("raw cases", length(area_names)))

# cases (with PMC) for every age group (per 1000 children in that age group)
cases_with_PMC_per1000<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                                    NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                                    units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))

# average annual cases (with PMC) for whole age group (per 1000 children in that age group)
annual_cases_with_PMC_per1000<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                           NAME_2 = area_names,
                                           units=rep("raw cases", length(area_names)))

# raw cases averted (with PMC) for every age group
cases_averted_with_PMC<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                                    NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                                    units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))

# average annual cases averted (with PMC) for whole age group
annual_cases_averted_with_PMC<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                           NAME_2 = area_names,
                                           units=rep("raw cases", length(area_names)))


# cases averted (with PMC) for every age group (per 1000 children in that age group)
cases_averted_with_PMC_per1000<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                                            NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                                            units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))

# average annual cases averted (with PMC) for whole age group (per 1000 children in that age group)
annual_cases_averted_with_PMC_per1000<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                                   NAME_2 = area_names,
                                                   units=rep("raw cases", length(area_names)))

# average % reduction in cases (with PMC) for whole age group
cases_reduction_with_PMC <- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                       units=rep("average reduction (%)", length(area_names)), NAME_2 = area_names)


# cases (with PMC) for every age group (per 1000 children in that age group)
cases_with_additional_PMC_per1000<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                                               NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                                               units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))

# average annual cases (with PMC) for whole age group (per 1000 children in that age group)
annual_cases_with_additional_PMC_per1000<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                                      NAME_2 = area_names,
                                                      units=rep("raw cases", length(area_names)))

# raw cases averted (with PMC) for every age group
cases_averted_with_additional_PMC<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                                               NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                                               units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))

# average annual cases averted (with PMC) for whole age group
annual_cases_averted_with_additional_PMC<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                                      NAME_2 = area_names,
                                                      units=rep("raw cases", length(area_names)))


# cases averted (with PMC) for every age group (per 1000 children in that age group)
cases_averted_with_additional_PMC_per1000<- data.frame(age_group = rep(age_group_names,length(area_names)), age_in_days_midpoint = rep(age_in_days_midpoint,length(area_names)),
                                                       NAME_2 = rep(area_names, each=length(age_in_days_midpoint)),
                                                       units=rep("raw cases", length(area_names)*length(age_in_days_midpoint)))

# average annual cases averted (with PMC) for whole age group (per 1000 children in that age group)
annual_cases_averted_with_additional_PMC_per1000<- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                                              NAME_2 = area_names,
                                                              units=rep("raw cases", length(area_names)))

# average % reduction in cases (with PMC) for whole age group
cases_reduction_with_additional_PMC <- data.frame(age_group = paste0("n_age_0_", max(age_max)),
                                                  units=rep("average reduction (%)", length(area_names)), NAME_2 = area_names)


##### INITIALISE VECTORS FOR CALCULATIONS #####

# raw cases
current_cases_no_PMC_clin <- current_cases_no_PMC_sev <- current_cases_no_PMC_tot <- current_cases_no_PMC_asym <- c()
current_cases_averted_with_PMC_clin <- current_cases_averted_with_PMC_sev <- current_cases_averted_with_PMC_tot <- current_cases_averted_with_PMC_asym <- c()
current_cases_with_PMC_clin <- current_cases_with_PMC_sev <- current_cases_with_PMC_tot <- current_cases_with_PMC_asym <- c()
current_cases_averted_with_additional_PMC_clin <- current_cases_averted_with_additional_PMC_sev <- current_cases_averted_with_additional_PMC_tot <- current_cases_averted_with_additional_PMC_asym <- c()
current_cases_with_additional_PMC_clin <- current_cases_with_additional_PMC_sev <- current_cases_with_additional_PMC_tot <- current_cases_with_additional_PMC_asym <- c()

# cases per 1000 children in that age group
current_cases_no_PMC_clin_per1000 <- current_cases_no_PMC_sev_per1000 <- current_cases_no_PMC_tot_per1000 <- current_cases_no_PMC_asym_per1000 <- c()
current_cases_averted_with_PMC_clin_per1000 <- current_cases_averted_with_PMC_sev_per1000 <- current_cases_averted_with_PMC_tot_per1000 <- current_cases_averted_with_PMC_asym_per1000 <- c()
current_cases_with_PMC_clin_per1000 <- current_cases_with_PMC_sev_per1000 <- current_cases_with_PMC_tot_per1000 <- current_cases_with_PMC_asym_per1000 <- c()
current_cases_averted_with_additional_PMC_clin_per1000 <- current_cases_averted_with_additional_PMC_sev_per1000 <- current_cases_averted_with_additional_PMC_tot_per1000 <- current_cases_averted_with_additional_PMC_asym_per1000 <- c()
current_cases_with_additional_PMC_clin_per1000 <- current_cases_with_additional_PMC_sev_per1000 <- current_cases_with_additional_PMC_tot_per1000 <- current_cases_with_additional_PMC_asym_per1000 <- c()

# % reduction with PMC 
current_cases_reduction_with_PMC_clin <- current_cases_reduction_with_PMC_sev <- current_cases_reduction_with_PMC_tot <- current_cases_reduction_with_PMC_asym <- c()
current_cases_reduction_with_additional_PMC_clin <- current_cases_reduction_with_additional_PMC_sev <- current_cases_reduction_with_additional_PMC_tot <- current_cases_reduction_with_additional_PMC_asym <- c()

# annual cases
annual_current_cases_no_PMC_clin <- annual_current_cases_no_PMC_sev <- annual_current_cases_no_PMC_tot <- annual_current_cases_no_PMC_asym <- c()
annual_current_cases_with_PMC_clin <- annual_current_cases_with_PMC_sev <- annual_current_cases_with_PMC_tot <- annual_current_cases_with_PMC_asym <- c()
annual_current_cases_averted_with_PMC_clin <- annual_current_cases_averted_with_PMC_sev <- annual_current_cases_averted_with_PMC_tot <- annual_current_cases_averted_with_PMC_asym <- c()
annual_current_cases_with_additional_PMC_clin <- annual_current_cases_with_additional_PMC_sev <- annual_current_cases_with_additional_PMC_tot <- annual_current_cases_with_additional_PMC_asym <- c()
annual_current_cases_averted_with_additional_PMC_clin <- annual_current_cases_averted_with_additional_PMC_sev <- annual_current_cases_averted_with_additional_PMC_tot <- annual_current_cases_averted_with_additional_PMC_asym <- c()

# annual cases (per 1000 children in that age group)
annual_current_cases_no_PMC_clin_per1000 <- annual_current_cases_no_PMC_sev_per1000 <- annual_current_cases_no_PMC_tot_per1000 <- annual_current_cases_no_PMC_asym_per1000 <- c()
annual_current_cases_with_PMC_clin_per1000 <- annual_current_cases_with_PMC_sev_per1000 <- annual_current_cases_with_PMC_tot_per1000 <- annual_current_cases_with_PMC_asym_per1000 <- c()
annual_current_cases_averted_with_PMC_clin_per1000 <- annual_current_cases_averted_with_PMC_sev_per1000 <- annual_current_cases_averted_with_PMC_tot_per1000 <- annual_current_cases_averted_with_PMC_asym_per1000 <- c()
annual_current_cases_with_additional_PMC_clin_per1000 <- annual_current_cases_with_additional_PMC_sev_per1000 <- annual_current_cases_with_additional_PMC_tot_per1000 <- annual_current_cases_with_additional_PMC_asym_per1000 <- c()
annual_current_cases_averted_with_additional_PMC_clin_per1000 <- annual_current_cases_averted_with_additional_PMC_sev_per1000 <- annual_current_cases_averted_with_additional_PMC_tot_per1000 <- annual_current_cases_averted_with_additional_PMC_asym_per1000 <- c()

##### RUN CALCULATIONS ACROSS WHOLE COUNTRY #####

for (i in 1:length(area_names)){
  
  # extracting average age group proportions from the imperial model age distributions 
  age_distribution <- rep(colMeans((population_df %>% 
                                      filter(area == area_names[i]))[, 6:(dim(population_df)[2])] / human_population), 4)
  
  # sum of rural and urban areas as weighted mean is used 
  
  if (rur_or_urb != "merged") {
    area_population <- sum((full_data$population %>% 
                              filter(name_2 == area_names[i], year==2023, urban_rural==rur_or_urb))$pop)
  } else {
    area_population <- sum((full_data$population %>% 
                              filter(name_2 == area_names[i], year==2023))$pop)
  }
  
  # number of cases in each age group per year (no PMC)
  current_cases_no_PMC <- (incidence_ppy_df %>% filter(area==area_names[i]))$value * age_distribution * area_population
  
  # number of cases in each age group per year (no PMC) by infection class 
  current_cases_no_PMC_clin <- c(current_cases_no_PMC_clin, current_cases_no_PMC[1:max(age_max)])
  current_cases_no_PMC_sev <- c(current_cases_no_PMC_sev, current_cases_no_PMC[(1+max(age_max)):(2*max(age_max))])
  current_cases_no_PMC_tot <- c(current_cases_no_PMC_tot, current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))])
  current_cases_no_PMC_asym <- c(current_cases_no_PMC_asym, current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))])
  
  # annual number of cases in each age group per year (no PMC) by infection class 
  annual_current_cases_no_PMC_clin <- c(annual_current_cases_no_PMC_clin, sum(current_cases_no_PMC[1:max(age_max)]))
  annual_current_cases_no_PMC_sev <- c(annual_current_cases_no_PMC_sev, sum(current_cases_no_PMC[(1+max(age_max)):(2*max(age_max))]))
  annual_current_cases_no_PMC_tot <- c(annual_current_cases_no_PMC_tot, sum(current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))]))
  annual_current_cases_no_PMC_asym <- c(annual_current_cases_no_PMC_asym, sum(current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))]))
  
  # number of cases in each age group per year (no PMC) (per 1000 children in that age group)
  current_cases_no_PMC_per1000 <- (incidence_ppy_df %>% filter(area==area_names[i]))$value * 1000
  
  # number of cases in each age group per year (no PMC) by infection class (per 1000 children in that age group)
  current_cases_no_PMC_clin_per1000 <- c(current_cases_no_PMC_clin_per1000, current_cases_no_PMC_per1000[1:max(age_max)])
  current_cases_no_PMC_sev_per1000 <- c(current_cases_no_PMC_sev_per1000, current_cases_no_PMC_per1000[(1+max(age_max)):(2*max(age_max))])
  current_cases_no_PMC_tot_per1000 <- c(current_cases_no_PMC_tot_per1000, current_cases_no_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))])
  current_cases_no_PMC_asym_per1000 <- c(current_cases_no_PMC_asym_per1000, current_cases_no_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))])
  
  # annual number of cases in each age group per year (no PMC) by infection class (per 1000 children in that age group)
  annual_current_cases_no_PMC_clin_per1000 <- c(annual_current_cases_no_PMC_clin_per1000, mean(current_cases_no_PMC_per1000[1:max(age_max)]))
  annual_current_cases_no_PMC_sev_per1000 <- c(annual_current_cases_no_PMC_sev_per1000, mean(current_cases_no_PMC_per1000[(1+max(age_max)):(2*max(age_max))]))
  annual_current_cases_no_PMC_tot_per1000 <- c(annual_current_cases_no_PMC_tot_per1000, mean(current_cases_no_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))]))
  annual_current_cases_no_PMC_asym_per1000 <- c(annual_current_cases_no_PMC_asym_per1000, mean(current_cases_no_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))]))
  
  # number of cases in each age group per year (with PMC)
  current_cases_with_PMC <- (PMC_impact_ppy %>% filter(area==area_names[i]))$value * age_distribution * area_population
  current_cases_with_additional_PMC <- (PMC_impact_ppy_additional_doses %>% filter(area==area_names[i]))$value * age_distribution * area_population
  
  # number of cases in each age group per year (with PMC) (per 1000 children in that age group)
  current_cases_with_PMC_per1000 <- (PMC_impact_ppy %>% filter(area==area_names[i]))$value * 1000
  current_cases_with_additional_PMC_per1000 <- (PMC_impact_ppy_additional_doses %>% filter(area==area_names[i]))$value * 1000
  
  # number of cases in each age group per year (with PMC) by infection class 
  current_cases_with_PMC_clin <- c(current_cases_with_PMC_clin, current_cases_with_PMC[1:max(age_max)])
  current_cases_with_PMC_sev <- c(current_cases_with_PMC_sev, current_cases_with_PMC[(1+max(age_max)):(2*max(age_max))])
  current_cases_with_PMC_tot <- c(current_cases_with_PMC_tot, current_cases_with_PMC[(1+2*max(age_max)):(3*max(age_max))])
  current_cases_with_PMC_asym <- c(current_cases_with_PMC_asym, current_cases_with_PMC[(1+3*max(age_max)):(4*max(age_max))])
  
  current_cases_with_additional_PMC_clin <- c(current_cases_with_additional_PMC_clin, current_cases_with_additional_PMC[1:max(age_max)])
  current_cases_with_additional_PMC_sev <- c(current_cases_with_additional_PMC_sev, current_cases_with_additional_PMC[(1+max(age_max)):(2*max(age_max))])
  current_cases_with_additional_PMC_tot <- c(current_cases_with_additional_PMC_tot, current_cases_with_additional_PMC[(1+2*max(age_max)):(3*max(age_max))])
  current_cases_with_additional_PMC_asym <- c(current_cases_with_additional_PMC_asym, current_cases_with_additional_PMC[(1+3*max(age_max)):(4*max(age_max))])
  
  # number of cases in each age group per year (with PMC) by infection class (per 1000 children in that age group)
  current_cases_with_PMC_clin_per1000 <- c(current_cases_with_PMC_clin_per1000, current_cases_with_PMC_per1000[1:max(age_max)])
  current_cases_with_PMC_sev_per1000 <- c(current_cases_with_PMC_sev_per1000, current_cases_with_PMC_per1000[(1+max(age_max)):(2*max(age_max))])
  current_cases_with_PMC_tot_per1000 <- c(current_cases_with_PMC_tot_per1000, current_cases_with_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))])
  current_cases_with_PMC_asym_per1000 <- c(current_cases_with_PMC_asym_per1000, current_cases_with_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))])
  
  current_cases_with_additional_PMC_clin_per1000 <- c(current_cases_with_additional_PMC_clin_per1000, current_cases_with_additional_PMC_per1000[1:max(age_max)])
  current_cases_with_additional_PMC_sev_per1000 <- c(current_cases_with_additional_PMC_sev_per1000, current_cases_with_additional_PMC_per1000[(1+max(age_max)):(2*max(age_max))])
  current_cases_with_additional_PMC_tot_per1000 <- c(current_cases_with_additional_PMC_tot_per1000, current_cases_with_additional_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))])
  current_cases_with_additional_PMC_asym_per1000 <- c(current_cases_with_additional_PMC_asym_per1000, current_cases_with_additional_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))])
  
  # annual number of cases in each age group per year (with PMC) by infection class 
  annual_current_cases_with_PMC_clin <- c(annual_current_cases_with_PMC_clin, sum(current_cases_with_PMC[1:max(age_max)]))
  annual_current_cases_with_PMC_sev <- c(annual_current_cases_with_PMC_sev, sum(current_cases_with_PMC[(1+max(age_max)):(2*max(age_max))]))
  annual_current_cases_with_PMC_tot <- c(annual_current_cases_with_PMC_tot, sum(current_cases_with_PMC[(1+2*max(age_max)):(3*max(age_max))]))
  annual_current_cases_with_PMC_asym <- c(annual_current_cases_with_PMC_asym, sum(current_cases_with_PMC[(1+3*max(age_max)):(4*max(age_max))]))
  
  annual_current_cases_with_additional_PMC_clin <- c(annual_current_cases_with_additional_PMC_clin, sum(current_cases_with_additional_PMC[1:max(age_max)]))
  annual_current_cases_with_additional_PMC_sev <- c(annual_current_cases_with_additional_PMC_sev, sum(current_cases_with_additional_PMC[(1+max(age_max)):(2*max(age_max))]))
  annual_current_cases_with_additional_PMC_tot <- c(annual_current_cases_with_additional_PMC_tot, sum(current_cases_with_additional_PMC[(1+2*max(age_max)):(3*max(age_max))]))
  annual_current_cases_with_additional_PMC_asym <- c(annual_current_cases_with_additional_PMC_asym, sum(current_cases_with_additional_PMC[(1+3*max(age_max)):(4*max(age_max))]))
  
  # annual number of cases in each age group per year (with PMC) by infection class (per 1000 children in that age group)
  annual_current_cases_with_PMC_clin_per1000 <- c(annual_current_cases_with_PMC_clin_per1000, mean(current_cases_with_PMC_per1000[1:max(age_max)]))
  annual_current_cases_with_PMC_sev_per1000 <- c(annual_current_cases_with_PMC_sev_per1000, mean(current_cases_with_PMC_per1000[(1+max(age_max)):(2*max(age_max))]))
  annual_current_cases_with_PMC_tot_per1000 <- c(annual_current_cases_with_PMC_tot_per1000, mean(current_cases_with_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))]))
  annual_current_cases_with_PMC_asym_per1000 <- c(annual_current_cases_with_PMC_asym_per1000, mean(current_cases_with_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))]))
  
  annual_current_cases_with_additional_PMC_clin_per1000 <- c(annual_current_cases_with_additional_PMC_clin_per1000, mean(current_cases_with_additional_PMC_per1000[1:max(age_max)]))
  annual_current_cases_with_additional_PMC_sev_per1000 <- c(annual_current_cases_with_additional_PMC_sev_per1000, mean(current_cases_with_additional_PMC_per1000[(1+max(age_max)):(2*max(age_max))]))
  annual_current_cases_with_additional_PMC_tot_per1000 <- c(annual_current_cases_with_additional_PMC_tot_per1000, mean(current_cases_with_additional_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))]))
  annual_current_cases_with_additional_PMC_asym_per1000 <- c(annual_current_cases_with_additional_PMC_asym_per1000, mean(current_cases_with_additional_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))]))
  
  # average reduction in cases (with PMC) by infection class 
  current_cases_reduction_with_PMC_clin <- c(current_cases_reduction_with_PMC_clin, (mean(current_cases_no_PMC[1:max(age_max)]) - mean(current_cases_with_PMC[1:max(age_max)])) / mean(current_cases_no_PMC[1:max(age_max)]) * 100)
  current_cases_reduction_with_PMC_sev <- c(current_cases_reduction_with_PMC_sev, (mean(current_cases_no_PMC[(1+max(age_max)):(2*max(age_max))]) - mean(current_cases_with_PMC[(1+max(age_max)):(2*max(age_max))])) / mean(current_cases_no_PMC[(1+max(age_max)):(2*max(age_max))]) * 100)
  current_cases_reduction_with_PMC_tot <- c(current_cases_reduction_with_PMC_tot, (mean(current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))]) - mean(current_cases_with_PMC[(1+2*max(age_max)):(3*max(age_max))])) / mean(current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))]) * 100)
  current_cases_reduction_with_PMC_asym <- c(current_cases_reduction_with_PMC_asym, (mean(current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))]) - mean(current_cases_with_PMC[(1+3*max(age_max)):(4*max(age_max))])) / mean(current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))]) * 100)
  
  current_cases_reduction_with_additional_PMC_clin <- c(current_cases_reduction_with_additional_PMC_clin, (mean(current_cases_no_PMC[1:max(age_max)]) - mean(current_cases_with_additional_PMC[1:max(age_max)])) / mean(current_cases_no_PMC[1:max(age_max)]) * 100)
  current_cases_reduction_with_additional_PMC_sev <- c(current_cases_reduction_with_additional_PMC_sev, (mean(current_cases_no_PMC[(1+max(age_max)):(2*max(age_max))]) - mean(current_cases_with_additional_PMC[(1+max(age_max)):(2*max(age_max))])) / mean(current_cases_no_PMC[(1+max(age_max)):(2*max(age_max))]) * 100)
  current_cases_reduction_with_additional_PMC_tot <- c(current_cases_reduction_with_additional_PMC_tot, (mean(current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))]) - mean(current_cases_with_additional_PMC[(1+2*max(age_max)):(3*max(age_max))])) / mean(current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))]) * 100)
  current_cases_reduction_with_additional_PMC_asym <- c(current_cases_reduction_with_additional_PMC_asym, (mean(current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))]) - mean(current_cases_with_additional_PMC[(1+3*max(age_max)):(4*max(age_max))])) / mean(current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))]) * 100)
  
}

# raw cases averted (with PMC) by infection class 
cases_averted_with_PMC_clin <- (current_cases_no_PMC_clin - current_cases_with_PMC_clin)
cases_averted_with_PMC_sev <- (current_cases_no_PMC_sev - current_cases_with_PMC_sev)
cases_averted_with_PMC_tot <- (current_cases_no_PMC_tot - current_cases_with_PMC_tot)
cases_averted_with_PMC_asym <- (current_cases_no_PMC_asym - current_cases_with_PMC_asym)

cases_averted_with_additional_PMC_clin <- (current_cases_no_PMC_clin - current_cases_with_additional_PMC_clin)
cases_averted_with_additional_PMC_sev <- (current_cases_no_PMC_sev - current_cases_with_additional_PMC_sev)
cases_averted_with_additional_PMC_tot <- (current_cases_no_PMC_tot - current_cases_with_additional_PMC_tot)
cases_averted_with_additional_PMC_asym <- (current_cases_no_PMC_asym - current_cases_with_additional_PMC_asym)

# raw cases averted (with PMC) by infection class (per 1000 children in that age group)
cases_averted_with_PMC_clin_per1000 <- (current_cases_no_PMC_clin_per1000 - current_cases_with_PMC_clin_per1000)
cases_averted_with_PMC_sev_per1000 <- (current_cases_no_PMC_sev_per1000 - current_cases_with_PMC_sev_per1000)
cases_averted_with_PMC_tot_per1000 <- (current_cases_no_PMC_tot_per1000 - current_cases_with_PMC_tot_per1000)
cases_averted_with_PMC_asym_per1000 <- (current_cases_no_PMC_asym_per1000 - current_cases_with_PMC_asym_per1000)

cases_averted_with_additional_PMC_clin_per1000 <- (current_cases_no_PMC_clin_per1000 - current_cases_with_additional_PMC_clin_per1000)
cases_averted_with_additional_PMC_sev_per1000 <- (current_cases_no_PMC_sev_per1000 - current_cases_with_additional_PMC_sev_per1000)
cases_averted_with_additional_PMC_tot_per1000 <- (current_cases_no_PMC_tot_per1000 - current_cases_with_additional_PMC_tot_per1000)
cases_averted_with_additional_PMC_asym_per1000 <- (current_cases_no_PMC_asym_per1000 - current_cases_with_additional_PMC_asym_per1000)

# annual cases averted (with PMC) by infection class 
annual_cases_averted_with_PMC_clin <- (annual_current_cases_no_PMC_clin - annual_current_cases_with_PMC_clin)
annual_cases_averted_with_PMC_sev <- (annual_current_cases_no_PMC_sev - annual_current_cases_with_PMC_sev)
annual_cases_averted_with_PMC_tot <- (annual_current_cases_no_PMC_tot - annual_current_cases_with_PMC_tot)
annual_cases_averted_with_PMC_asym <- (annual_current_cases_no_PMC_asym - annual_current_cases_with_PMC_asym)

annual_cases_averted_with_additional_PMC_clin <- (annual_current_cases_no_PMC_clin - annual_current_cases_with_additional_PMC_clin)
annual_cases_averted_with_additional_PMC_sev <- (annual_current_cases_no_PMC_sev - annual_current_cases_with_additional_PMC_sev)
annual_cases_averted_with_additional_PMC_tot <- (annual_current_cases_no_PMC_tot - annual_current_cases_with_additional_PMC_tot)
annual_cases_averted_with_additional_PMC_asym <- (annual_current_cases_no_PMC_asym - annual_current_cases_with_additional_PMC_asym)

# annual cases averted (with PMC) by infection class (per 1000 children in that age group)
annual_cases_averted_with_PMC_clin_per1000 <- (annual_current_cases_no_PMC_clin_per1000 - annual_current_cases_with_PMC_clin_per1000)
annual_cases_averted_with_PMC_sev_per1000 <- (annual_current_cases_no_PMC_sev_per1000 - annual_current_cases_with_PMC_sev_per1000)
annual_cases_averted_with_PMC_tot_per1000 <- (annual_current_cases_no_PMC_tot_per1000 - annual_current_cases_with_PMC_tot_per1000)
annual_cases_averted_with_PMC_asym_per1000 <- (annual_current_cases_no_PMC_asym_per1000 - annual_current_cases_with_PMC_asym_per1000)

annual_cases_averted_with_additional_PMC_clin_per1000 <- (annual_current_cases_no_PMC_clin_per1000 - annual_current_cases_with_additional_PMC_clin_per1000)
annual_cases_averted_with_additional_PMC_sev_per1000 <- (annual_current_cases_no_PMC_sev_per1000 - annual_current_cases_with_additional_PMC_sev_per1000)
annual_cases_averted_with_additional_PMC_tot_per1000 <- (annual_current_cases_no_PMC_tot_per1000 - annual_current_cases_with_additional_PMC_tot_per1000)
annual_cases_averted_with_additional_PMC_asym_per1000 <- (annual_current_cases_no_PMC_asym_per1000 - annual_current_cases_with_additional_PMC_asym_per1000)

# average reduction in cases by infection class 
cases_reduction_with_PMC_clin <- (current_cases_no_PMC_clin - current_cases_with_PMC_clin)/current_cases_no_PMC_clin * 100
cases_reduction_with_PMC_sev <- (current_cases_no_PMC_sev - current_cases_with_PMC_sev)/current_cases_no_PMC_sev * 100
cases_reduction_with_PMC_tot <- (current_cases_no_PMC_tot - current_cases_with_PMC_tot)/current_cases_no_PMC_tot * 100
cases_reduction_with_PMC_asym <- (current_cases_no_PMC_asym - current_cases_with_PMC_asym)/current_cases_no_PMC_asym * 100

cases_reduction_with_additional_PMC_clin <- (current_cases_no_PMC_clin - current_cases_with_additional_PMC_clin)/current_cases_no_PMC_clin * 100
cases_reduction_with_additional_PMC_sev <- (current_cases_no_PMC_sev - current_cases_with_additional_PMC_sev)/current_cases_no_PMC_sev * 100
cases_reduction_with_additional_PMC_tot <- (current_cases_no_PMC_tot - current_cases_with_additional_PMC_tot)/current_cases_no_PMC_tot * 100
cases_reduction_with_additional_PMC_asym <- (current_cases_no_PMC_asym - current_cases_with_additional_PMC_asym)/current_cases_no_PMC_asym * 100

##### APPEND VECTORS TO CALCULATION DATAFRAMES #####

cases_no_PMC$clinical <- current_cases_no_PMC_clin
cases_no_PMC$severe <- current_cases_no_PMC_sev
cases_no_PMC$total <- current_cases_no_PMC_tot
cases_no_PMC$asymptomatic <- current_cases_no_PMC_asym

cases_no_PMC_per1000$clinical <- current_cases_no_PMC_clin_per1000
cases_no_PMC_per1000$severe <- current_cases_no_PMC_sev_per1000
cases_no_PMC_per1000$total <- current_cases_no_PMC_tot_per1000
cases_no_PMC_per1000$asymptomatic <- current_cases_no_PMC_asym_per1000

annual_cases_no_PMC$clinical <- annual_current_cases_no_PMC_clin
annual_cases_no_PMC$severe <- annual_current_cases_no_PMC_sev
annual_cases_no_PMC$total <- annual_current_cases_no_PMC_tot
annual_cases_no_PMC$asymptomatic <- annual_current_cases_no_PMC_asym

annual_cases_no_PMC_per1000$clinical <- annual_current_cases_no_PMC_clin_per1000
annual_cases_no_PMC_per1000$severe <- annual_current_cases_no_PMC_sev_per1000
annual_cases_no_PMC_per1000$total <- annual_current_cases_no_PMC_tot_per1000
annual_cases_no_PMC_per1000$asymptomatic <- annual_current_cases_no_PMC_asym_per1000

cases_averted_with_PMC$clinical <- cases_averted_with_PMC_clin
cases_averted_with_PMC$severe <- cases_averted_with_PMC_sev
cases_averted_with_PMC$total <- cases_averted_with_PMC_tot
cases_averted_with_PMC$asymptomatic <- cases_averted_with_PMC_asym

cases_averted_with_additional_PMC$clinical <- cases_averted_with_additional_PMC_clin
cases_averted_with_additional_PMC$severe <- cases_averted_with_additional_PMC_sev
cases_averted_with_additional_PMC$total <- cases_averted_with_additional_PMC_tot
cases_averted_with_additional_PMC$asymptomatic <- cases_averted_with_additional_PMC_asym

cases_averted_with_PMC_per1000$clinical <- cases_averted_with_PMC_clin_per1000
cases_averted_with_PMC_per1000$severe <- cases_averted_with_PMC_sev_per1000
cases_averted_with_PMC_per1000$total <- cases_averted_with_PMC_tot_per1000
cases_averted_with_PMC_per1000$asymptomatic <- cases_averted_with_PMC_asym_per1000

cases_averted_with_additional_PMC_per1000$clinical <- cases_averted_with_additional_PMC_clin_per1000
cases_averted_with_additional_PMC_per1000$severe <- cases_averted_with_additional_PMC_sev_per1000
cases_averted_with_additional_PMC_per1000$total <- cases_averted_with_additional_PMC_tot_per1000
cases_averted_with_additional_PMC_per1000$asymptomatic <- cases_averted_with_additional_PMC_asym_per1000

annual_cases_averted_with_PMC$clinical <- annual_cases_averted_with_PMC_clin
annual_cases_averted_with_PMC$severe <- annual_cases_averted_with_PMC_sev
annual_cases_averted_with_PMC$total <- annual_cases_averted_with_PMC_tot
annual_cases_averted_with_PMC$asymptomatic <- annual_cases_averted_with_PMC_asym

annual_cases_averted_with_additional_PMC$clinical <- annual_cases_averted_with_additional_PMC_clin
annual_cases_averted_with_additional_PMC$severe <- annual_cases_averted_with_additional_PMC_sev
annual_cases_averted_with_additional_PMC$total <- annual_cases_averted_with_additional_PMC_tot
annual_cases_averted_with_additional_PMC$asymptomatic <- annual_cases_averted_with_additional_PMC_asym

annual_cases_averted_with_PMC_per1000$clinical <- annual_cases_averted_with_PMC_clin_per1000
annual_cases_averted_with_PMC_per1000$severe <- annual_cases_averted_with_PMC_sev_per1000
annual_cases_averted_with_PMC_per1000$total <- annual_cases_averted_with_PMC_tot_per1000
annual_cases_averted_with_PMC_per1000$asymptomatic <- annual_cases_averted_with_PMC_asym_per1000

annual_cases_averted_with_additional_PMC_per1000$clinical <- annual_cases_averted_with_additional_PMC_clin_per1000
annual_cases_averted_with_additional_PMC_per1000$severe <- annual_cases_averted_with_additional_PMC_sev_per1000
annual_cases_averted_with_additional_PMC_per1000$total <- annual_cases_averted_with_additional_PMC_tot_per1000
annual_cases_averted_with_additional_PMC_per1000$asymptomatic <- annual_cases_averted_with_additional_PMC_asym_per1000

cases_with_PMC$clinical <- current_cases_with_PMC_clin
cases_with_PMC$severe <- current_cases_with_PMC_sev
cases_with_PMC$total <- current_cases_with_PMC_tot
cases_with_PMC$asymptomatic <- current_cases_with_PMC_asym

cases_with_additional_PMC$clinical <- current_cases_with_additional_PMC_clin
cases_with_additional_PMC$severe <- current_cases_with_additional_PMC_sev
cases_with_additional_PMC$total <- current_cases_with_additional_PMC_tot
cases_with_additional_PMC$asymptomatic <- current_cases_with_additional_PMC_asym

cases_with_PMC_per1000$clinical <- current_cases_with_PMC_clin_per1000
cases_with_PMC_per1000$severe <- current_cases_with_PMC_sev_per1000
cases_with_PMC_per1000$total <- current_cases_with_PMC_tot_per1000
cases_with_PMC_per1000$asymptomatic <- current_cases_with_PMC_asym_per1000

cases_with_additional_PMC_per1000$clinical <- current_cases_with_additional_PMC_clin_per1000
cases_with_additional_PMC_per1000$severe <- current_cases_with_additional_PMC_sev_per1000
cases_with_additional_PMC_per1000$total <- current_cases_with_additional_PMC_tot_per1000
cases_with_additional_PMC_per1000$asymptomatic <- current_cases_with_additional_PMC_asym_per1000

annual_cases_with_PMC$clinical <- annual_current_cases_with_PMC_clin
annual_cases_with_PMC$severe <- annual_current_cases_with_PMC_sev
annual_cases_with_PMC$total <- annual_current_cases_with_PMC_tot
annual_cases_with_PMC$asymptomatic <- annual_current_cases_with_PMC_asym

annual_cases_with_additional_PMC$clinical <- annual_current_cases_with_additional_PMC_clin
annual_cases_with_additional_PMC$severe <- annual_current_cases_with_additional_PMC_sev
annual_cases_with_additional_PMC$total <- annual_current_cases_with_additional_PMC_tot
annual_cases_with_additional_PMC$asymptomatic <- annual_current_cases_with_additional_PMC_asym

annual_cases_with_PMC_per1000$clinical <- annual_current_cases_with_PMC_clin_per1000
annual_cases_with_PMC_per1000$severe <- annual_current_cases_with_PMC_sev_per1000
annual_cases_with_PMC_per1000$total <- annual_current_cases_with_PMC_tot_per1000
annual_cases_with_PMC_per1000$asymptomatic <- annual_current_cases_with_PMC_asym_per1000

annual_cases_with_additional_PMC_per1000$clinical <- annual_current_cases_with_additional_PMC_clin_per1000
annual_cases_with_additional_PMC_per1000$severe <- annual_current_cases_with_additional_PMC_sev_per1000
annual_cases_with_additional_PMC_per1000$total <- annual_current_cases_with_additional_PMC_tot_per1000
annual_cases_with_additional_PMC_per1000$asymptomatic <- annual_current_cases_with_additional_PMC_asym_per1000

cases_reduction_with_PMC$clinical <- current_cases_reduction_with_PMC_clin
cases_reduction_with_PMC$severe <- current_cases_reduction_with_PMC_sev
cases_reduction_with_PMC$total <- current_cases_reduction_with_PMC_tot
cases_reduction_with_PMC$asymptomatic <- current_cases_reduction_with_PMC_asym

cases_reduction_with_additional_PMC$clinical <- current_cases_reduction_with_additional_PMC_clin
cases_reduction_with_additional_PMC$severe <- current_cases_reduction_with_additional_PMC_sev
cases_reduction_with_additional_PMC$total <- current_cases_reduction_with_additional_PMC_tot
cases_reduction_with_additional_PMC$asymptomatic <- current_cases_reduction_with_additional_PMC_asym


##### SET UP CALCULATION DATAFRAMES (6 MONTH AGE GROUP INTERVALS) #####

# raw baseline cases (no PMC)
cases_no_PMC_sixmonth<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                   age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                   NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                   units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))

# baseline cases (no PMC) for every age group (per 1000 children in that age group)
cases_no_PMC_sixmonth_per1000<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                           age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                           NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                           units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))

# cases (with PMC) 
cases_with_PMC_sixmonth<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                     age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                     NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                     units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))

# cases (with PMC) (per 1000 children in that age group)
cases_with_PMC_sixmonth_per1000<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                             age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                             NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                             units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))

# raw cases averted (with PMC) 
cases_averted_with_PMC_sixmonth<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                             age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                             NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                             units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))

# raw cases averted (with PMC) (per 1000 children in that age group)
cases_averted_with_PMC_sixmonth_per1000<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                                     age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                                     NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                                     units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))

# average % reduction in cases (with PMC)
cases_reduction_with_PMC_sixmonth <- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                                age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                                NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                                units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))

# cases (with PMC) 
cases_with_additional_PMC_sixmonth<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                                age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                                NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                                units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))

# cases (with PMC) (per 1000 children in that age group)
cases_with_additional_PMC_sixmonth_per1000<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                                        age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                                        NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                                        units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))

# raw cases averted (with PMC) 
cases_averted_with_additional_PMC_sixmonth<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                                        age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                                        NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                                        units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))

# raw cases averted (with PMC) (per 1000 children in that age group)
cases_averted_with_additional_PMC_sixmonth_per1000<- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                                                age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                                                NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                                                units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))

# average % reduction in cases (with PMC)
cases_reduction_with_additional_PMC_sixmonth <- data.frame(age_group = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                                           age_in_days_midpoint = rep(sixmonth_intervals_midpoint[1:no_sixmonth_intervals],times=length(area_names)),
                                                           NAME_2 = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                                           units=rep("raw cases", length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])))

##### INITIALISE VECTORS FOR CALCULATIONS (6 MONTH AGE GROUP INTERVALS) #####

# raw cases
cases_no_PMC_clin <- cases_no_PMC_sev <- cases_no_PMC_tot <- cases_no_PMC_asym <- c()
cases_with_PMC_clin <- cases_with_PMC_sev <- cases_with_PMC_tot <- cases_with_PMC_asym <- c()
cases_with_additional_PMC_clin <- cases_with_additional_PMC_sev <- cases_with_additional_PMC_tot <- cases_with_additional_PMC_asym <- c()

# cases per 1000 children in that age group
cases_no_PMC_clin_per1000 <- cases_no_PMC_sev_per1000 <- cases_no_PMC_tot_per1000 <- cases_no_PMC_asym_per1000 <- c()
cases_with_PMC_clin_per1000 <- cases_with_PMC_sev_per1000 <- cases_with_PMC_tot_per1000 <- cases_with_PMC_asym_per1000 <- c()
cases_with_additional_PMC_clin_per1000 <- cases_with_additional_PMC_sev_per1000 <- cases_with_additional_PMC_tot_per1000 <- cases_with_additional_PMC_asym_per1000 <- c()

# cases averted (with PMC) 
cases_averted_with_PMC_clin <- cases_averted_with_PMC_sev <- cases_averted_with_PMC_tot <- cases_averted_with_PMC_asym <- c()
cases_averted_with_additional_PMC_clin <- cases_averted_with_additional_PMC_sev <- cases_averted_with_additional_PMC_tot <- cases_averted_with_additional_PMC_asym <- c()

# cases averted (per 1000 children in that age group)
cases_averted_with_PMC_clin_per1000 <- cases_averted_with_PMC_sev_per1000 <- cases_averted_with_PMC_tot_per1000 <- cases_averted_with_PMC_asym_per1000 <- c()
cases_averted_with_additional_PMC_clin_per1000 <- cases_averted_with_additional_PMC_sev_per1000 <- cases_averted_with_additional_PMC_tot_per1000 <- cases_averted_with_additional_PMC_asym_per1000 <- c()

# cases reduction 
cases_reduction_with_PMC_clin <- cases_reduction_with_PMC_sev <- cases_reduction_with_PMC_tot <- cases_reduction_with_PMC_asym <- c()
cases_reduction_with_additional_PMC_clin <- cases_reduction_with_additional_PMC_sev <- cases_reduction_with_additional_PMC_tot <- cases_reduction_with_additional_PMC_asym <- c()

##### RUN CALCULATIONS ACROSS WHOLE COUNTRY (6 MONTH AGE GROUP INTERVALS) #####

for (i in 1:length(area_names)) {
  
  # raw cases (no PMC) by infection class
  current_cases_no_PMC_clin <- (cases_no_PMC %>% filter(NAME_2 == area_names[i]))$clinical
  current_cases_no_PMC_sev <- (cases_no_PMC %>% filter(NAME_2 == area_names[i]))$severe
  current_cases_no_PMC_tot <- (cases_no_PMC %>% filter(NAME_2 == area_names[i]))$total
  current_cases_no_PMC_asym <- (cases_no_PMC %>% filter(NAME_2 == area_names[i]))$asymptomatic
  
  # raw cases (with PMC) by infection class
  current_cases_with_PMC_clin <- (cases_with_PMC %>% filter(NAME_2 == area_names[i]))$clinical
  current_cases_with_PMC_sev <- (cases_with_PMC %>% filter(NAME_2 == area_names[i]))$severe
  current_cases_with_PMC_tot <- (cases_with_PMC %>% filter(NAME_2 == area_names[i]))$total
  current_cases_with_PMC_asym <- (cases_with_PMC %>% filter(NAME_2 == area_names[i]))$asymptomatic
  
  current_cases_with_additional_PMC_clin <- (cases_with_additional_PMC %>% filter(NAME_2 == area_names[i]))$clinical
  current_cases_with_additional_PMC_sev <- (cases_with_additional_PMC %>% filter(NAME_2 == area_names[i]))$severe
  current_cases_with_additional_PMC_tot <- (cases_with_additional_PMC %>% filter(NAME_2 == area_names[i]))$total
  current_cases_with_additional_PMC_asym <- (cases_with_additional_PMC %>% filter(NAME_2 == area_names[i]))$asymptomatic
  
  # cases (no PMC) by infection class (per 1000 children in that age group)
  current_cases_no_PMC_clin_per1000 <- (incidence_ppy_df %>% filter(area == area_names[i], infection_class == "clinical"))$value * 1000
  current_cases_no_PMC_sev_per1000 <- (incidence_ppy_df %>% filter(area == area_names[i], infection_class == "severe"))$value * 1000
  current_cases_no_PMC_tot_per1000 <- (incidence_ppy_df %>% filter(area == area_names[i], infection_class == "total"))$value * 1000
  current_cases_no_PMC_asym_per1000 <- (incidence_ppy_df %>% filter(area == area_names[i], infection_class == "asymptomatic"))$value * 1000
  
  # cases (with PMC) by infection class (per 1000 children in that age group)
  current_cases_with_PMC_clin_per1000 <- (PMC_impact_ppy %>% filter(area == area_names[i], infection_class == "clinical"))$value * 1000
  current_cases_with_PMC_sev_per1000 <- (PMC_impact_ppy %>% filter(area == area_names[i], infection_class == "severe"))$value * 1000
  current_cases_with_PMC_tot_per1000 <- (PMC_impact_ppy %>% filter(area == area_names[i], infection_class == "total"))$value * 1000
  current_cases_with_PMC_asym_per1000 <- (PMC_impact_ppy %>% filter(area == area_names[i], infection_class == "asymptomatic"))$value * 1000
  
  current_cases_with_additional_PMC_clin_per1000 <- (PMC_impact_ppy_additional_doses %>% filter(area == area_names[i], infection_class == "clinical"))$value * 1000
  current_cases_with_additional_PMC_sev_per1000 <- (PMC_impact_ppy_additional_doses %>% filter(area == area_names[i], infection_class == "severe"))$value * 1000
  current_cases_with_additional_PMC_tot_per1000 <- (PMC_impact_ppy_additional_doses %>% filter(area == area_names[i], infection_class == "total"))$value * 1000
  current_cases_with_additional_PMC_asym_per1000 <- (PMC_impact_ppy_additional_doses %>% filter(area == area_names[i], infection_class == "asymptomatic"))$value * 1000
  
  
  # split data into each 6 month age group for each infection class (no PMC)
  split_current_cases_no_PMC_clin <- split(current_cases_no_PMC_clin, ceiling(seq_along(current_cases_no_PMC_clin)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_no_PMC_clin) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_no_PMC_sev <- split(current_cases_no_PMC_sev, ceiling(seq_along(current_cases_no_PMC_sev)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_no_PMC_sev) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_no_PMC_tot <- split(current_cases_no_PMC_tot, ceiling(seq_along(current_cases_no_PMC_tot)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_no_PMC_tot) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_no_PMC_asym <- split(current_cases_no_PMC_asym, ceiling(seq_along(current_cases_no_PMC_asym)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_no_PMC_asym) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  
  # split data into each 6 month age group for each infection class (with PMC)
  split_current_cases_with_PMC_clin <- split(current_cases_with_PMC_clin, ceiling(seq_along(current_cases_with_PMC_clin)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_with_PMC_clin) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_with_PMC_sev <- split(current_cases_with_PMC_sev, ceiling(seq_along(current_cases_with_PMC_sev)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_with_PMC_sev) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_with_PMC_tot <- split(current_cases_with_PMC_tot, ceiling(seq_along(current_cases_with_PMC_tot)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_with_PMC_tot) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_with_PMC_asym <- split(current_cases_with_PMC_asym, ceiling(seq_along(current_cases_with_PMC_asym)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_with_PMC_asym) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  
  split_current_cases_with_additional_PMC_clin <- split(current_cases_with_additional_PMC_clin, ceiling(seq_along(current_cases_with_additional_PMC_clin)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_with_additional_PMC_clin) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_with_additional_PMC_sev <- split(current_cases_with_additional_PMC_sev, ceiling(seq_along(current_cases_with_additional_PMC_sev)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_with_additional_PMC_sev) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_with_additional_PMC_tot <- split(current_cases_with_additional_PMC_tot, ceiling(seq_along(current_cases_with_additional_PMC_tot)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_with_additional_PMC_tot) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_with_additional_PMC_asym <- split(current_cases_with_additional_PMC_asym, ceiling(seq_along(current_cases_with_additional_PMC_asym)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_with_additional_PMC_asym) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  # split data into each 6 month age group for each infection class (no PMC) (per 1000 children in that age group)
  split_current_cases_no_PMC_clin_per1000 <- split(current_cases_no_PMC_clin_per1000, ceiling(seq_along(current_cases_no_PMC_clin_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_no_PMC_clin_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_no_PMC_sev_per1000 <- split(current_cases_no_PMC_sev_per1000, ceiling(seq_along(current_cases_no_PMC_sev_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_no_PMC_sev_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_no_PMC_tot_per1000 <- split(current_cases_no_PMC_tot_per1000, ceiling(seq_along(current_cases_no_PMC_tot_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_no_PMC_tot_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_no_PMC_asym_per1000 <- split(current_cases_no_PMC_asym_per1000, ceiling(seq_along(current_cases_no_PMC_asym_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_no_PMC_asym_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  
  # split data into each 6 month age group for each infection class (with PMC) (per 1000 children in that age group)
  split_current_cases_with_PMC_clin_per1000 <- split(current_cases_with_PMC_clin_per1000, ceiling(seq_along(current_cases_with_PMC_clin_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_with_PMC_clin_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_with_PMC_sev_per1000 <- split(current_cases_with_PMC_sev_per1000, ceiling(seq_along(current_cases_with_PMC_sev_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_with_PMC_sev_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_with_PMC_tot_per1000 <- split(current_cases_with_PMC_tot_per1000, ceiling(seq_along(current_cases_with_PMC_tot_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_with_PMC_tot_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_with_PMC_asym_per1000 <- split(current_cases_with_PMC_asym_per1000, ceiling(seq_along(current_cases_with_PMC_asym_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_with_PMC_asym_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  
  split_current_cases_with_additional_PMC_clin_per1000 <- split(current_cases_with_additional_PMC_clin_per1000, ceiling(seq_along(current_cases_with_additional_PMC_clin_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_with_additional_PMC_clin_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_with_additional_PMC_sev_per1000 <- split(current_cases_with_additional_PMC_sev_per1000, ceiling(seq_along(current_cases_with_additional_PMC_sev_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_with_additional_PMC_sev_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_with_additional_PMC_tot_per1000 <- split(current_cases_with_additional_PMC_tot_per1000, ceiling(seq_along(current_cases_with_additional_PMC_tot_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_with_additional_PMC_tot_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  split_current_cases_with_additional_PMC_asym_per1000 <- split(current_cases_with_additional_PMC_asym_per1000, ceiling(seq_along(current_cases_with_additional_PMC_asym_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
  names(split_current_cases_with_additional_PMC_asym_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
  
  
  for (j in 1:length(sixmonth_intervals[1:no_sixmonth_intervals])) {
    
    # sum of cases (no PMC) for each 6 month age group
    cases_no_PMC_clin<- c(cases_no_PMC_clin, sum(split_current_cases_no_PMC_clin[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_no_PMC_sev<- c(cases_no_PMC_sev, sum(split_current_cases_no_PMC_sev[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_no_PMC_tot<- c(cases_no_PMC_tot, sum(split_current_cases_no_PMC_tot[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_no_PMC_asym<- c(cases_no_PMC_asym, sum(split_current_cases_no_PMC_asym[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    
    # sum of cases (with PMC) for each 6 month age group
    cases_with_PMC_clin<- c(cases_with_PMC_clin, sum(split_current_cases_with_PMC_clin[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_with_PMC_sev<- c(cases_with_PMC_sev, sum(split_current_cases_with_PMC_sev[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_with_PMC_tot<- c(cases_with_PMC_tot, sum(split_current_cases_with_PMC_tot[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_with_PMC_asym<- c(cases_with_PMC_asym, sum(split_current_cases_with_PMC_asym[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    
    cases_with_additional_PMC_clin<- c(cases_with_additional_PMC_clin, sum(split_current_cases_with_additional_PMC_clin[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_with_additional_PMC_sev<- c(cases_with_additional_PMC_sev, sum(split_current_cases_with_additional_PMC_sev[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_with_additional_PMC_tot<- c(cases_with_additional_PMC_tot, sum(split_current_cases_with_additional_PMC_tot[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_with_additional_PMC_asym<- c(cases_with_additional_PMC_asym, sum(split_current_cases_with_additional_PMC_asym[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    
    # sum of cases (no PMC) for each 6 month age group (per 1000 children in that age group)
    cases_no_PMC_clin_per1000<- c(cases_no_PMC_clin_per1000, mean(split_current_cases_no_PMC_clin_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_no_PMC_sev_per1000<- c(cases_no_PMC_sev_per1000, mean(split_current_cases_no_PMC_sev_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_no_PMC_tot_per1000<- c(cases_no_PMC_tot_per1000, mean(split_current_cases_no_PMC_tot_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_no_PMC_asym_per1000<- c(cases_no_PMC_asym_per1000, mean(split_current_cases_no_PMC_asym_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    
    # sum of cases (with PMC) for each 6 month age group (per 1000 children in that age group)
    cases_with_PMC_clin_per1000<- c(cases_with_PMC_clin_per1000, mean(split_current_cases_with_PMC_clin_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_with_PMC_sev_per1000<- c(cases_with_PMC_sev_per1000, mean(split_current_cases_with_PMC_sev_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_with_PMC_tot_per1000<- c(cases_with_PMC_tot_per1000, mean(split_current_cases_with_PMC_tot_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_with_PMC_asym_per1000<- c(cases_with_PMC_asym_per1000, mean(split_current_cases_with_PMC_asym_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    
    cases_with_additional_PMC_clin_per1000<- c(cases_with_additional_PMC_clin_per1000, mean(split_current_cases_with_additional_PMC_clin_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_with_additional_PMC_sev_per1000<- c(cases_with_additional_PMC_sev_per1000, mean(split_current_cases_with_additional_PMC_sev_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_with_additional_PMC_tot_per1000<- c(cases_with_additional_PMC_tot_per1000, mean(split_current_cases_with_additional_PMC_tot_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    cases_with_additional_PMC_asym_per1000<- c(cases_with_additional_PMC_asym_per1000, mean(split_current_cases_with_additional_PMC_asym_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
    
  }
  
}  


# cases averted (with PMC) by infection class (per 1000 children in that age group)
cases_averted_with_PMC_clin_per1000 <- (cases_no_PMC_clin_per1000 - cases_with_PMC_clin_per1000)
cases_averted_with_PMC_sev_per1000 <- (cases_no_PMC_sev_per1000 - cases_with_PMC_sev_per1000)
cases_averted_with_PMC_tot_per1000 <- (cases_no_PMC_tot_per1000 - cases_with_PMC_tot_per1000)
cases_averted_with_PMC_asym_per1000 <- (cases_no_PMC_asym_per1000 - cases_with_PMC_asym_per1000)

cases_averted_with_additional_PMC_clin_per1000 <- (cases_no_PMC_clin_per1000 - cases_with_additional_PMC_clin_per1000)
cases_averted_with_additional_PMC_sev_per1000 <- (cases_no_PMC_sev_per1000 - cases_with_additional_PMC_sev_per1000)
cases_averted_with_additional_PMC_tot_per1000 <- (cases_no_PMC_tot_per1000 - cases_with_additional_PMC_tot_per1000)
cases_averted_with_additional_PMC_asym_per1000 <- (cases_no_PMC_asym_per1000 - cases_with_additional_PMC_asym_per1000)

# reduction in cases (with PMC) by infection class
cases_reduction_with_PMC_clin <- (cases_no_PMC_clin - cases_with_PMC_clin)/cases_no_PMC_clin * 100
cases_reduction_with_PMC_sev <- (cases_no_PMC_sev - cases_with_PMC_sev)/cases_no_PMC_sev * 100
cases_reduction_with_PMC_tot <- (cases_no_PMC_tot - cases_with_PMC_tot)/cases_no_PMC_tot * 100
cases_reduction_with_PMC_asym <- (cases_no_PMC_asym - cases_with_PMC_asym)/cases_no_PMC_asym * 100

cases_reduction_with_additional_PMC_clin <- (cases_no_PMC_clin - cases_with_additional_PMC_clin)/cases_no_PMC_clin * 100
cases_reduction_with_additional_PMC_sev <- (cases_no_PMC_sev - cases_with_additional_PMC_sev)/cases_no_PMC_sev * 100
cases_reduction_with_additional_PMC_tot <- (cases_no_PMC_tot - cases_with_additional_PMC_tot)/cases_no_PMC_tot * 100
cases_reduction_with_additional_PMC_asym <- (cases_no_PMC_asym - cases_with_additional_PMC_asym)/cases_no_PMC_asym * 100


##### APPEND VECTORS TO CALCULATION DATAFRAMES (6 MONTH AGE GROUP INTERVALS) #####

cases_no_PMC_sixmonth_per1000$clinical <- cases_no_PMC_clin_per1000
cases_no_PMC_sixmonth_per1000$severe <- cases_no_PMC_sev_per1000
cases_no_PMC_sixmonth_per1000$total <- cases_no_PMC_tot_per1000
cases_no_PMC_sixmonth_per1000$asymptomatic <- cases_no_PMC_asym_per1000

cases_with_PMC_sixmonth_per1000$clinical<- cases_with_PMC_clin_per1000
cases_with_PMC_sixmonth_per1000$severe<- cases_with_PMC_sev_per1000
cases_with_PMC_sixmonth_per1000$total<- cases_with_PMC_tot_per1000
cases_with_PMC_sixmonth_per1000$asymptomatic<- cases_with_PMC_asym_per1000

cases_with_additional_PMC_sixmonth_per1000$clinical<- cases_with_additional_PMC_clin_per1000
cases_with_additional_PMC_sixmonth_per1000$severe<- cases_with_additional_PMC_sev_per1000
cases_with_additional_PMC_sixmonth_per1000$total<- cases_with_additional_PMC_tot_per1000
cases_with_additional_PMC_sixmonth_per1000$asymptomatic<- cases_with_additional_PMC_asym_per1000

cases_averted_with_PMC_sixmonth_per1000$clinical <- cases_averted_with_PMC_clin_per1000
cases_averted_with_PMC_sixmonth_per1000$severe <- cases_averted_with_PMC_sev_per1000
cases_averted_with_PMC_sixmonth_per1000$total <- cases_averted_with_PMC_tot_per1000
cases_averted_with_PMC_sixmonth_per1000$asymptomatic <- cases_averted_with_PMC_asym_per1000

cases_averted_with_additional_PMC_sixmonth_per1000$clinical <- cases_averted_with_additional_PMC_clin_per1000
cases_averted_with_additional_PMC_sixmonth_per1000$severe <- cases_averted_with_additional_PMC_sev_per1000
cases_averted_with_additional_PMC_sixmonth_per1000$total <- cases_averted_with_additional_PMC_tot_per1000
cases_averted_with_additional_PMC_sixmonth_per1000$asymptomatic <- cases_averted_with_additional_PMC_asym_per1000

cases_reduction_with_PMC_sixmonth$clinical <- cases_reduction_with_PMC_clin
cases_reduction_with_PMC_sixmonth$severe <- cases_reduction_with_PMC_sev
cases_reduction_with_PMC_sixmonth$total <- cases_reduction_with_PMC_tot
cases_reduction_with_PMC_sixmonth$asymptomatic <- cases_reduction_with_PMC_asym

cases_reduction_with_additional_PMC_sixmonth$clinical <- cases_reduction_with_additional_PMC_clin
cases_reduction_with_additional_PMC_sixmonth$severe <- cases_reduction_with_additional_PMC_sev
cases_reduction_with_additional_PMC_sixmonth$total <- cases_reduction_with_additional_PMC_tot
cases_reduction_with_additional_PMC_sixmonth$asymptomatic <- cases_reduction_with_additional_PMC_asym



##### STORE DATAFRAMES IN CSV FORMAT #####

write.csv(cases_no_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_no_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
write.csv(annual_cases_no_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/annual_cases_no_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
write.csv(cases_with_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_with_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
write.csv(annual_cases_with_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/annual_cases_with_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
write.csv(cases_averted_with_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_averted_with_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
write.csv(annual_cases_averted_with_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/annual_cases_averted_with_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
write.csv(cases_reduction_with_PMC, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_reduction_with_PMC_", rur_or_urb, ".csv"), row.names=FALSE)
write.csv(cases_with_additional_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_with_additional_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
write.csv(annual_cases_with_additional_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/annual_cases_with_additional_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
write.csv(cases_averted_with_additional_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_averted_with_additional_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
write.csv(annual_cases_averted_with_additional_PMC_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/annual_cases_averted_with_additional_PMC_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
write.csv(cases_reduction_with_additional_PMC, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_reduction_with_additional_PMC_", rur_or_urb, ".csv"), row.names=FALSE)

write.csv(cases_no_PMC_sixmonth_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_no_PMC_sixmonth_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
write.csv(cases_with_PMC_sixmonth_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_with_PMC_sixmonth_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
write.csv(cases_averted_with_PMC_sixmonth_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_averted_with_PMC_sixmonth_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
write.csv(cases_reduction_with_PMC_sixmonth, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_reduction_with_PMC_sixmonth_", rur_or_urb, ".csv"), row.names=FALSE)
write.csv(cases_with_additional_PMC_sixmonth_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_with_additional_PMC_sixmonth_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
write.csv(cases_averted_with_additional_PMC_sixmonth_per1000, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_averted_with_additional_PMC_sixmonth_per1000_", rur_or_urb, ".csv"), row.names=FALSE)
write.csv(cases_reduction_with_additional_PMC_sixmonth, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/cases_reduction_with_additional_PMC_sixmonth_", rur_or_urb, ".csv"), row.names=FALSE)


to_merge_dataframes <- c("cases_no_PMC", "annual_cases_no_PMC", "cases_with_PMC", 
                         "annual_cases_with_PMC", "cases_with_additional_PMC", "annual_cases_with_additional_PMC",
                         "cases_averted_with_PMC", "annual_cases_averted_with_PMC",
                         "cases_averted_with_additional_PMC", "annual_cases_averted_with_additional_PMC",
                         "cases_no_PMC_sixmonth", "cases_with_PMC_sixmonth", "cases_averted_with_PMC_sixmonth", 
                         "cases_with_additional_PMC_sixmonth", "cases_averted_with_additional_PMC_sixmonth")


for (i in 1:length(to_merge_dataframes)) {
  rural_df <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/", to_merge_dataframes[i], "_rural.csv"))
  urban_df <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/", to_merge_dataframes[i], "_urban.csv"))
  
  merged_df <- c()
  
  for (j in 1:length(area_names)) {
    
    subset_rural_df <- rural_df %>% filter(NAME_2 == area_names[j])
    subset_urban_df <- urban_df %>% filter(NAME_2 == area_names[j])
    
    
    
    if (dim(subset_urban_df)[1] == 0) {
      subset_merged_df <- subset_rural_df 
    }
    
    if (dim(subset_rural_df)[1] == 0) {
      subset_merged_df <- subset_urban_df 
    }
    

    if (area_names[j] %in% unique(subset_rural_df$NAME_2) & area_names[j] %in% unique(subset_urban_df$NAME_2)) {
      
      subset_merged_df <- subset_rural_df 
      
      subset_merged_df$clinical <- subset_rural_df$clinical + subset_urban_df$clinical
      subset_merged_df$severe <- subset_rural_df$severe + subset_urban_df$severe
      subset_merged_df$total <- subset_rural_df$total + subset_urban_df$total
      subset_merged_df$asymptomatic <- subset_rural_df$asymptomatic + subset_urban_df$asymptomatic
      
    }
    
    
    
    subset_merged_df<-as.data.frame(subset_merged_df)
    
    #subset_merged_df$NAME_2 <- rep(area_names[j], times=dim(subset_merged_df)[1])
    
    merged_df <- rbind(merged_df, subset_merged_df)
    
  }
  
  merged_df <- as.data.frame(merged_df)
  
  write.csv(merged_df, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/", to_merge_dataframes[i], "_merged.csv"), row.names=FALSE)
  
}


##### CALCULATE INCIDENCE AND PMC IMPACT FOR WHOLE COUNTRY (both PMC schedules) #####

incidence_ppy_df <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/incidence_ppy_df_merged.csv"))
population_df <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/population_df_merged.csv"))
PMC_impact_ppy<-read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_merged.csv"))
PMC_impact_ppy_additional_doses<-read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_additional_merged.csv"))


PMC_impact_ppy_whole_country_weighted_additional_doses <- PMC_impact_ppy_additional_doses
PMC_impact_ppy_whole_country_weighted <- PMC_impact_ppy
incidence_ppy_df_whole_country_weighted <- incidence_ppy_df

population_weights <- c()

for (i in 1:length(area_names)){
  
  pop <- sum((full_data$population %>% filter(year == 2023, name_2 == area_names[i]))$pop)
  
  pop_weight <- pop / ( sum((full_data$population %>% filter(year==2023))$pop))
  population_weights <- c(population_weights, pop_weight)
}

population_weights <- rep(population_weights, each=4*length(age_in_days_midpoint))
PMC_impact_ppy_whole_country_weighted_additional_doses$value <- PMC_impact_ppy_whole_country_weighted_additional_doses$value * population_weights
PMC_impact_ppy_whole_country_weighted$value <- PMC_impact_ppy_whole_country_weighted$value * population_weights
incidence_ppy_df_whole_country_weighted$value <- incidence_ppy_df_whole_country_weighted$value * population_weights

PMC_impact_ppy_whole_country_additional_doses <- data.frame(age_in_days_midpoint)
PMC_impact_ppy_whole_country<- data.frame(age_in_days_midpoint)
incidence_ppy_df_whole_country <- data.frame(age_in_days_midpoint)

whole_country_list <- c(PMC_impact_ppy_whole_country_additional_doses,
                        PMC_impact_ppy_whole_country,
                        incidence_ppy_df_whole_country)

PMC_impact_ppy_whole_country_additional_doses_age_sum_clin <- PMC_impact_ppy_whole_country_age_sum_clin <- incidence_ppy_df_whole_country_age_sum_clin <- c()
PMC_impact_ppy_whole_country_additional_doses_age_sum_sev <- PMC_impact_ppy_whole_country_age_sum_sev <- incidence_ppy_df_whole_country_age_sum_sev <- c()
PMC_impact_ppy_whole_country_additional_doses_age_sum_tot <- PMC_impact_ppy_whole_country_age_sum_tot <- incidence_ppy_df_whole_country_age_sum_tot <- c()
PMC_impact_ppy_whole_country_additional_doses_age_sum_asym <- PMC_impact_ppy_whole_country_age_sum_asym <- incidence_ppy_df_whole_country_age_sum_asym <- c()

for (i in 1:length(age_in_days_midpoint)) {
  
  PMC_impact_ppy_whole_country_additional_doses_age_sum_clin <- c(PMC_impact_ppy_whole_country_additional_doses_age_sum_clin ,sum((PMC_impact_ppy_whole_country_weighted_additional_doses %>% filter(infection_class == "clinical", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
  PMC_impact_ppy_whole_country_age_sum_clin <- c(PMC_impact_ppy_whole_country_age_sum_clin, sum((PMC_impact_ppy_whole_country_weighted %>% filter(infection_class == "clinical", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
  incidence_ppy_df_whole_country_age_sum_clin <- c(incidence_ppy_df_whole_country_age_sum_clin, sum((incidence_ppy_df_whole_country_weighted %>% filter(infection_class == "clinical", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
  
  PMC_impact_ppy_whole_country_additional_doses_age_sum_sev <- c(PMC_impact_ppy_whole_country_additional_doses_age_sum_sev ,sum((PMC_impact_ppy_whole_country_weighted_additional_doses %>% filter(infection_class == "severe", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
  PMC_impact_ppy_whole_country_age_sum_sev <- c(PMC_impact_ppy_whole_country_age_sum_sev, sum((PMC_impact_ppy_whole_country_weighted %>% filter(infection_class == "severe", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
  incidence_ppy_df_whole_country_age_sum_sev <- c(incidence_ppy_df_whole_country_age_sum_sev, sum((incidence_ppy_df_whole_country_weighted %>% filter(infection_class == "severe", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
  
  PMC_impact_ppy_whole_country_additional_doses_age_sum_tot <- c(PMC_impact_ppy_whole_country_additional_doses_age_sum_tot ,sum((PMC_impact_ppy_whole_country_weighted_additional_doses %>% filter(infection_class == "total", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
  PMC_impact_ppy_whole_country_age_sum_tot <- c(PMC_impact_ppy_whole_country_age_sum_tot, sum((PMC_impact_ppy_whole_country_weighted %>% filter(infection_class == "total", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
  incidence_ppy_df_whole_country_age_sum_tot <- c(incidence_ppy_df_whole_country_age_sum_tot, sum((incidence_ppy_df_whole_country_weighted %>% filter(infection_class == "total", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
  
  PMC_impact_ppy_whole_country_additional_doses_age_sum_asym <- c(PMC_impact_ppy_whole_country_additional_doses_age_sum_asym ,sum((PMC_impact_ppy_whole_country_weighted_additional_doses %>% filter(infection_class == "asymptomatic", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
  PMC_impact_ppy_whole_country_age_sum_asym <- c(PMC_impact_ppy_whole_country_age_sum_asym, sum((PMC_impact_ppy_whole_country_weighted %>% filter(infection_class == "asymptomatic", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
  incidence_ppy_df_whole_country_age_sum_asym <- c(incidence_ppy_df_whole_country_age_sum_asym, sum((incidence_ppy_df_whole_country_weighted %>% filter(infection_class == "asymptomatic", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
  
}

PMC_impact_ppy_whole_country_additional_doses$clinical <- PMC_impact_ppy_whole_country_additional_doses_age_sum_clin
PMC_impact_ppy_whole_country$clinical <- PMC_impact_ppy_whole_country_age_sum_clin
incidence_ppy_df_whole_country$clinical <- incidence_ppy_df_whole_country_age_sum_clin

PMC_impact_ppy_whole_country_additional_doses$severe <- PMC_impact_ppy_whole_country_additional_doses_age_sum_sev
PMC_impact_ppy_whole_country$severe <- PMC_impact_ppy_whole_country_age_sum_sev
incidence_ppy_df_whole_country$severe <- incidence_ppy_df_whole_country_age_sum_sev

PMC_impact_ppy_whole_country_additional_doses$total <- PMC_impact_ppy_whole_country_additional_doses_age_sum_tot
PMC_impact_ppy_whole_country$total <- PMC_impact_ppy_whole_country_age_sum_tot
incidence_ppy_df_whole_country$total <- incidence_ppy_df_whole_country_age_sum_tot

PMC_impact_ppy_whole_country_additional_doses$asymptomatic <- PMC_impact_ppy_whole_country_additional_doses_age_sum_asym
PMC_impact_ppy_whole_country$asymptomatic <- PMC_impact_ppy_whole_country_age_sum_asym
incidence_ppy_df_whole_country$asymptomatic <- incidence_ppy_df_whole_country_age_sum_asym


write.csv(incidence_ppy_df_whole_country, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/incidence_ppy_df_whole_country.csv"), row.names=FALSE)
write.csv(PMC_impact_ppy_whole_country, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_whole_country.csv"), row.names=FALSE)
write.csv(PMC_impact_ppy_whole_country_additional_doses, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_whole_country_additional_doses.csv"), row.names=FALSE)


