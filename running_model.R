

# 1. model has run and now need to do the pmc, merge and graphs analysis
# 2. check that it all is working correctly and check different admin1 units
# 3. work out a way to calculate # pmc doses given by using population size and 
#    coverage of PMC. work it out for each dose individually and then a sum of
#    all doses during the schedule 
# 4. incorporate this work into the economics table work to calculate costs 




##### LOAD PACKAGES ##### 

library(pacman)
p_load(malariasimulation, foreSIGHT, malariaEquilibrium, tidyr, dplyr, ggplot2,
       reshape2, ggpubr, gridExtra, readxl, stringi, scene, XML, maps, readr,
       here, sf)

##### IDENTIFY COUNTRY AND AREA CLASSIFICATION #####  

country_code <- "CMR" 

##### Set WD ##### 

# Model outputs will be saved in a folder called "simulation_results" within this work directory
# Ensure the following are in this WD: 
# 1. country shapefiles
# 2. country rds foresite file
# 3. DHS coverage data
# 4. "HAPLOTYPE PROPORTIONS.R" script
# 5. "plusproject_countries_coverage_dhs" excel data
# 6. "haplotypes_combined_forIshana2" excel data
# 7. "drug_policies_updated" excel data 

# set to one below to run on computer
setwd(paste0("C:/Users/IshanaAtkinson/OneDrive - London School of Hygiene and Tropical Medicine/Documents/R files/RUN_MODEL"))

# set to one below to run on cluster
#setwd(paste0("/home/lshia6/", country_code, "_HPC"))


###### SET SEED FOR IMPERIAL MODEL ######

set.seed(222)

###### READ FORESITE DATA AND GET PARAMETERS ######

full_data <- readRDS(paste0(getwd(), "/", country_code, "/", country_code, ".rds")) 

# convert all "-" and spaces into "_" to ease in analysis later on and remove any accents from admin-1 area names 
full_data$sites$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$sites$name_1), id = "Latin-ASCII")
full_data$prevalence$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$prevalence$name_1), id = "Latin-ASCII") 
full_data$interventions$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$interventions$name_1), id = "Latin-ASCII")
full_data$population$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$population$name_1), id = "Latin-ASCII") 
full_data$vectors$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$vectors$name_1), id = "Latin-ASCII")
full_data$pyrethroid_resistance$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$pyrethroid_resistance$name_1), id = "Latin-ASCII")
full_data$seasonality$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$seasonality$name_1), id = "Latin-ASCII")
full_data$eir$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$eir$name_1), id = "Latin-ASCII") 

full_data$sites$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$sites$name_2), id = "Latin-ASCII")
full_data$prevalence$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$prevalence$name_2), id = "Latin-ASCII") 
full_data$interventions$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$interventions$name_2), id = "Latin-ASCII")
full_data$population$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$population$name_2), id = "Latin-ASCII") 
full_data$vectors$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$vectors$name_2), id = "Latin-ASCII")
full_data$pyrethroid_resistance$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$pyrethroid_resistance$name_2), id = "Latin-ASCII")
full_data$seasonality$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$seasonality$name_2), id = "Latin-ASCII")
full_data$eir$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$eir$name_2), id = "Latin-ASCII") 


###### SET UP DATAFRAMES TO FILL ######

sim_output_rural <- c() # simulation output data frame (rural)
incidence_ppy_df_rural <- c() # incidence per person per year data frame (rural)
incidence_df_rural <- c() # incidence data frame (rural)
population_df_rural <- c() # population size data frame (rural)


sim_output_urban <- c() # simulation output data frame (urban)
incidence_ppy_df_urban <- c() # incidence per person per year data frame (urban)
incidence_df_urban <- c() # incidence data frame (urban)
population_df_urban <- c() # population size data frame (urban)



##### COMMON PARAMETERS/VARIABLES ##### 

human_population <- 1e6 # population size in model 
min_age_to_model <- 0 # minimum age to model (in days)
max_age_to_model <- 2.5*365 # enter in multiples of 0.5, maximum age to model (in days)

# set the time span over which to simulate
# NOTE: currently 23 years is the max as we only have ITN/IRS data for this length
year <- 365; years <- 1; sim_length_for_data <- year * years

years_proj_forward <- 1 # number of years to project forward following known data
years_of_simulation <- years + years_proj_forward # simulation length (years) 
sim_length <- (years + years_proj_forward) * year # simulation length (days)

# interval between modeled age groups (1 = days, 7 = weeks etc)
step_length <- 1

# vector of min and max ages to be used in each age bracket
age_min <- seq(min_age_to_model, max_age_to_model, step_length) 
age_max <- seq(min_age_to_model, max_age_to_model, step_length) + step_length

# six month ages (in days)
sixmonth_intervals <- c(0, 183, 365, 548, 730, 913, 1095, 1278, 1460, 1643,
                        1825, 2008, 2190, 2373, 2555, 2738, 2920, 3285, 3468,
                        3650, 4015)

# time steps used in model (number of rows in the simulations output data frame)
timesteps<-seq(0,round(sim_length),1)

# vector for the midpoint age for each age bracket (useful for graphing)
age_in_days <- seq(age_min[1], age_max[length(age_max)], length.out=length(age_min) + 1)
age_in_days_midpoint <- age_in_days[-length(age_in_days)] + diff(age_in_days)/2

# number of 6 month interval age groups to model 
no_sixmonth_intervals <- round(max(age_max)/182.5)

# empty vectors for the column names of interest 
age_group_names <- c() # age groups
age_group_names_sixmonth <- c() # 6 month age groups
sixmonth_intervals_midpoint <- c() # 6 month age group midpoints
clin_inc_cols <- c() # clinical incidence column names
sev_inc_cols <- c() # severe incidence column names
tot_inc_cols <- c() # total incidence column names
asym_inc_cols <- c() # asymptomatic incidence column names

# fill empty vectors
for (i in 1:(length(age_min))) {
  age_group_names <- append(age_group_names, paste0("n_age_", as.character(age_min[i]), "_", as.character(age_max[i])))
}

for (i in 1:(length(sixmonth_intervals) - 1)) {
  
  if (i == (length(sixmonth_intervals) - 1)) {
    age_group_names_sixmonth <- append(age_group_names_sixmonth, paste0("n_age_", as.character(sixmonth_intervals[i]), "_", as.character(sixmonth_intervals[i+1])))
    sixmonth_intervals_midpoint <- append(sixmonth_intervals_midpoint, (sixmonth_intervals[i]+sixmonth_intervals[i+1])/2)
    }
  
  if (i != (length(sixmonth_intervals) - 1)) {
    age_group_names_sixmonth <- append(age_group_names_sixmonth, paste0("n_age_", as.character(sixmonth_intervals[i]), "_", as.character(sixmonth_intervals[i+1] - 1)))
    sixmonth_intervals_midpoint <- append(sixmonth_intervals_midpoint, (sixmonth_intervals[i]+(sixmonth_intervals[i+1] - 1))/2)
    }
  
}

# write column names 
for (i in 1:length(age_min)) {
  clin_inc_cols <- append(clin_inc_cols, paste0("n_inc_clinical_", as.character(age_min[i]), "_", as.character(age_max[i])))
  sev_inc_cols <- append(sev_inc_cols, paste0("n_inc_severe_", as.character(age_min[i]), "_", as.character(age_max[i])))
  tot_inc_cols <- append(tot_inc_cols, paste0("n_inc_", as.character(age_min[i]), "_", as.character(age_max[i])))
  asym_inc_cols <- append(asym_inc_cols, paste0("n_inc_asym_", as.character(age_min[i]), "_", as.character(age_max[i])))
}

##### RURAL MODEL SIMULATION PARAMETERS #####

rur_or_urb <- "rural" # set rurality

# vector of area names in country with rural areas 
area_names <- unique((full_data$sites %>% filter(urban_rural == rur_or_urb))$name_2)

area_names <-"Gaza"

##### RUN RURAL MODEL SIMULATION #####

if (rur_or_urb == "rural") {
  
  for (i in 1:length(area_names)){
    
    # start time simulation
    start_time <- Sys.time()
    
    ##### SET UP PARAMETERS AND INTERVENTIONS #####
    
    # current area 
    current_area_name <-area_names[i]
    
    # starting EIR
    starting_EIR <- (full_data$eir %>%
                       filter(name_2==current_area_name) %>%
                       filter(spp=="pf") %>%
                       filter(urban_rural == rur_or_urb))$eir
    
    
    # if theres no rural data for the current admin-1 area
    if (length(starting_EIR) == 0) {
      print(paste0("No ", rur_or_urb, " data for the area: ", current_area_name))
      next
    }
    
    # if the EIR is 0 
    if (starting_EIR == 0) {
      print(paste0("Starting EIR == 0 for ", rur_or_urb, " area in ", current_area_name, ". Therefore model cannot run."))
      next
    }
    
    
    # SEASONALITY
    seas_data <- full_data$seasonality %>% filter(name_2==current_area_name) 
    
    g0 <- seas_data$g0
    g <- as.double(c(seas_data$g1, seas_data$g2, seas_data$g3))
    h <- as.double(c(seas_data$h1, seas_data$h2, seas_data$h3))
    
    
    # AGE STRUCTURE 
    # initialise the age structures and seasonality in parameters
    
    simparams <- get_parameters(
      list(
        human_population = human_population,
        age_group_rendering_min_ages = age_min,
        age_group_rendering_max_ages = age_max,
        incidence_rendering_min_ages = age_min,
        incidence_rendering_max_ages = age_max,
        clinical_incidence_rendering_min_ages = age_min,
        clinical_incidence_rendering_max_ages = age_max,
        severe_incidence_rendering_min_ages = age_min,
        severe_incidence_rendering_max_ages = age_max,
        model_seasonality = TRUE,
        g0 = g0,
        g = g,
        h = h
      )
    )
    
    # SPECIES
    
    # subset foresite file to access species data by area
    species_data <- full_data$vectors %>% 
      filter(name_2 == current_area_name)
    
    # arabiensis
    if (any(species_data$species == "arabiensis") == TRUE) {
      arab_prop <- (species_data %>% filter(species == "arabiensis"))$prop
    } else {
      arab_prop <- 0
    }
    
    # funestus
    if (any(species_data$species == "funestus") == TRUE) {
      fun_prop <- (species_data %>% filter(species == "funestus"))$prop
    } else {
      fun_prop <- 0
    }
    
    # gambiae
    if (any(species_data$species == "gambiae") == TRUE) {
      gamb_prop <- (species_data %>% filter(species == "gambiae"))$prop
    } else {
      gamb_prop <- 0
    }
    
    # vector of proportions of each species 
    species_prop <- round(c(arab_prop, fun_prop, gamb_prop),digits=10)
    
    # add to parameter list
    simparams <- set_species(
      simparams,
      species = list(arab_params, fun_params, gamb_params), # must be in same order as species_prop
      proportions = species_prop
    )
    
    
    # INTERVENTION DATA
    
    # subset foresite file to get intervention data by area/classification
    intervention_data <- full_data$interventions %>%
      filter(name_2 == current_area_name) %>%
      filter(urban_rural == rur_or_urb)
    
    # subset intervention data by last n years of known data (n defined earlier)
    intervention_data <- tail(intervention_data, n=years)
     
    # assume interventions continue at the same coverage and schedule from last known data
    interv_data_proj_forwards <- intervention_data |> expand_interventions(max_year=(tail(intervention_data$year, n=1)+years_proj_forward), group_var="name_2")
    interv_data_proj_forwards <- interv_data_proj_forwards |> fill_extrapolate(group_var = "name_2")
    
    
    # BED NETS
    
    # bed nets assumed to be distributed at the end of each year
    itn_timesteps <- c(1:(sim_length/year)) * year
    itn_timesteps <- itn_timesteps[itn_timesteps <= tail(timesteps, n=1)] # remove values higher than simulation length


    simparams <- set_bednets(
      simparams,
      timesteps = itn_timesteps,
      coverages = interv_data_proj_forwards$itn_use,  # each round is distributed to x% of the population randomly
      retention = 5 * year, # Nets are kept on average 5 years
      dn0 = matrix(interv_data_proj_forwards$dn0, nrow = length(itn_timesteps), ncol = length(simparams$species)), # matrix of death probabilities for each mosquito species (here 3 species only so 3 columns) for last X years
      rn = matrix(interv_data_proj_forwards$rn0, nrow = length(itn_timesteps), ncol = length(simparams$species)), # matrix of repelling probabilities for each mosquito species (here 3 species only so 3 columns) for last X years
      rnm = matrix(interv_data_proj_forwards$rnm, nrow = length(itn_timesteps), ncol = length(simparams$species)), # matrix of minimum repelling probabilities for each mosquito species (here 3 species only so 3 columns) for last X years
      gamman = interv_data_proj_forwards$gamman # vector of bed net half-lives for each distribution timestep
    )
    
    
    # IRS
    
    # irs is assumed to be implemented at the beginning of each year
    irs_timesteps <- c(1,(1:(sim_length/year) * year + 1))
    irs_timesteps <- irs_timesteps[irs_timesteps <= tail(timesteps, n=1)] #  remove values higher than simulation length


    simparams <- set_spraying(
      simparams,
      timesteps = irs_timesteps,
      coverages = interv_data_proj_forwards$irs_cov, # NOTE: coverage is very low so has very little impact on model. tail() to access the last X number of years of data
      ls_theta = matrix(interv_data_proj_forwards$ls_theta, nrow = length(irs_timesteps), ncol = length(simparams$species)), # matrix of mortality parameters; nrows=length(timesteps), ncols=length(species)
      ls_gamma = matrix(interv_data_proj_forwards$ls_gamma, nrow = length(irs_timesteps), ncol = length(simparams$species)), # matrix of mortality parameters per round of IRS and per species
      ks_theta = matrix(interv_data_proj_forwards$ks_theta, nrow = length(irs_timesteps), ncol = length(simparams$species)), # matrix of feeding success parameters per round of IRS and per species
      ks_gamma = matrix(interv_data_proj_forwards$ks_gamma, nrow = length(irs_timesteps), ncol = length(simparams$species)), # matrix of feeding success parameters per round of IRS and per species
      ms_theta = matrix(interv_data_proj_forwards$ms_theta, nrow = length(irs_timesteps), ncol = length(simparams$species)), # matrix of deterrence parameters per round of IRS and per species
      ms_gamma = matrix(interv_data_proj_forwards$ms_gamma, nrow = length(irs_timesteps), ncol = length(simparams$species)) # matrix of deterrence parameters per round of IRS and per species
    )
    
    
    
    # PMC 
    
    # # ages at which PMC is administered
    # schedule_BEN_8doses<- c(10*7, 14*7, 6*30, 9*30, 12*30, 15*30, 18*30, 24*30)
    # 
    # # coverage of each PMC dose to target population 
    # cov_BEN_8doses<- rep(0.5, sim_length)
    # 
    # # timesteps for each round of PMC
    # testing_timesteps <- (1:sim_length)
    # 
    # # drug used
    # #SP_AQ_params<- c(1,0.32,4.3,38.1)
    # simparams <- set_drugs(simparams, list(SP_AQ_params))
    # 
    # simparams <- set_pmc(
    #   simparams,
    #   drug = 1,
    #   timesteps = testing_timesteps,
    #   coverages = cov_BEN_8doses,
    #   ages = schedule_BEN_8doses
    # )
    
    
    # TREATMENT
    
    treatment_policy <- read_xlsx("drug_policies_updated.xlsx", sheet=1)

    # most recent polcy for the country
    country_policy <- (treatment_policy %>% filter(ISO_CODE == country_code))["2020"]

    # treatment coverages (overall, ACT and non-ACT)
    overall_treatment_cov <- as.numeric(interv_data_proj_forwards$tx_cov)
    ACT_treatment_cov <- as.numeric(interv_data_proj_forwards$prop_act) * overall_treatment_cov
    non_ACT_treatment_cov <- overall_treatment_cov - ACT_treatment_cov

    # change treatment coverages at the beginning of each year
    treatment_timestep <- seq(from=1, to=years_of_simulation*year, by=year)

    # AS-AQ parameters obtained from model fitting
    AS_AQ_params <- c(0.9, 0.09434, 4.7, 16.4)
    
    sim_params <- set_drugs(simparams, list(AL_params, SP_AQ_params, AS_AQ_params))

    # AL listed
    if (country_policy == "AL" | country_policy == "AL+PQ") {
      sim_params <- set_clinical_treatment(
        parameters = sim_params,
        drug = 1, # AL (ACT)
        timesteps =  treatment_timestep, # Treatment coverage changes
        coverages =  ACT_treatment_cov)
    }

    # AS+AQ or AS+SP listed
    if (country_policy == "AS+AQ") {
      sim_params <- set_clinical_treatment(
        parameters = sim_params,
        drug = 3, # AS-AQ (ACT) # check this is okay since AS-AQ acts longer than AS alone
        timesteps =  treatment_timestep, # Treatment coverage changes
        coverages =  ACT_treatment_cov)
    }

    # AL and AS+AQ listed so we assume 50/50 split in coverage
    if (country_policy == "AL, AS+AQ") {
      sim_params <- set_clinical_treatment(
        parameters = sim_params,
        drug = 1, # AL
        timesteps =  treatment_timestep, # Treatment coverage changes
        coverages =  0.5*ACT_treatment_cov)
    }

    if (country_policy == "AL, AS+AQ") {
      sim_params <- set_clinical_treatment(
        parameters = sim_params,
        drug = 3, # AS+AQ
        timesteps =  treatment_timestep, # Treatment coverage changes
        coverages =  0.5*ACT_treatment_cov)
    }

    if (country_policy == "NO DATA") {
      sim_params <- set_clinical_treatment(
        parameters = sim_params,
        drug = 1, # Assume AL in absence of data
        timesteps =  treatment_timestep, # Treatment coverage changes
        coverages =  ACT_treatment_cov)
    }


    # assume AL for anything else

    if (country_policy != "AL+PQ" &
        country_policy != "AL" &
        country_policy != "AS+AQ" &
        country_policy != "AL, AS+AQ" &
        country_policy != "NO DATA") {
      sim_params <- set_clinical_treatment(
        parameters = sim_params,
        drug = 1, # Assume AL
        timesteps =  treatment_timestep, # Treatment coverage changes
        coverages =  ACT_treatment_cov)
    }


    # assume SP+AQ for non-ACT treatments
    sim_params <- set_clinical_treatment(
      parameters = sim_params,
      drug = 2, # SP-AQ (non-ACT)
      timesteps =  treatment_timestep, # Treatment coverage changes
      coverages =  non_ACT_treatment_cov)
    
    
    ##### RUN MODEL #####
    
    # adjusts IBM parameters to match equilibrium parameters
    simparams <- set_equilibrium(simparams, starting_EIR)
    
    # run simulation 
    test_sim <- run_simulation(sim_length, simparams)
    
    # assign location data to simulation results 
    test_sim$iso_code <- rep(country_code, dim(test_sim)[1])
    test_sim$rur_or_urb <- rep(rur_or_urb, dim(test_sim)[1])
    test_sim$area <- rep(current_area_name, dim(test_sim)[1])
    test_sim <- test_sim %>%
      relocate(timestep, iso_code, rur_or_urb, area)
    
    
    # store all simulation results
    sim_output_rural <- rbind(sim_output_rural, test_sim)
    
    
    ##### CREATE INCIDENCE VECTORS #####

    clin_inc_data_mats_rural <- c() # clinical incidence
    sev_inc_data_mats_rural <- c() # severe incidence
    tot_inc_data_mats_rural <- c() # total incidence
    asym_inc_data_mats_rural <- c() # asymptomatic incidence
    pop_size_data_mats_rural <- c() # population size 
    new_clin_inc_per_person_rural <- c() # clinical incidence (per capita)
    new_sev_inc_per_person_rural <- c() # severe incidence (per capita)
    new_tot_inc_per_person_rural <- c() # total incidence (per capita)
    new_asym_inc_per_person_rural <- c() # asymptomatic incidence (per capita)
    
    for (j in 1:length(test_sim$timestep)) {
      
      # create matrix with data for population size for each age group
      current_pop_size_data_rural <- test_sim[j, age_group_names]
      
      # create matrix with data for new infections for each age group
      current_clin_inc_data_rural <- test_sim[j, clin_inc_cols]
      current_sev_inc_data_rural <- test_sim[j, sev_inc_cols]
      current_tot_inc_data_rural <- test_sim[j, tot_inc_cols]
      current_asym_inc_data_rural <- current_tot_inc_data_rural - current_clin_inc_data_rural
      
      # populate data frame with data at each timestep 
      clin_inc_data_mats_rural<-rbind(clin_inc_data_mats_rural, current_clin_inc_data_rural)
      sev_inc_data_mats_rural<-rbind(sev_inc_data_mats_rural, current_sev_inc_data_rural)
      tot_inc_data_mats_rural<-rbind(tot_inc_data_mats_rural, current_tot_inc_data_rural)
      asym_inc_data_mats_rural<-rbind(asym_inc_data_mats_rural, current_asym_inc_data_rural)
      pop_size_data_mats_rural<-rbind(pop_size_data_mats_rural, current_pop_size_data_rural)
      
      # new infections per capita at each timestep (NaN appear is pop size = 0)
      new_clin_inc_per_person_rural <- rbind(new_clin_inc_per_person_rural, current_clin_inc_data_rural/current_pop_size_data_rural)
      new_sev_inc_per_person_rural <- rbind(new_sev_inc_per_person_rural, current_sev_inc_data_rural/current_pop_size_data_rural)
      new_tot_inc_per_person_rural <- rbind(new_tot_inc_per_person_rural, current_tot_inc_data_rural/current_pop_size_data_rural)
      new_asym_inc_per_person_rural <- rbind(new_asym_inc_per_person_rural, current_asym_inc_data_rural/current_pop_size_data_rural)
      
    }
    
    # assign location, time and infection class data to simulation results 
    colnames(clin_inc_data_mats_rural) <- age_group_names
    clin_inc_data_mats_rural$timestep <- 1:sim_length
    clin_inc_data_mats_rural$infection_class <- "clinical"
    clin_inc_data_mats_rural$iso_code <- rep(country_code, dim(clin_inc_data_mats_rural)[1])
    clin_inc_data_mats_rural$rur_or_urb <- rep(rur_or_urb, dim(clin_inc_data_mats_rural)[1])
    clin_inc_data_mats_rural$area <- rep(current_area_name, dim(clin_inc_data_mats_rural)[1])
    clin_inc_data_mats_rural <- clin_inc_data_mats_rural %>%
      relocate(timestep, iso_code, rur_or_urb, area, infection_class)
    
    colnames(sev_inc_data_mats_rural) <- age_group_names
    sev_inc_data_mats_rural$timestep <- 1:sim_length
    sev_inc_data_mats_rural$infection_class <- "severe"
    sev_inc_data_mats_rural$iso_code <- rep(country_code, dim(sev_inc_data_mats_rural)[1])
    sev_inc_data_mats_rural$rur_or_urb <- rep(rur_or_urb, dim(sev_inc_data_mats_rural)[1])
    sev_inc_data_mats_rural$area <- rep(current_area_name, dim(sev_inc_data_mats_rural)[1])
    sev_inc_data_mats_rural <- sev_inc_data_mats_rural %>%
      relocate(timestep, iso_code, rur_or_urb, area, infection_class)
    
    colnames(tot_inc_data_mats_rural) <- age_group_names
    tot_inc_data_mats_rural$timestep <- 1:sim_length
    tot_inc_data_mats_rural$infection_class <- "total"
    tot_inc_data_mats_rural$iso_code <- rep(country_code, dim(tot_inc_data_mats_rural)[1])
    tot_inc_data_mats_rural$rur_or_urb <- rep(rur_or_urb, dim(tot_inc_data_mats_rural)[1])
    tot_inc_data_mats_rural$area <- rep(current_area_name, dim(tot_inc_data_mats_rural)[1])
    tot_inc_data_mats_rural <- tot_inc_data_mats_rural %>%
      relocate(timestep, iso_code, rur_or_urb, area, infection_class)
    
    colnames(asym_inc_data_mats_rural) <- age_group_names
    asym_inc_data_mats_rural$timestep <- 1:sim_length
    asym_inc_data_mats_rural$infection_class <- "asymptomatic"
    asym_inc_data_mats_rural$iso_code <- rep(country_code, dim(asym_inc_data_mats_rural)[1])
    asym_inc_data_mats_rural$rur_or_urb <- rep(rur_or_urb, dim(asym_inc_data_mats_rural)[1])
    asym_inc_data_mats_rural$area <- rep(current_area_name, dim(asym_inc_data_mats_rural)[1])
    asym_inc_data_mats_rural <- asym_inc_data_mats_rural %>%
      relocate(timestep, iso_code, rur_or_urb, area, infection_class)
    
    colnames(pop_size_data_mats_rural) <- age_group_names
    pop_size_data_mats_rural$timestep <- 1:sim_length
    pop_size_data_mats_rural$iso_code <- rep(country_code, dim(pop_size_data_mats_rural)[1])
    pop_size_data_mats_rural$rur_or_urb <- rep(rur_or_urb, dim(pop_size_data_mats_rural)[1])
    pop_size_data_mats_rural$area <- rep(current_area_name, dim(pop_size_data_mats_rural)[1])
    pop_size_data_mats_rural <- pop_size_data_mats_rural %>%
      relocate(timestep, iso_code, rur_or_urb, area)
    
    population_df_rural <- rbind(population_df_rural, pop_size_data_mats_rural)
    
    
    # store clinical incidence results from each simulation run 
    
    # create average new clinical infections per person per year for each age group
    
    clin_inc_ppy_rural  <- data.frame()
    sev_inc_ppy_rural  <- data.frame()
    tot_inc_ppy_rural  <- data.frame()
    asym_inc_ppy_rural  <- data.frame()

    for (j in 1:dim(new_clin_inc_per_person_rural)[2]) {
      clin_inc_ppy_rural <- rbind(clin_inc_ppy_rural, sum(new_clin_inc_per_person_rural[,j], na.rm=TRUE)/years_of_simulation)
    }
    
    for (j in 1:dim(new_sev_inc_per_person_rural)[2]) {
      sev_inc_ppy_rural <- rbind(sev_inc_ppy_rural, sum(new_sev_inc_per_person_rural[,j], na.rm=TRUE)/years_of_simulation)
    }
    
    for (j in 1:dim(new_tot_inc_per_person_rural)[2]) {
      tot_inc_ppy_rural <- rbind(tot_inc_ppy_rural, sum(new_tot_inc_per_person_rural[,j], na.rm=TRUE)/years_of_simulation)
    }
    
    for (j in 1:dim(new_asym_inc_per_person_rural)[2]) {
      asym_inc_ppy_rural <- rbind(asym_inc_ppy_rural, sum(new_asym_inc_per_person_rural[,j], na.rm=TRUE)/years_of_simulation)
    }
    
    
    # assign location and age group data to simulation results
    
    colnames(clin_inc_ppy_rural) = "value"
    clin_inc_ppy_rural$area <- current_area_name
    clin_inc_ppy_rural$infection_class <- "clinical"
    clin_inc_ppy_rural$iso_code <- country_code
    clin_inc_ppy_rural$rur_or_urb <- rur_or_urb
    clin_inc_ppy_rural$age_group <- age_group_names 
    clin_inc_ppy_rural$age_in_days_midpoint <- age_in_days_midpoint
    clin_inc_ppy_rural$units <- "ppy" 
    clin_inc_ppy_rural <- clin_inc_ppy_rural %>%
      relocate(iso_code, area, rur_or_urb, age_group, age_in_days_midpoint, infection_class, units)
    
    colnames(sev_inc_ppy_rural) = "value"
    sev_inc_ppy_rural$area <- current_area_name
    sev_inc_ppy_rural$infection_class <- "severe"
    sev_inc_ppy_rural$iso_code <- country_code
    sev_inc_ppy_rural$rur_or_urb <- rur_or_urb
    sev_inc_ppy_rural$age_group <- age_group_names
    sev_inc_ppy_rural$age_in_days_midpoint <- age_in_days_midpoint
    sev_inc_ppy_rural$units <- "ppy" 
    sev_inc_ppy_rural <- sev_inc_ppy_rural %>%
      relocate(iso_code, area, rur_or_urb, age_group, age_in_days_midpoint, infection_class, units)
    
    colnames(tot_inc_ppy_rural) = "value"
    tot_inc_ppy_rural$area <- current_area_name
    tot_inc_ppy_rural$infection_class <- "total"
    tot_inc_ppy_rural$iso_code <- country_code
    tot_inc_ppy_rural$rur_or_urb <- rur_or_urb
    tot_inc_ppy_rural$age_group <- age_group_names
    tot_inc_ppy_rural$age_in_days_midpoint <- age_in_days_midpoint
    tot_inc_ppy_rural$units <- "ppy" 
    tot_inc_ppy_rural <- tot_inc_ppy_rural %>%
      relocate(iso_code, area, rur_or_urb, age_group, age_in_days_midpoint, infection_class, units)
    
    colnames(asym_inc_ppy_rural) = "value"
    asym_inc_ppy_rural$area <- current_area_name    
    asym_inc_ppy_rural$infection_class <- "asymptomatic"
    asym_inc_ppy_rural$iso_code <- country_code
    asym_inc_ppy_rural$rur_or_urb <- rur_or_urb
    asym_inc_ppy_rural$age_group <- age_group_names
    asym_inc_ppy_rural$age_in_days_midpoint <- age_in_days_midpoint
    asym_inc_ppy_rural$units <- "ppy" 
    asym_inc_ppy_rural <- asym_inc_ppy_rural %>%
      relocate(iso_code, area, rur_or_urb, age_group, age_in_days_midpoint, infection_class, units)
    
    
    # merge all infection class incidence dataframes together
    
    current_incidence_ppy_df_rural <- rbind(clin_inc_ppy_rural, sev_inc_ppy_rural,
                                            tot_inc_ppy_rural, asym_inc_ppy_rural)
    
    current_incidence_df_rural <- rbind(clin_inc_data_mats_rural, sev_inc_data_mats_rural,
                                        tot_inc_data_mats_rural, asym_inc_data_mats_rural)
    
    incidence_ppy_df_rural <- rbind(incidence_ppy_df_rural, current_incidence_ppy_df_rural)
    incidence_df_rural <- rbind(incidence_df_rural, current_incidence_df_rural)
    
    # end timer
    end_time <- Sys.time()
    
    # time taken for simulation
    time_elapsed <- end_time - start_time
    
    ##### PRINT UPDATES #####
    print(paste0("Successful run for the ", rur_or_urb, " region in: ", current_area_name, ". ",
                 "Time taken to run model loop: ", time_elapsed))
    
    
  }

  
  
  ##### store simulation data in CSV format #####

  # all simulation data
  write.csv(sim_output_rural, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/sim_output_rural.csv"), row.names=FALSE)
  
  # average clinical incidence per person per year for each site
  write.csv(incidence_ppy_df_rural, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/incidence_ppy_df_rural.csv"), row.names=FALSE)
  
  # number of clinical infections in each age group at each timestep, for each site
  write.csv(incidence_df_rural, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/incidence_df_rural.csv"), row.names=FALSE)
  
  # population sizes in each age group at each timestep, for each site 
  write.csv(population_df_rural, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/population_df_rural.csv"), row.names=FALSE)
  

  
}


##### URBAN MODEL SIMULATION PARAMETERS ####

rur_or_urb <- "urban" # set rurality


# vector of area names in country with rural areas 
area_names <- unique((full_data$sites %>% filter(urban_rural == rur_or_urb))$name_2)

area_names <-"Gaza"

##### RUN URBAN MODEL SIMULATION #####

if (rur_or_urb == "urban") {
  
  for (i in 1:length(area_names)){
    
    # start time simulation
    start_time <- Sys.time()
    
    ##### SET UP PARAMETERS AND INTERVENTIONS #####
    
    # current area 
    current_area_name <-area_names[i]
    
    # starting EIR
    starting_EIR <- (full_data$eir %>%
                       filter(name_2==current_area_name) %>%
                       filter(spp=="pf") %>%
                       filter(urban_rural == rur_or_urb))$eir
    
    
    # if theres no urban data for the current admin-1 area
    if (length(starting_EIR) == 0) {
      print(paste0("No ", rur_or_urb, " data for the area: ", current_area_name))
      next
    }
    
    # if the EIR is 0 
    if (starting_EIR == 0) {
      print(paste0("Starting EIR == 0 for ", rur_or_urb, " area in ", current_area_name, ". Therefore model cannot run."))
      next
    }
    
    
    # SEASONALITY
    seas_data <- full_data$seasonality %>% filter(name_2==current_area_name) 
    
    g0 <- seas_data$g0
    g <- as.double(c(seas_data$g1, seas_data$g2, seas_data$g3))
    h <- as.double(c(seas_data$h1, seas_data$h2, seas_data$h3))
    
    
    # AGE STRUCTURE 
    # initialise the age structures and seasonality in parameters
    
    simparams <- get_parameters(
      list(
        human_population = human_population,
        age_group_rendering_min_ages = age_min,
        age_group_rendering_max_ages = age_max,
        incidence_rendering_min_ages = age_min,
        incidence_rendering_max_ages = age_max,
        clinical_incidence_rendering_min_ages = age_min,
        clinical_incidence_rendering_max_ages = age_max,
        severe_incidence_rendering_min_ages = age_min,
        severe_incidence_rendering_max_ages = age_max,
        model_seasonality = TRUE,
        g0 = g0,
        g = g,
        h = h
      )
    )
    
    # SPECIES
    
    # subset foresite file to access species data by area
    species_data <- full_data$vectors %>% 
      filter(name_2 == current_area_name)
    
    # arabiensis
    if (any(species_data$species == "arabiensis") == TRUE) {
      arab_prop <- (species_data %>% filter(species == "arabiensis"))$prop
    } else {
      arab_prop <- 0
    }
    
    # funestus
    if (any(species_data$species == "funestus") == TRUE) {
      fun_prop <- (species_data %>% filter(species == "funestus"))$prop
    } else {
      fun_prop <- 0
    }
    
    # gambiae
    if (any(species_data$species == "gambiae") == TRUE) {
      gamb_prop <- (species_data %>% filter(species == "gambiae"))$prop
    } else {
      gamb_prop <- 0
    }
    
    # vector of proportions of each species 
    species_prop <- round(c(arab_prop, fun_prop, gamb_prop),digits=10)
    
    # add to parameter list
    simparams <- set_species(
      simparams,
      species = list(arab_params, fun_params, gamb_params), # must be in same order as species_prop
      proportions = species_prop
    )
    
    
    # INTERVENTION DATA
    
    # subset foresite file to get intervention data by area/classification
    intervention_data <- full_data$interventions %>%
      filter(name_2 == current_area_name) %>%
      filter(urban_rural == rur_or_urb)
    
    # subset intervention data by last n years of known data (n defined earlier)
    intervention_data <- tail(intervention_data, n=years)
    
    # assume interventions continue at the same coverage and schedule from last known data
    interv_data_proj_forwards <- intervention_data |> expand_interventions(max_year=(tail(intervention_data$year, n=1)+years_proj_forward), group_var="name_2")
    interv_data_proj_forwards <- interv_data_proj_forwards |> fill_extrapolate(group_var = "name_2")
    
    
    # BED NETS
    
    # bed nets assumed to be distributed at the end of each year
    itn_timesteps <- c(1:(sim_length/year)) * year
    itn_timesteps <- itn_timesteps[itn_timesteps <= tail(timesteps, n=1)] # remove values higher than simulation length
    
    
    simparams <- set_bednets(
      simparams,
      timesteps = itn_timesteps,
      coverages = interv_data_proj_forwards$itn_use,  # each round is distributed to x% of the population randomly
      retention = 5 * year, # Nets are kept on average 5 years
      dn0 = matrix(interv_data_proj_forwards$dn0, nrow = length(itn_timesteps), ncol = length(simparams$species)), # matrix of death probabilities for each mosquito species (here 3 species only so 3 columns) for last X years
      rn = matrix(interv_data_proj_forwards$rn0, nrow = length(itn_timesteps), ncol = length(simparams$species)), # matrix of repelling probabilities for each mosquito species (here 3 species only so 3 columns) for last X years
      rnm = matrix(interv_data_proj_forwards$rnm, nrow = length(itn_timesteps), ncol = length(simparams$species)), # matrix of minimum repelling probabilities for each mosquito species (here 3 species only so 3 columns) for last X years
      gamman = interv_data_proj_forwards$gamman # vector of bed net half-lives for each distribution timestep
    )
    
    
    # IRS
    
    # irs is assumed to be implemented at the beginning of each year
    irs_timesteps <- c(1,(1:(sim_length/year) * year + 1))
    irs_timesteps <- irs_timesteps[irs_timesteps <= tail(timesteps, n=1)] #  remove values higher than simulation length
    
    
    simparams <- set_spraying(
      simparams,
      timesteps = irs_timesteps,
      coverages = interv_data_proj_forwards$irs_cov, # NOTE: coverage is very low so has very little impact on model. tail() to access the last X number of years of data
      ls_theta = matrix(interv_data_proj_forwards$ls_theta, nrow = length(irs_timesteps), ncol = length(simparams$species)), # matrix of mortality parameters; nrows=length(timesteps), ncols=length(species)
      ls_gamma = matrix(interv_data_proj_forwards$ls_gamma, nrow = length(irs_timesteps), ncol = length(simparams$species)), # matrix of mortality parameters per round of IRS and per species
      ks_theta = matrix(interv_data_proj_forwards$ks_theta, nrow = length(irs_timesteps), ncol = length(simparams$species)), # matrix of feeding success parameters per round of IRS and per species
      ks_gamma = matrix(interv_data_proj_forwards$ks_gamma, nrow = length(irs_timesteps), ncol = length(simparams$species)), # matrix of feeding success parameters per round of IRS and per species
      ms_theta = matrix(interv_data_proj_forwards$ms_theta, nrow = length(irs_timesteps), ncol = length(simparams$species)), # matrix of deterrence parameters per round of IRS and per species
      ms_gamma = matrix(interv_data_proj_forwards$ms_gamma, nrow = length(irs_timesteps), ncol = length(simparams$species)) # matrix of deterrence parameters per round of IRS and per species
    )
    
    
    
    # PMC 
    
    # # ages at which PMC is administered
    # schedule_BEN_8doses<- c(10*7, 14*7, 6*30, 9*30, 12*30, 15*30, 18*30, 24*30)
    # 
    # # coverage of each PMC dose to target population 
    # cov_BEN_8doses<- rep(0.5, sim_length)
    # 
    # # timesteps for each round of PMC
    # testing_timesteps <- (1:sim_length)
    # 
    # # drug used
    # #SP_AQ_params<- c(1,0.32,4.3,38.1)
    # simparams <- set_drugs(simparams, list(SP_AQ_params))
    # 
    # simparams <- set_pmc(
    #   simparams,
    #   drug = 1,
    #   timesteps = testing_timesteps,
    #   coverages = cov_BEN_8doses,
    #   ages = schedule_BEN_8doses
    # )
    
    
    # TREATMENT
    
    treatment_policy <- read_xlsx("drug_policies_updated.xlsx", sheet=1)

    # most recent polcy for the country
    country_policy <- (treatment_policy %>% filter(ISO_CODE == country_code))["2020"]

    # treatment coverages (overall, ACT and non-ACT)
    overall_treatment_cov <- as.numeric(interv_data_proj_forwards$tx_cov)
    ACT_treatment_cov <- as.numeric(interv_data_proj_forwards$prop_act) * overall_treatment_cov
    non_ACT_treatment_cov <- overall_treatment_cov - ACT_treatment_cov

    # change treatment coverages at the beginning of each year
    treatment_timestep <- seq(from=1, to=years_of_simulation*year, by=year)

    # AS-AQ parameters obtained from model fitting
    AS_AQ_params <- c(0.9, 0.09434, 4.7, 16.4)

    sim_params <- set_drugs(simparams, list(AL_params, SP_AQ_params, AS_AQ_params))

    # AL listed
    if (country_policy == "AL" | country_policy == "AL+PQ") {
      sim_params <- set_clinical_treatment(
        parameters = sim_params,
        drug = 1, # AL (ACT)
        timesteps =  treatment_timestep, # Treatment coverage changes
        coverages =  ACT_treatment_cov)
    }

    # AS+AQ or AS+SP listed
    if (country_policy == "AS+AQ") {
      sim_params <- set_clinical_treatment(
        parameters = sim_params,
        drug = 3, # AS-AQ (ACT) # check this is okay since AS-AQ acts longer than AS alone
        timesteps =  treatment_timestep, # Treatment coverage changes
        coverages =  ACT_treatment_cov)
    }

    # AL and AS+AQ listed so we assume 50/50 split in coverage
    if (country_policy == "AL, AS+AQ") {
      sim_params <- set_clinical_treatment(
        parameters = sim_params,
        drug = 1, # AL
        timesteps =  treatment_timestep, # Treatment coverage changes
        coverages =  0.5*ACT_treatment_cov)
    }

    if (country_policy == "AL, AS+AQ") {
      sim_params <- set_clinical_treatment(
        parameters = sim_params,
        drug = 3, # AS+AQ
        timesteps =  treatment_timestep, # Treatment coverage changes
        coverages =  0.5*ACT_treatment_cov)
    }

    if (country_policy == "NO DATA") {
      sim_params <- set_clinical_treatment(
        parameters = sim_params,
        drug = 1, # Assume AL in absence of data
        timesteps =  treatment_timestep, # Treatment coverage changes
        coverages =  ACT_treatment_cov)
    }


    # assume AL for anything else

    if (country_policy != "AL+PQ" &
        country_policy != "AL" &
        country_policy != "AS+AQ" &
        country_policy != "AL, AS+AQ" &
        country_policy != "NO DATA") {
      sim_params <- set_clinical_treatment(
        parameters = sim_params,
        drug = 1, # Assume AL
        timesteps =  treatment_timestep, # Treatment coverage changes
        coverages =  ACT_treatment_cov)
    }


    # assume SP+AQ for non-ACT treatments
    sim_params <- set_clinical_treatment(
      parameters = sim_params,
      drug = 2, # SP-AQ (non-ACT)
      timesteps =  treatment_timestep, # Treatment coverage changes
      coverages =  non_ACT_treatment_cov)
    
    
    ##### RUN MODEL #####
    
    # adjusts IBM parameters to match equilibrium parameters
    simparams <- set_equilibrium(simparams, starting_EIR)
    
    # run simulation 
    test_sim <- run_simulation(sim_length, simparams)
    
    # assign location data to simulation results 
    test_sim$iso_code <- rep(country_code, dim(test_sim)[1])
    test_sim$rur_or_urb <- rep(rur_or_urb, dim(test_sim)[1])
    test_sim$area <- rep(current_area_name, dim(test_sim)[1])
    test_sim <- test_sim %>%
      relocate(timestep, iso_code, rur_or_urb, area)
    
    
    # store all simulation results
    sim_output_urban <- rbind(sim_output_urban, test_sim)
    
    
    ##### CREATE INCIDENCE VECTORS #####
    
    clin_inc_data_mats_urban <- c() # clinical incidence
    sev_inc_data_mats_urban <- c() # severe incidence
    tot_inc_data_mats_urban <- c() # total incidence
    asym_inc_data_mats_urban <- c() # asymptomatic incidence
    pop_size_data_mats_urban <- c() # population size 
    new_clin_inc_per_person_urban <- c() # clinical incidence (per capita)
    new_sev_inc_per_person_urban <- c() # severe incidence (per capita)
    new_tot_inc_per_person_urban <- c() # total incidence (per capita)
    new_asym_inc_per_person_urban <- c() # asymptomatic incidence (per capita)
    
    for (j in 1:length(test_sim$timestep)) {
      
      # create matrix with data for population size for each age group
      current_pop_size_data_urban <- test_sim[j, age_group_names]
      
      # create matrix with data for new infections for each age group
      current_clin_inc_data_urban <- test_sim[j, clin_inc_cols]
      current_sev_inc_data_urban <- test_sim[j, sev_inc_cols]
      current_tot_inc_data_urban <- test_sim[j, tot_inc_cols]
      current_asym_inc_data_urban <- current_tot_inc_data_urban - current_clin_inc_data_urban
      
      # populate data frame with data at each timestep 
      clin_inc_data_mats_urban<-rbind(clin_inc_data_mats_urban, current_clin_inc_data_urban)
      sev_inc_data_mats_urban<-rbind(sev_inc_data_mats_urban, current_sev_inc_data_urban)
      tot_inc_data_mats_urban<-rbind(tot_inc_data_mats_urban, current_tot_inc_data_urban)
      asym_inc_data_mats_urban<-rbind(asym_inc_data_mats_urban, current_asym_inc_data_urban)
      pop_size_data_mats_urban<-rbind(pop_size_data_mats_urban, current_pop_size_data_urban)
      
      # new infections per capita at each timestep (NaN appear is pop size = 0)
      new_clin_inc_per_person_urban <- rbind(new_clin_inc_per_person_urban, current_clin_inc_data_urban/current_pop_size_data_urban)
      new_sev_inc_per_person_urban <- rbind(new_sev_inc_per_person_urban, current_sev_inc_data_urban/current_pop_size_data_urban)
      new_tot_inc_per_person_urban <- rbind(new_tot_inc_per_person_urban, current_tot_inc_data_urban/current_pop_size_data_urban)
      new_asym_inc_per_person_urban <- rbind(new_asym_inc_per_person_urban, current_asym_inc_data_urban/current_pop_size_data_urban)
      
    }
    
    # assign location, time and infection class data to simulation results 
    colnames(clin_inc_data_mats_urban) <- age_group_names
    clin_inc_data_mats_urban$timestep <- 1:sim_length
    clin_inc_data_mats_urban$infection_class <- "clinical"
    clin_inc_data_mats_urban$iso_code <- rep(country_code, dim(clin_inc_data_mats_urban)[1])
    clin_inc_data_mats_urban$rur_or_urb <- rep(rur_or_urb, dim(clin_inc_data_mats_urban)[1])
    clin_inc_data_mats_urban$area <- rep(current_area_name, dim(clin_inc_data_mats_urban)[1])
    clin_inc_data_mats_urban <- clin_inc_data_mats_urban %>%
      relocate(timestep, iso_code, rur_or_urb, area, infection_class)
    
    colnames(sev_inc_data_mats_urban) <- age_group_names
    sev_inc_data_mats_urban$timestep <- 1:sim_length
    sev_inc_data_mats_urban$infection_class <- "severe"
    sev_inc_data_mats_urban$iso_code <- rep(country_code, dim(sev_inc_data_mats_urban)[1])
    sev_inc_data_mats_urban$rur_or_urb <- rep(rur_or_urb, dim(sev_inc_data_mats_urban)[1])
    sev_inc_data_mats_urban$area <- rep(current_area_name, dim(sev_inc_data_mats_urban)[1])
    sev_inc_data_mats_urban <- sev_inc_data_mats_urban %>%
      relocate(timestep, iso_code, rur_or_urb, area, infection_class)
    
    colnames(tot_inc_data_mats_urban) <- age_group_names
    tot_inc_data_mats_urban$timestep <- 1:sim_length
    tot_inc_data_mats_urban$infection_class <- "total"
    tot_inc_data_mats_urban$iso_code <- rep(country_code, dim(tot_inc_data_mats_urban)[1])
    tot_inc_data_mats_urban$rur_or_urb <- rep(rur_or_urb, dim(tot_inc_data_mats_urban)[1])
    tot_inc_data_mats_urban$area <- rep(current_area_name, dim(tot_inc_data_mats_urban)[1])
    tot_inc_data_mats_urban <- tot_inc_data_mats_urban %>%
      relocate(timestep, iso_code, rur_or_urb, area, infection_class)
    
    colnames(asym_inc_data_mats_urban) <- age_group_names
    asym_inc_data_mats_urban$timestep <- 1:sim_length
    asym_inc_data_mats_urban$infection_class <- "asymptomatic"
    asym_inc_data_mats_urban$iso_code <- rep(country_code, dim(asym_inc_data_mats_urban)[1])
    asym_inc_data_mats_urban$rur_or_urb <- rep(rur_or_urb, dim(asym_inc_data_mats_urban)[1])
    asym_inc_data_mats_urban$area <- rep(current_area_name, dim(asym_inc_data_mats_urban)[1])
    asym_inc_data_mats_urban <- asym_inc_data_mats_urban %>%
      relocate(timestep, iso_code, rur_or_urb, area, infection_class)
    
    colnames(pop_size_data_mats_urban) <- age_group_names
    pop_size_data_mats_urban$timestep <- 1:sim_length
    pop_size_data_mats_urban$iso_code <- rep(country_code, dim(pop_size_data_mats_urban)[1])
    pop_size_data_mats_urban$rur_or_urb <- rep(rur_or_urb, dim(pop_size_data_mats_urban)[1])
    pop_size_data_mats_urban$area <- rep(current_area_name, dim(pop_size_data_mats_urban)[1])
    pop_size_data_mats_urban <- pop_size_data_mats_urban %>%
      relocate(timestep, iso_code, rur_or_urb, area)
    
    population_df_urban <- rbind(population_df_urban, pop_size_data_mats_urban)
    
    
    # store clinical incidence results from each simulation run 
    
    # create average new clinical infections per person per year for each age group
    
    clin_inc_ppy_urban  <- data.frame()
    sev_inc_ppy_urban  <- data.frame()
    tot_inc_ppy_urban  <- data.frame()
    asym_inc_ppy_urban  <- data.frame()
    
    for (j in 1:dim(new_clin_inc_per_person_urban)[2]) {
      clin_inc_ppy_urban <- rbind(clin_inc_ppy_urban, sum(new_clin_inc_per_person_urban[,j], na.rm=TRUE)/years_of_simulation)
    }
    
    for (j in 1:dim(new_sev_inc_per_person_urban)[2]) {
      sev_inc_ppy_urban <- rbind(sev_inc_ppy_urban, sum(new_sev_inc_per_person_urban[,j], na.rm=TRUE)/years_of_simulation)
    }
    
    for (j in 1:dim(new_tot_inc_per_person_urban)[2]) {
      tot_inc_ppy_urban <- rbind(tot_inc_ppy_urban, sum(new_tot_inc_per_person_urban[,j], na.rm=TRUE)/years_of_simulation)
    }
    
    for (j in 1:dim(new_asym_inc_per_person_urban)[2]) {
      asym_inc_ppy_urban <- rbind(asym_inc_ppy_urban, sum(new_asym_inc_per_person_urban[,j], na.rm=TRUE)/years_of_simulation)
    }
    
    
    # assign location and age group data to simulation results
    
    colnames(clin_inc_ppy_urban) = "value"
    clin_inc_ppy_urban$area <- current_area_name
    clin_inc_ppy_urban$infection_class <- "clinical"
    clin_inc_ppy_urban$iso_code <- country_code
    clin_inc_ppy_urban$rur_or_urb <- rur_or_urb
    clin_inc_ppy_urban$age_group <- age_group_names 
    clin_inc_ppy_urban$age_in_days_midpoint <- age_in_days_midpoint
    clin_inc_ppy_urban$units <- "ppy" 
    clin_inc_ppy_urban <- clin_inc_ppy_urban %>%
      relocate(iso_code, area, rur_or_urb, age_group, age_in_days_midpoint, infection_class, units)
    
    colnames(sev_inc_ppy_urban) = "value"
    sev_inc_ppy_urban$area <- current_area_name
    sev_inc_ppy_urban$infection_class <- "severe"
    sev_inc_ppy_urban$iso_code <- country_code
    sev_inc_ppy_urban$rur_or_urb <- rur_or_urb
    sev_inc_ppy_urban$age_group <- age_group_names
    sev_inc_ppy_urban$age_in_days_midpoint <- age_in_days_midpoint
    sev_inc_ppy_urban$units <- "ppy" 
    sev_inc_ppy_urban <- sev_inc_ppy_urban %>%
      relocate(iso_code, area, rur_or_urb, age_group, age_in_days_midpoint, infection_class, units)
    
    colnames(tot_inc_ppy_urban) = "value"
    tot_inc_ppy_urban$area <- current_area_name
    tot_inc_ppy_urban$infection_class <- "total"
    tot_inc_ppy_urban$iso_code <- country_code
    tot_inc_ppy_urban$rur_or_urb <- rur_or_urb
    tot_inc_ppy_urban$age_group <- age_group_names
    tot_inc_ppy_urban$age_in_days_midpoint <- age_in_days_midpoint
    tot_inc_ppy_urban$units <- "ppy" 
    tot_inc_ppy_urban <- tot_inc_ppy_urban %>%
      relocate(iso_code, area, rur_or_urb, age_group, age_in_days_midpoint, infection_class, units)
    
    colnames(asym_inc_ppy_urban) = "value"
    asym_inc_ppy_urban$area <- current_area_name    
    asym_inc_ppy_urban$infection_class <- "asymptomatic"
    asym_inc_ppy_urban$iso_code <- country_code
    asym_inc_ppy_urban$rur_or_urb <- rur_or_urb
    asym_inc_ppy_urban$age_group <- age_group_names
    asym_inc_ppy_urban$age_in_days_midpoint <- age_in_days_midpoint
    asym_inc_ppy_urban$units <- "ppy" 
    asym_inc_ppy_urban <- asym_inc_ppy_urban %>%
      relocate(iso_code, area, rur_or_urb, age_group, age_in_days_midpoint, infection_class, units)
    
    
    # merge all infection class incidence dataframes together
    
    current_incidence_ppy_df_urban <- rbind(clin_inc_ppy_urban, sev_inc_ppy_urban,
                                            tot_inc_ppy_urban, asym_inc_ppy_urban)
    
    current_incidence_df_urban <- rbind(clin_inc_data_mats_urban, sev_inc_data_mats_urban,
                                        tot_inc_data_mats_urban, asym_inc_data_mats_urban)
    
    incidence_ppy_df_urban <- rbind(incidence_ppy_df_urban, current_incidence_ppy_df_urban)
    incidence_df_urban <- rbind(incidence_df_urban, current_incidence_df_urban)
    
    # end timer
    end_time <- Sys.time()
    
    # time taken for simulation
    time_elapsed <- end_time - start_time
    
    ##### PRINT UPDATES #####
    print(paste0("Successful run for the ", rur_or_urb, " region in: ", current_area_name, ". ",
                 "Time taken to run model loop: ", time_elapsed))
    
    
  }
  
  
  
  ##### store simulation data in CSV format #####
  
  # all simulation data
  write.csv(sim_output_urban, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/sim_output_urban.csv"), row.names=FALSE)
  
  # average clinical incidence per person per year for each site
  write.csv(incidence_ppy_df_urban, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/incidence_ppy_df_urban.csv"), row.names=FALSE)
  
  # number of clinical infections in each age group at each timestep, for each site
  write.csv(incidence_df_urban, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/incidence_df_urban.csv"), row.names=FALSE)
  
  # population sizes in each age group at each timestep, for each site 
  write.csv(population_df_urban, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/population_df_urban.csv"), row.names=FALSE)
  
  
  
}


