
##### READ IN IMPERIAL MODEL OUTPUT #####

# rural output 
incidence_ppy_df_rural <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/incidence_ppy_df_rural.csv"))
population_df_rural <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/population_df_rural.csv"))

# urban output
incidence_ppy_df_urban <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/incidence_ppy_df_urban.csv"))
population_df_urban <- read.csv(paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/population_df_urban.csv"))

##### READ IN PROPORTIONS OF HAPLOTYPES #####
source("HAPLOTYPE_PROPORTIONS.R")

# reset area names to those that have haplotype data 
area_names <- unique((haplotype_proportions %>% filter(iso_code == country_code))$NAME_2)

##### EPI VACCINE COVERAGE DATA #####

coverage_data <- read_xlsx("plusproject_countries_coverage_dhs.xlsx", sheet = 2)

# coverage data for given country
cov <- coverage_data %>% filter(country == country_code, schedule == 4)
cov_additional_doses <- coverage_data %>% filter(country == country_code, (schedule == 8 | schedule == 4))

# vaccine schedule for given country
schedule <- unique((coverage_data %>% filter(country == country_code, schedule == 4))$vaccine)
schedule_additional_doses <- unique((coverage_data %>% filter(country == country_code, (schedule == 8 | schedule == 4)))$vaccine)


vaccines <- c() # vaccines given 
vaccine_weeks <- c() # target age for each vaccine
vaccine_cov_rural <- vaccine_cov_urban <- c() # coverage of vaccine by rurality

coverage_df_additional_schedule <- c()

for (i in 1:length(area_names)) {
  
  # same for each rural and urban
  vaccines <- unique((cov_additional_doses %>% filter(region == area_names[i]))$vaccine)
  vaccine_weeks <- as.numeric(unique((cov_additional_doses %>% filter(region == area_names[i]))$vaccine_weeks))
  

  # coverage of vaccine by rurality
  
  # assume 90% of children to uptake co-delivered vaccine will uptake PMC dose 
  
  if (length((cov_additional_doses %>% filter(region == area_names[i], rural_urban == "rural"))$cov) == 0) {
    vaccine_cov_rural <- as.numeric(rep(0, times=length(unique(vaccine_weeks))))
  } else {
    vaccine_cov_rural <- 0.9*as.numeric((cov_additional_doses %>% filter(region == area_names[i], rural_urban == "rural"))$cov)
  }
  
  if (length((cov_additional_doses %>% filter(region == area_names[i], rural_urban == "urban"))$cov) == 0) {
    vaccine_cov_urban <- as.numeric(rep(0, times=length(unique(vaccine_weeks))))
  } else {
    vaccine_cov_urban <- 0.9*as.numeric((cov_additional_doses %>% filter(region == area_names[i], rural_urban == "urban"))$cov)
  }
  
  for (j in 1:length(unique(vaccine_weeks))) {
    coverage_df_additional_schedule <- rbind(coverage_df_additional_schedule, c(area_names[i], vaccines[j], as.numeric(vaccine_weeks[j]), as.numeric(vaccine_cov_rural[j]), as.numeric(vaccine_cov_urban[j])))
  }
  
}

colnames(coverage_df_additional_schedule) <- c("area", "vaccine", "vaccine_weeks", "cov_rural", "cov_urban")
coverage_df_additional_schedule <- as.data.frame(coverage_df_additional_schedule)
coverage_df_additional_schedule$vaccine_weeks <- as.numeric(coverage_df_additional_schedule$vaccine_weeks)
coverage_df_additional_schedule$vaccine_days <- 7 * coverage_df_additional_schedule$vaccine_weeks 


# additional EPI vaccine coverage dataframe
coverage_df <- coverage_df_additional_schedule %>% filter(vaccine %in% schedule)
coverage_df$vaccine_days <- 7 *coverage_df$vaccine_weeks 



##### SET UP EFFICACY CURVES #####

# weibull scale parameters for each haplotype
lambda_trip<-59.57659
lambda_quadr<-33.05391
lambda_quint<-18.55328
lambda_sext<-12.81186
lambda_other<-23
lambda_VAGKAA<-18.55328 # assumed to be same as the QUINTUPLE
lambda_VAGKGS<-12.81186 # assumed to be same as the SEXTUPLE

# weibull shape parameters for each haplotype
w_trip<- 8.435971
w_quadr<-4.862126
w_quint<-2.4840752
w_sext<-3.691953
w_other<-4.5
w_VAGKAA<-1.7
w_VAGKGS<- 4.1

##### RURAL PMC IMPACT #####

# store rural PMC impact data for each admin-1 area
list_of_df_rural <- vector(mode = "list", length(area_names))
PMC_impact_ppy_rural <- data.frame()

# calculate incidence when PMC is given for all rural areas in country 
for (i in 1:length(area_names)) {
  
  # vaccine schedule in days
  schedule <- 7 * as.numeric((coverage_df %>% filter(area == area_names[i]))$vaccine_weeks)
  
  # rural vaccine coverage 
  cov_rural <- as.numeric((coverage_df %>% filter(area == area_names[i]))$cov_rural)
  
  # Get the order of indices that would sort schedule_additional_doses
  sorted_indices <- order(schedule)
  
  # Use sorted_indices to reorder both vectors
  sorted_schedule <- schedule[sorted_indices]
  sorted_cov_rural <- cov_rural[sorted_indices]
  
  schedule <- sorted_schedule
  cov_rural <- sorted_cov_rural

  
  # set up df with the timesteps 
  if (sim_length > length(age_in_days_midpoint)) {
    df_rural<- data.frame(time=1:sim_length)
  }
  
  if (sim_length < length(age_in_days_midpoint)) {
    df_rural<- data.frame(time=1:length(age_in_days_midpoint))
  }
  
  # initalise empty columns
  df_rural$prot_trip<-NA
  df_rural$prot_quadr<-NA
  df_rural$prot_quint<-NA
  df_rural$prot_sext<-NA
  df_rural$prot_other<-NA
  df_rural$prot_VAGKAA<-NA
  df_rural$prot_VAGKGS<-NA
  
  
  # construct weibull curves to model the efficacy of SP through time for 
  # each haplotype 
  
  if (length(schedule) > 0) {
    for (t in 1: schedule[1]) {  # day 0 to dose 1 on day 70 
      df_rural$prot_trip[t]<-0
      df_rural$prot_quadr[t]<-0
      df_rural$prot_quint[t]<-0
      df_rural$prot_sext[t]<- 0
      df_rural$prot_other[t]<-0
      df_rural$prot_VAGKAA[t]<- 0
      df_rural$prot_VAGKGS[t]<- 0  
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_rural))  {  
      df_rural$prot_trip[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_rural[length(cov_rural)]
      df_rural$prot_quadr[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_rural[length(cov_rural)]
      df_rural$prot_quint[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_rural[length(cov_rural)]
      df_rural$prot_sext[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_rural[length(cov_rural)]
      df_rural$prot_other[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKAA[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKGS[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_rural[length(cov_rural)]
    }    
  }
  
  if (length(schedule) > 1) {
    for (t in (schedule[1]+1) : schedule[2])  {   # day 71 to dose 2 on day 98
      df_rural$prot_trip[t]<- exp(-(df_rural$time[t-schedule[1]]/lambda_trip)^w_trip) * cov_rural[1]
      df_rural$prot_quadr[t]<- exp(-(df_rural$time[t-schedule[1]]/lambda_quadr)^w_quadr)* cov_rural[1]
      df_rural$prot_quint[t]<- exp(-(df_rural$time[t-schedule[1]]/lambda_quint)^w_quint)* cov_rural[1]
      df_rural$prot_sext[t]<- exp(-(df_rural$time[t-schedule[1]]/lambda_sext)^w_sext)* cov_rural[1]
      df_rural$prot_other[t]<- exp(-(df_rural$time[t-schedule[1]]/lambda_other)^w_other)* cov_rural[1]
      df_rural$prot_VAGKAA[t]<- exp(-(df_rural$time[t-schedule[1]]/lambda_VAGKAA)^w_VAGKAA)* cov_rural[1]
      df_rural$prot_VAGKGS[t]<- exp(-(df_rural$time[t-schedule[1]]/lambda_VAGKGS)^w_VAGKGS)* cov_rural[1]
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_rural))  {  
      df_rural$prot_trip[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_rural[length(cov_rural)]
      df_rural$prot_quadr[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_rural[length(cov_rural)]
      df_rural$prot_quint[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_rural[length(cov_rural)]
      df_rural$prot_sext[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_rural[length(cov_rural)]
      df_rural$prot_other[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKAA[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKGS[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_rural[length(cov_rural)]
    }    
  } 
  
  if (length(schedule) > 2) {
    for (t in (schedule[2]+1) : schedule[3])  {  #  day 99 to dose 3 on day 180
      df_rural$prot_trip[t]<- exp(-(df_rural$time[t-schedule[2]]/lambda_trip)^w_trip)* cov_rural[2]
      df_rural$prot_quadr[t]<- exp(-(df_rural$time[t-schedule[2]]/lambda_quadr)^w_quadr)* cov_rural[2]
      df_rural$prot_quint[t]<- exp(-(df_rural$time[t-schedule[2]]/lambda_quint)^w_quint)* cov_rural[2]
      df_rural$prot_sext[t]<- exp(-(df_rural$time[t-schedule[2]]/lambda_sext)^w_sext)* cov_rural[2]
      df_rural$prot_other[t]<- exp(-(df_rural$time[t-schedule[2]]/lambda_other)^w_other)* cov_rural[2]
      df_rural$prot_VAGKAA[t]<- exp(-(df_rural$time[t-schedule[2]]/lambda_VAGKAA)^w_VAGKAA)* cov_rural[2]
      df_rural$prot_VAGKGS[t]<- exp(-(df_rural$time[t-schedule[2]]/lambda_VAGKGS)^w_VAGKGS)* cov_rural[2]
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_rural))  {  
      df_rural$prot_trip[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_rural[length(cov_rural)]
      df_rural$prot_quadr[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_rural[length(cov_rural)]
      df_rural$prot_quint[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_rural[length(cov_rural)]
      df_rural$prot_sext[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_rural[length(cov_rural)]
      df_rural$prot_other[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKAA[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKGS[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_rural[length(cov_rural)]
    }    
  }
  
  if (length(schedule) > 3) {
    for (t in (schedule[3]+1) : schedule[4])  {  # day 181 to dose 4 on day 270
      df_rural$prot_trip[t]<- exp(-(df_rural$time[t-schedule[3]]/lambda_trip)^w_trip)* cov_rural[3]
      df_rural$prot_quadr[t]<- exp(-(df_rural$time[t-schedule[3]]/lambda_quadr)^w_quadr)* cov_rural[3]
      df_rural$prot_quint[t]<- exp(-(df_rural$time[t-schedule[3]]/lambda_quint)^w_quint)* cov_rural[3]
      df_rural$prot_sext[t]<- exp(-(df_rural$time[t-schedule[3]]/lambda_sext)^w_sext)* cov_rural[3]
      df_rural$prot_other[t]<- exp(-(df_rural$time[t-schedule[3]]/lambda_other)^w_other)* cov_rural[3]
      df_rural$prot_VAGKAA[t]<- exp(-(df_rural$time[t-schedule[3]]/lambda_VAGKAA)^w_VAGKAA)* cov_rural[3]
      df_rural$prot_VAGKGS[t]<- exp(-(df_rural$time[t-schedule[3]]/lambda_VAGKGS)^w_VAGKGS)* cov_rural[3]
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_rural))  {  
      df_rural$prot_trip[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_rural[length(cov_rural)]
      df_rural$prot_quadr[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_rural[length(cov_rural)]
      df_rural$prot_quint[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_rural[length(cov_rural)]
      df_rural$prot_sext[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_rural[length(cov_rural)]
      df_rural$prot_other[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKAA[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKGS[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_rural[length(cov_rural)]
    }    
  }
  
  if (length(schedule) > 4) {
    for (t in (schedule[4]+1) : schedule[5])  {  # day 271 to dose 5 on day 360 
      df_rural$prot_trip[t]<- exp(-(df_rural$time[t-schedule[4]]/lambda_trip)^w_trip)* cov_rural[4]
      df_rural$prot_quadr[t]<- exp(-(df_rural$time[t-schedule[4]]/lambda_quadr)^w_quadr)* cov_rural[4]
      df_rural$prot_quint[t]<- exp(-(df_rural$time[t-schedule[4]]/lambda_quint)^w_quint)* cov_rural[4]
      df_rural$prot_sext[t]<- exp(-(df_rural$time[t-schedule[4]]/lambda_sext)^w_sext)* cov_rural[4]
      df_rural$prot_other[t]<- exp(-(df_rural$time[t-schedule[4]]/lambda_other)^w_other)* cov_rural[4]
      df_rural$prot_VAGKAA[t]<- exp(-(df_rural$time[t-schedule[4]]/lambda_VAGKAA)^w_VAGKAA)* cov_rural[4]
      df_rural$prot_VAGKGS[t]<- exp(-(df_rural$time[t-schedule[4]]/lambda_VAGKGS)^w_VAGKGS)* cov_rural[4]
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_rural))  {  
      df_rural$prot_trip[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_rural[length(cov_rural)]
      df_rural$prot_quadr[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_rural[length(cov_rural)]
      df_rural$prot_quint[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_rural[length(cov_rural)]
      df_rural$prot_sext[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_rural[length(cov_rural)]
      df_rural$prot_other[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKAA[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKGS[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_rural[length(cov_rural)]
    }    
  }
  
  if (length(schedule) > 5) {
    for (t in (schedule[5]+1) : schedule[6])  {  # day 361 to dose 6 on day 450 
      df_rural$prot_trip[t]<- exp(-(df_rural$time[t-schedule[5]]/lambda_trip)^w_trip)* cov_rural[5]
      df_rural$prot_quadr[t]<- exp(-(df_rural$time[t-schedule[5]]/lambda_quadr)^w_quadr)* cov_rural[5]
      df_rural$prot_quint[t]<- exp(-(df_rural$time[t-schedule[5]]/lambda_quint)^w_quint)* cov_rural[5]
      df_rural$prot_sext[t]<- exp(-(df_rural$time[t-schedule[5]]/lambda_sext)^w_sext)* cov_rural[5]
      df_rural$prot_other[t]<- exp(-(df_rural$time[t-schedule[5]]/lambda_other)^w_other)* cov_rural[5]
      df_rural$prot_VAGKAA[t]<- exp(-(df_rural$time[t-schedule[5]]/lambda_VAGKAA)^w_VAGKAA)* cov_rural[5]
      df_rural$prot_VAGKGS[t]<- exp(-(df_rural$time[t-schedule[5]]/lambda_VAGKGS)^w_VAGKGS)* cov_rural[5]
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_rural))  {  
      df_rural$prot_trip[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_rural[length(cov_rural)]
      df_rural$prot_quadr[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_rural[length(cov_rural)]
      df_rural$prot_quint[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_rural[length(cov_rural)]
      df_rural$prot_sext[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_rural[length(cov_rural)]
      df_rural$prot_other[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKAA[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKGS[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_rural[length(cov_rural)]
    }    
  }
  
  if (length(schedule) > 6) {
    for (t in (schedule[6]+1) : schedule[7])  {  # day 451 to dose 7 on day 540 
      df_rural$prot_trip[t]<- exp(-(df_rural$time[t-schedule[6]]/lambda_trip)^w_trip)* cov_rural[6]
      df_rural$prot_quadr[t]<- exp(-(df_rural$time[t-schedule[6]]/lambda_quadr)^w_quadr)* cov_rural[6]
      df_rural$prot_quint[t]<- exp(-(df_rural$time[t-schedule[6]]/lambda_quint)^w_quint)* cov_rural[6]
      df_rural$prot_sext[t]<- exp(-(df_rural$time[t-schedule[6]]/lambda_sext)^w_sext)* cov_rural[6]
      df_rural$prot_other[t]<- exp(-(df_rural$time[t-schedule[6]]/lambda_other)^w_other)* cov_rural[6]
      df_rural$prot_VAGKAA[t]<- exp(-(df_rural$time[t-schedule[6]]/lambda_VAGKAA)^w_VAGKAA)* cov_rural[6]
      df_rural$prot_VAGKGS[t]<- exp(-(df_rural$time[t-schedule[6]]/lambda_VAGKGS)^w_VAGKGS)* cov_rural[6]
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_rural))  {  
      df_rural$prot_trip[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_rural[length(cov_rural)]
      df_rural$prot_quadr[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_rural[length(cov_rural)]
      df_rural$prot_quint[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_rural[length(cov_rural)]
      df_rural$prot_sext[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_rural[length(cov_rural)]
      df_rural$prot_other[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKAA[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKGS[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_rural[length(cov_rural)]
    }    
  }
  
  if (length(schedule) > 7) {
    for (t in (schedule[7]+1) : schedule[8])  {  # day 541 to dose 8 on day 720
      df_rural$prot_trip[t]<- exp(-(df_rural$time[t-schedule[7]]/lambda_trip)^w_trip)* cov_rural[7]
      df_rural$prot_quadr[t]<- exp(-(df_rural$time[t-schedule[7]]/lambda_quadr)^w_quadr)* cov_rural[7]
      df_rural$prot_quint[t]<- exp(-(df_rural$time[t-schedule[7]]/lambda_quint)^w_quint)* cov_rural[7]
      df_rural$prot_sext[t]<- exp(-(df_rural$time[t-schedule[7]]/lambda_sext)^w_sext)* cov_rural[7]
      df_rural$prot_other[t]<- exp(-(df_rural$time[t-schedule[7]]/lambda_other)^w_other)* cov_rural[7]
      df_rural$prot_VAGKAA[t]<- exp(-(df_rural$time[t-schedule[7]]/lambda_VAGKAA)^w_VAGKAA)* cov_rural[7]
      df_rural$prot_VAGKGS[t]<- exp(-(df_rural$time[t-schedule[7]]/lambda_VAGKGS)^w_VAGKGS)* cov_rural[7]
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_rural))  {  
      df_rural$prot_trip[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_rural[length(cov_rural)]
      df_rural$prot_quadr[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_rural[length(cov_rural)]
      df_rural$prot_quint[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_rural[length(cov_rural)]
      df_rural$prot_sext[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_rural[length(cov_rural)]
      df_rural$prot_other[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKAA[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKGS[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_rural[length(cov_rural)]
    }    
  }
  
  if (length(schedule) > 8) {
    for (t in (schedule[8]+1) : nrow(df_rural))  {  # day 721 to end of simulation
      df_rural$prot_trip[t]<- exp(-(df_rural$time[t-schedule[8]]/lambda_trip)^w_trip)* cov_rural[8]
      df_rural$prot_quadr[t]<- exp(-(df_rural$time[t-schedule[8]]/lambda_quadr)^w_quadr)* cov_rural[8]
      df_rural$prot_quint[t]<- exp(-(df_rural$time[t-schedule[8]]/lambda_quint)^w_quint)* cov_rural[8]
      df_rural$prot_sext[t]<- exp(-(df_rural$time[t-schedule[8]]/lambda_sext)^w_sext)* cov_rural[8]
      df_rural$prot_other[t]<- exp(-(df_rural$time[t-schedule[8]]/lambda_other)^w_other)* cov_rural[8]
      df_rural$prot_VAGKAA[t]<- exp(-(df_rural$time[t-schedule[8]]/lambda_VAGKAA)^w_VAGKAA)* cov_rural[8]
      df_rural$prot_VAGKGS[t]<- exp(-(df_rural$time[t-schedule[8]]/lambda_VAGKGS)^w_VAGKGS)* cov_rural[8]
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_rural))  {  
      df_rural$prot_trip[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_rural[length(cov_rural)]
      df_rural$prot_quadr[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_rural[length(cov_rural)]
      df_rural$prot_quint[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_rural[length(cov_rural)]
      df_rural$prot_sext[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_rural[length(cov_rural)]
      df_rural$prot_other[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKAA[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_rural[length(cov_rural)]
      df_rural$prot_VAGKGS[t]<- exp(-(df_rural$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_rural[length(cov_rural)]
    }    
  }
  
  # proportions of each haplotype for current admin-1 area 
  proportions <- haplotype_proportions %>% filter(iso_code == country_code, NAME_2 == area_names[i])
  
  
  # overall protection for current admin-1 area
  df_rural[paste0("prot_overall_", area_names[i])] <- as.double(proportions$I_AKA_)*df_rural$prot_trip +
    as.double(proportions$I_GKA_)*df_rural$prot_quadr +
    as.double(proportions$I_GEA_)*df_rural$prot_quint +
    as.double(proportions$I_GEG_)*df_rural$prot_sext +
    as.double(proportions$V_GKA_)*df_rural$prot_VAGKAA +
    as.double(proportions$V_GKG_)*df_rural$prot_VAGKGS +
    as.double(proportions$other)*df_rural$prot_other
  
  
  # save dataframe for current area
  list_of_df_rural[[i]] <- df_rural 
  
  # PMC impact on incidence (ppy)
  
  # replicate incidence (without PMC) dataframe
  new_PMC_impact_ppy_df_rural <- (incidence_ppy_df_rural %>% filter(area == area_names[i]))   
  
  
  # replace "value" column with new value when PMC is given 
  new_PMC_impact_ppy_df_rural$value <- as.double(new_PMC_impact_ppy_df_rural$value) * rep((1-df_rural[paste0("prot_overall_", area_names[i])][min(age_min):max(age_max),]),4) 
  
  PMC_impact_ppy_rural <- rbind(PMC_impact_ppy_rural, new_PMC_impact_ppy_df_rural)
  
}

names(list_of_df_rural) <- area_names

##### RURAL PMC IMPACT (additional PMC SCHEDULE) #####


# store rural PMC impact data for each admin-1 area
list_of_df_rural_additional <- vector(mode = "list", length(area_names))
PMC_impact_ppy_rural_additional <- data.frame()


# calculate incidence when PMC is given for all rural areas in country 
for (i in 1:length(area_names)) {
  
  # additional vaccine schedule in days
  schedule_additional_doses <- 7 * as.numeric((coverage_df_additional_schedule %>% filter(area == area_names[i]))$vaccine_weeks)
  
  # rural vaccine coverage 
  cov_additional_doses_rural <- as.numeric((coverage_df_additional_schedule %>% filter(area == area_names[i]))$cov_rural)
  
  # Get the order of indices that would sort schedule_additional_doses
  sorted_indices <- order(schedule_additional_doses)
  
  # Use sorted_indices to reorder both vectors
  sorted_schedule <- schedule_additional_doses[sorted_indices]
  sorted_cov_rural <- cov_additional_doses_rural[sorted_indices]
  
  schedule_additional_doses <- sorted_schedule
  cov_additional_doses_rural <- sorted_cov_rural
  
  # set up df with the timesteps 
  if (sim_length > length(age_in_days_midpoint)) {
    df_rural_additional<- data.frame(time=1:sim_length)
  }
  
  if (sim_length < length(age_in_days_midpoint)) {
    df_rural_additional<- data.frame(time=1:length(age_in_days_midpoint))
  }
  
  # initalise empty columns
  df_rural_additional$prot_trip<-NA
  df_rural_additional$prot_quadr<-NA
  df_rural_additional$prot_quint<-NA
  df_rural_additional$prot_sext<-NA
  df_rural_additional$prot_other<-NA
  df_rural_additional$prot_VAGKAA<-NA
  df_rural_additional$prot_VAGKGS<-NA
  
  
  # construct weibull curves to model the efficacy of SP through time for 
  # each haplotype 
  
  if (length(schedule_additional_doses) > 0) {
    for (t in 1: schedule_additional_doses[1]) {  # day 0 to dose 1 on day 70 
      df_rural_additional$prot_trip[t]<-0
      df_rural_additional$prot_quadr[t]<-0
      df_rural_additional$prot_quint[t]<-0
      df_rural_additional$prot_sext[t]<- 0
      df_rural_additional$prot_other[t]<-0
      df_rural_additional$prot_VAGKAA[t]<- 0
      df_rural_additional$prot_VAGKGS[t]<- 0  
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_rural_additional))  {  
      df_rural_additional$prot_trip[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quadr[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quint[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_sext[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_other[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKAA[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKGS[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
    }    
  }
  
  if (length(schedule_additional_doses) > 1) {
    for (t in (schedule_additional_doses[1]+1) : schedule_additional_doses[2])  {   # day 71 to dose 2 on day 98
      df_rural_additional$prot_trip[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[1]]/lambda_trip)^w_trip) * cov_additional_doses_rural[1]
      df_rural_additional$prot_quadr[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[1]]/lambda_quadr)^w_quadr)* cov_additional_doses_rural[1]
      df_rural_additional$prot_quint[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[1]]/lambda_quint)^w_quint)* cov_additional_doses_rural[1]
      df_rural_additional$prot_sext[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[1]]/lambda_sext)^w_sext)* cov_additional_doses_rural[1]
      df_rural_additional$prot_other[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[1]]/lambda_other)^w_other)* cov_additional_doses_rural[1]
      df_rural_additional$prot_VAGKAA[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[1]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_rural[1]
      df_rural_additional$prot_VAGKGS[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[1]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_rural[1]
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_rural_additional))  {  
      df_rural_additional$prot_trip[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quadr[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quint[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_sext[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_other[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKAA[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKGS[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
    }    
  } 
  
  if (length(schedule_additional_doses) > 2) {
    for (t in (schedule_additional_doses[2]+1) : schedule_additional_doses[3])  {  #  day 99 to dose 3 on day 180
      df_rural_additional$prot_trip[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[2]]/lambda_trip)^w_trip)* cov_additional_doses_rural[2]
      df_rural_additional$prot_quadr[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[2]]/lambda_quadr)^w_quadr)* cov_additional_doses_rural[2]
      df_rural_additional$prot_quint[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[2]]/lambda_quint)^w_quint)* cov_additional_doses_rural[2]
      df_rural_additional$prot_sext[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[2]]/lambda_sext)^w_sext)* cov_additional_doses_rural[2]
      df_rural_additional$prot_other[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[2]]/lambda_other)^w_other)* cov_additional_doses_rural[2]
      df_rural_additional$prot_VAGKAA[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[2]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_rural[2]
      df_rural_additional$prot_VAGKGS[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[2]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_rural[2]
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_rural_additional))  {  
      df_rural_additional$prot_trip[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quadr[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quint[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_sext[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_other[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKAA[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKGS[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
    }    
  }
  
  if (length(schedule_additional_doses) > 3) {
    for (t in (schedule_additional_doses[3]+1) : schedule_additional_doses[4])  {  # day 181 to dose 4 on day 270
      df_rural_additional$prot_trip[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[3]]/lambda_trip)^w_trip)* cov_additional_doses_rural[3]
      df_rural_additional$prot_quadr[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[3]]/lambda_quadr)^w_quadr)* cov_additional_doses_rural[3]
      df_rural_additional$prot_quint[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[3]]/lambda_quint)^w_quint)* cov_additional_doses_rural[3]
      df_rural_additional$prot_sext[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[3]]/lambda_sext)^w_sext)* cov_additional_doses_rural[3]
      df_rural_additional$prot_other[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[3]]/lambda_other)^w_other)* cov_additional_doses_rural[3]
      df_rural_additional$prot_VAGKAA[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[3]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_rural[3]
      df_rural_additional$prot_VAGKGS[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[3]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_rural[3]
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_rural_additional))  {  
      df_rural_additional$prot_trip[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quadr[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quint[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_sext[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_other[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKAA[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKGS[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
    }    
  }
  
  if (length(schedule_additional_doses) > 4) {
    for (t in (schedule_additional_doses[4]+1) : schedule_additional_doses[5])  {  # day 271 to dose 5 on day 360 
      df_rural_additional$prot_trip[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[4]]/lambda_trip)^w_trip)* cov_additional_doses_rural[4]
      df_rural_additional$prot_quadr[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[4]]/lambda_quadr)^w_quadr)* cov_additional_doses_rural[4]
      df_rural_additional$prot_quint[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[4]]/lambda_quint)^w_quint)* cov_additional_doses_rural[4]
      df_rural_additional$prot_sext[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[4]]/lambda_sext)^w_sext)* cov_additional_doses_rural[4]
      df_rural_additional$prot_other[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[4]]/lambda_other)^w_other)* cov_additional_doses_rural[4]
      df_rural_additional$prot_VAGKAA[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[4]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_rural[4]
      df_rural_additional$prot_VAGKGS[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[4]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_rural[4]
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_rural_additional))  {  
      df_rural_additional$prot_trip[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quadr[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quint[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_sext[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_other[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKAA[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKGS[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
    }    
  }
  
  if (length(schedule_additional_doses) > 5) {
    for (t in (schedule_additional_doses[5]+1) : schedule_additional_doses[6])  {  # day 361 to dose 6 on day 450 
      df_rural_additional$prot_trip[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[5]]/lambda_trip)^w_trip)* cov_additional_doses_rural[5]
      df_rural_additional$prot_quadr[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[5]]/lambda_quadr)^w_quadr)* cov_additional_doses_rural[5]
      df_rural_additional$prot_quint[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[5]]/lambda_quint)^w_quint)* cov_additional_doses_rural[5]
      df_rural_additional$prot_sext[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[5]]/lambda_sext)^w_sext)* cov_additional_doses_rural[5]
      df_rural_additional$prot_other[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[5]]/lambda_other)^w_other)* cov_additional_doses_rural[5]
      df_rural_additional$prot_VAGKAA[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[5]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_rural[5]
      df_rural_additional$prot_VAGKGS[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[5]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_rural[5]
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_rural_additional))  {  
      df_rural_additional$prot_trip[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quadr[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quint[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_sext[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_other[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKAA[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKGS[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
    }    
  }
  
  if (length(schedule_additional_doses) > 6) {
    for (t in (schedule_additional_doses[6]+1) : schedule_additional_doses[7])  {  # day 451 to dose 7 on day 540 
      df_rural_additional$prot_trip[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[6]]/lambda_trip)^w_trip)* cov_additional_doses_rural[6]
      df_rural_additional$prot_quadr[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[6]]/lambda_quadr)^w_quadr)* cov_additional_doses_rural[6]
      df_rural_additional$prot_quint[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[6]]/lambda_quint)^w_quint)* cov_additional_doses_rural[6]
      df_rural_additional$prot_sext[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[6]]/lambda_sext)^w_sext)* cov_additional_doses_rural[6]
      df_rural_additional$prot_other[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[6]]/lambda_other)^w_other)* cov_additional_doses_rural[6]
      df_rural_additional$prot_VAGKAA[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[6]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_rural[6]
      df_rural_additional$prot_VAGKGS[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[6]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_rural[6]
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_rural_additional))  {  
      df_rural_additional$prot_trip[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quadr[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quint[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_sext[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_other[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKAA[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKGS[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
    }    
  }
  
  if (length(schedule_additional_doses) > 7) {
    for (t in (schedule_additional_doses[7]+1) : schedule_additional_doses[8])  {  # day 541 to dose 8 on day 720
      df_rural_additional$prot_trip[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[7]]/lambda_trip)^w_trip)* cov_additional_doses_rural[7]
      df_rural_additional$prot_quadr[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[7]]/lambda_quadr)^w_quadr)* cov_additional_doses_rural[7]
      df_rural_additional$prot_quint[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[7]]/lambda_quint)^w_quint)* cov_additional_doses_rural[7]
      df_rural_additional$prot_sext[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[7]]/lambda_sext)^w_sext)* cov_additional_doses_rural[7]
      df_rural_additional$prot_other[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[7]]/lambda_other)^w_other)* cov_additional_doses_rural[7]
      df_rural_additional$prot_VAGKAA[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[7]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_rural[7]
      df_rural_additional$prot_VAGKGS[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[7]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_rural[7]
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_rural_additional))  {  
      df_rural_additional$prot_trip[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quadr[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quint[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_sext[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_other[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKAA[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKGS[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
    }    
  }
  
  if (length(schedule_additional_doses) > 8) {
    for (t in (schedule_additional_doses[8]+1) : nrow(df_rural_additional))  {  # day 721 to end of simulation
      df_rural_additional$prot_trip[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[8]]/lambda_trip)^w_trip)* cov_additional_doses_rural[8]
      df_rural_additional$prot_quadr[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[8]]/lambda_quadr)^w_quadr)* cov_additional_doses_rural[8]
      df_rural_additional$prot_quint[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[8]]/lambda_quint)^w_quint)* cov_additional_doses_rural[8]
      df_rural_additional$prot_sext[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[8]]/lambda_sext)^w_sext)* cov_additional_doses_rural[8]
      df_rural_additional$prot_other[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[8]]/lambda_other)^w_other)* cov_additional_doses_rural[8]
      df_rural_additional$prot_VAGKAA[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[8]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_rural[8]
      df_rural_additional$prot_VAGKGS[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[8]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_rural[8]
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_rural_additional))  {  
      df_rural_additional$prot_trip[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quadr[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_quint[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_sext[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_other[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKAA[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
      df_rural_additional$prot_VAGKGS[t]<- exp(-(df_rural_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_rural[length(cov_additional_doses_rural)]
    }    
  }
  
  # proportions of each haplotype for current admin-1 area 
  proportions <- haplotype_proportions %>% filter(iso_code == country_code, NAME_2 == area_names[i])
  
  # overall protection for current admin-1 area
  df_rural_additional[paste0("prot_overall_", area_names[i])] <- as.double(proportions$I_AKA_)*df_rural_additional$prot_trip +
    as.double(proportions$I_GKA_)*df_rural_additional$prot_quadr +
    as.double(proportions$I_GEA_)*df_rural_additional$prot_quint +
    as.double(proportions$I_GEG_)*df_rural_additional$prot_sext +
    as.double(proportions$V_GKA_)*df_rural_additional$prot_VAGKAA +
    as.double(proportions$V_GKG_)*df_rural_additional$prot_VAGKGS +
    as.double(proportions$other)*df_rural_additional$prot_other
  
  
  # save dataframe for current area
  list_of_df_rural_additional[[i]] <- df_rural_additional 
  
  # PMC impact on incidence (ppy)
  
  # replicate incidence (without PMC) dataframe
  new_PMC_impact_ppy_df_rural_additional <- (incidence_ppy_df_rural %>% filter(area == area_names[i]))   
  
  
  # replace "value" column with new value when PMC is given 
  new_PMC_impact_ppy_df_rural_additional$value <- as.double(new_PMC_impact_ppy_df_rural_additional$value) * rep((1-df_rural_additional[paste0("prot_overall_", area_names[i])][min(age_min):max(age_max),]),4) 
  
  PMC_impact_ppy_rural_additional <- rbind(PMC_impact_ppy_rural_additional, new_PMC_impact_ppy_df_rural_additional)
  
}

names(list_of_df_rural_additional) <- area_names




##### URBAN PMC IMPACT #####

list_of_df_urban <- vector(mode = "list", length(area_names))


PMC_impact_ppy_urban <- data.frame()

for (i in 1:length(area_names)) {
  
  # vaccine schedule in days
  schedule <- 7 * as.numeric((coverage_df %>% filter(area == area_names[i]))$vaccine_weeks)
  
  # urban vaccine coverage 
  cov_urban <- as.numeric((coverage_df %>% filter(area == area_names[i]))$cov_urban)

  # Get the order of indices that would sort schedule_additional_doses
  sorted_indices <- order(schedule)
  
  # Use sorted_indices to reorder both vectors
  sorted_schedule <- schedule[sorted_indices]
  sorted_cov_urban <- cov_urban[sorted_indices]
  
  schedule <- sorted_schedule
  cov_urban <- sorted_cov_urban
  
  
  # set up df with the timesteps 
  if (sim_length > length(age_in_days_midpoint)) {
    df_urban<- data.frame(time=1:sim_length)
  }
  
  if (sim_length < length(age_in_days_midpoint)) {
    df_urban<- data.frame(time=1:length(age_in_days_midpoint))
  }
  
  # initalise empty columns
  df_urban$prot_trip<-NA
  df_urban$prot_quadr<-NA
  df_urban$prot_quint<-NA
  df_urban$prot_sext<-NA
  df_urban$prot_other<-NA
  df_urban$prot_VAGKAA<-NA
  df_urban$prot_VAGKGS<-NA
  
  
  # construct weibull curves to model the efficacy of SP through time for 
  # each haplotype 
  
  if (length(schedule) > 0) {
    for (t in 1: schedule[1]) {  # day 0 to dose 1 on day 70 
      df_urban$prot_trip[t]<-0
      df_urban$prot_quadr[t]<-0
      df_urban$prot_quint[t]<-0
      df_urban$prot_sext[t]<- 0
      df_urban$prot_other[t]<-0
      df_urban$prot_VAGKAA[t]<- 0
      df_urban$prot_VAGKGS[t]<- 0  
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_urban))  {  
      df_urban$prot_trip[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_urban[length(cov_urban)]
      df_urban$prot_quadr[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_urban[length(cov_urban)]
      df_urban$prot_quint[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_urban[length(cov_urban)]
      df_urban$prot_sext[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_urban[length(cov_urban)]
      df_urban$prot_other[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKAA[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKGS[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_urban[length(cov_urban)]
    }    
  }
  
  if (length(schedule) > 1) {
    for (t in (schedule[1]+1) : schedule[2])  {   # day 71 to dose 2 on day 98
      df_urban$prot_trip[t]<- exp(-(df_urban$time[t-schedule[1]]/lambda_trip)^w_trip) * cov_urban[1]
      df_urban$prot_quadr[t]<- exp(-(df_urban$time[t-schedule[1]]/lambda_quadr)^w_quadr)* cov_urban[1]
      df_urban$prot_quint[t]<- exp(-(df_urban$time[t-schedule[1]]/lambda_quint)^w_quint)* cov_urban[1]
      df_urban$prot_sext[t]<- exp(-(df_urban$time[t-schedule[1]]/lambda_sext)^w_sext)* cov_urban[1]
      df_urban$prot_other[t]<- exp(-(df_urban$time[t-schedule[1]]/lambda_other)^w_other)* cov_urban[1]
      df_urban$prot_VAGKAA[t]<- exp(-(df_urban$time[t-schedule[1]]/lambda_VAGKAA)^w_VAGKAA)* cov_urban[1]
      df_urban$prot_VAGKGS[t]<- exp(-(df_urban$time[t-schedule[1]]/lambda_VAGKGS)^w_VAGKGS)* cov_urban[1]
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_urban))  {  
      df_urban$prot_trip[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_urban[length(cov_urban)]
      df_urban$prot_quadr[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_urban[length(cov_urban)]
      df_urban$prot_quint[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_urban[length(cov_urban)]
      df_urban$prot_sext[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_urban[length(cov_urban)]
      df_urban$prot_other[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKAA[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKGS[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_urban[length(cov_urban)]
    }    
  } 
  
  if (length(schedule) > 2) {
    for (t in (schedule[2]+1) : schedule[3])  {  #  day 99 to dose 3 on day 180
      df_urban$prot_trip[t]<- exp(-(df_urban$time[t-schedule[2]]/lambda_trip)^w_trip)* cov_urban[2]
      df_urban$prot_quadr[t]<- exp(-(df_urban$time[t-schedule[2]]/lambda_quadr)^w_quadr)* cov_urban[2]
      df_urban$prot_quint[t]<- exp(-(df_urban$time[t-schedule[2]]/lambda_quint)^w_quint)* cov_urban[2]
      df_urban$prot_sext[t]<- exp(-(df_urban$time[t-schedule[2]]/lambda_sext)^w_sext)* cov_urban[2]
      df_urban$prot_other[t]<- exp(-(df_urban$time[t-schedule[2]]/lambda_other)^w_other)* cov_urban[2]
      df_urban$prot_VAGKAA[t]<- exp(-(df_urban$time[t-schedule[2]]/lambda_VAGKAA)^w_VAGKAA)* cov_urban[2]
      df_urban$prot_VAGKGS[t]<- exp(-(df_urban$time[t-schedule[2]]/lambda_VAGKGS)^w_VAGKGS)* cov_urban[2]
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_urban))  {  
      df_urban$prot_trip[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_urban[length(cov_urban)]
      df_urban$prot_quadr[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_urban[length(cov_urban)]
      df_urban$prot_quint[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_urban[length(cov_urban)]
      df_urban$prot_sext[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_urban[length(cov_urban)]
      df_urban$prot_other[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKAA[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKGS[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_urban[length(cov_urban)]
    }    
  }
  
  if (length(schedule) > 3) {
    for (t in (schedule[3]+1) : schedule[4])  {  # day 181 to dose 4 on day 270
      df_urban$prot_trip[t]<- exp(-(df_urban$time[t-schedule[3]]/lambda_trip)^w_trip)* cov_urban[3]
      df_urban$prot_quadr[t]<- exp(-(df_urban$time[t-schedule[3]]/lambda_quadr)^w_quadr)* cov_urban[3]
      df_urban$prot_quint[t]<- exp(-(df_urban$time[t-schedule[3]]/lambda_quint)^w_quint)* cov_urban[3]
      df_urban$prot_sext[t]<- exp(-(df_urban$time[t-schedule[3]]/lambda_sext)^w_sext)* cov_urban[3]
      df_urban$prot_other[t]<- exp(-(df_urban$time[t-schedule[3]]/lambda_other)^w_other)* cov_urban[3]
      df_urban$prot_VAGKAA[t]<- exp(-(df_urban$time[t-schedule[3]]/lambda_VAGKAA)^w_VAGKAA)* cov_urban[3]
      df_urban$prot_VAGKGS[t]<- exp(-(df_urban$time[t-schedule[3]]/lambda_VAGKGS)^w_VAGKGS)* cov_urban[3]
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_urban))  {  
      df_urban$prot_trip[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_urban[length(cov_urban)]
      df_urban$prot_quadr[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_urban[length(cov_urban)]
      df_urban$prot_quint[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_urban[length(cov_urban)]
      df_urban$prot_sext[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_urban[length(cov_urban)]
      df_urban$prot_other[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKAA[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKGS[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_urban[length(cov_urban)]
    }    
  }
  
  if (length(schedule) > 4) {
    for (t in (schedule[4]+1) : schedule[5])  {  # day 271 to dose 5 on day 360 
      df_urban$prot_trip[t]<- exp(-(df_urban$time[t-schedule[4]]/lambda_trip)^w_trip)* cov_urban[4]
      df_urban$prot_quadr[t]<- exp(-(df_urban$time[t-schedule[4]]/lambda_quadr)^w_quadr)* cov_urban[4]
      df_urban$prot_quint[t]<- exp(-(df_urban$time[t-schedule[4]]/lambda_quint)^w_quint)* cov_urban[4]
      df_urban$prot_sext[t]<- exp(-(df_urban$time[t-schedule[4]]/lambda_sext)^w_sext)* cov_urban[4]
      df_urban$prot_other[t]<- exp(-(df_urban$time[t-schedule[4]]/lambda_other)^w_other)* cov_urban[4]
      df_urban$prot_VAGKAA[t]<- exp(-(df_urban$time[t-schedule[4]]/lambda_VAGKAA)^w_VAGKAA)* cov_urban[4]
      df_urban$prot_VAGKGS[t]<- exp(-(df_urban$time[t-schedule[4]]/lambda_VAGKGS)^w_VAGKGS)* cov_urban[4]
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_urban))  {  
      df_urban$prot_trip[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_urban[length(cov_urban)]
      df_urban$prot_quadr[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_urban[length(cov_urban)]
      df_urban$prot_quint[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_urban[length(cov_urban)]
      df_urban$prot_sext[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_urban[length(cov_urban)]
      df_urban$prot_other[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKAA[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKGS[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_urban[length(cov_urban)]
    }    
  }
  
  if (length(schedule) > 5) {
    for (t in (schedule[5]+1) : schedule[6])  {  # day 361 to dose 6 on day 450 
      df_urban$prot_trip[t]<- exp(-(df_urban$time[t-schedule[5]]/lambda_trip)^w_trip)* cov_urban[5]
      df_urban$prot_quadr[t]<- exp(-(df_urban$time[t-schedule[5]]/lambda_quadr)^w_quadr)* cov_urban[5]
      df_urban$prot_quint[t]<- exp(-(df_urban$time[t-schedule[5]]/lambda_quint)^w_quint)* cov_urban[5]
      df_urban$prot_sext[t]<- exp(-(df_urban$time[t-schedule[5]]/lambda_sext)^w_sext)* cov_urban[5]
      df_urban$prot_other[t]<- exp(-(df_urban$time[t-schedule[5]]/lambda_other)^w_other)* cov_urban[5]
      df_urban$prot_VAGKAA[t]<- exp(-(df_urban$time[t-schedule[5]]/lambda_VAGKAA)^w_VAGKAA)* cov_urban[5]
      df_urban$prot_VAGKGS[t]<- exp(-(df_urban$time[t-schedule[5]]/lambda_VAGKGS)^w_VAGKGS)* cov_urban[5]
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_urban))  {  
      df_urban$prot_trip[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_urban[length(cov_urban)]
      df_urban$prot_quadr[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_urban[length(cov_urban)]
      df_urban$prot_quint[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_urban[length(cov_urban)]
      df_urban$prot_sext[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_urban[length(cov_urban)]
      df_urban$prot_other[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKAA[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKGS[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_urban[length(cov_urban)]
    }    
  }
  
  if (length(schedule) > 6) {
    for (t in (schedule[6]+1) : schedule[7])  {  # day 451 to dose 7 on day 540 
      df_urban$prot_trip[t]<- exp(-(df_urban$time[t-schedule[6]]/lambda_trip)^w_trip)* cov_urban[6]
      df_urban$prot_quadr[t]<- exp(-(df_urban$time[t-schedule[6]]/lambda_quadr)^w_quadr)* cov_urban[6]
      df_urban$prot_quint[t]<- exp(-(df_urban$time[t-schedule[6]]/lambda_quint)^w_quint)* cov_urban[6]
      df_urban$prot_sext[t]<- exp(-(df_urban$time[t-schedule[6]]/lambda_sext)^w_sext)* cov_urban[6]
      df_urban$prot_other[t]<- exp(-(df_urban$time[t-schedule[6]]/lambda_other)^w_other)* cov_urban[6]
      df_urban$prot_VAGKAA[t]<- exp(-(df_urban$time[t-schedule[6]]/lambda_VAGKAA)^w_VAGKAA)* cov_urban[6]
      df_urban$prot_VAGKGS[t]<- exp(-(df_urban$time[t-schedule[6]]/lambda_VAGKGS)^w_VAGKGS)* cov_urban[6]
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_urban))  {  
      df_urban$prot_trip[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_urban[length(cov_urban)]
      df_urban$prot_quadr[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_urban[length(cov_urban)]
      df_urban$prot_quint[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_urban[length(cov_urban)]
      df_urban$prot_sext[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_urban[length(cov_urban)]
      df_urban$prot_other[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKAA[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKGS[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_urban[length(cov_urban)]
    }    
  }
  
  if (length(schedule) > 7) {
    for (t in (schedule[7]+1) : schedule[8])  {  # day 541 to dose 8 on day 720
      df_urban$prot_trip[t]<- exp(-(df_urban$time[t-schedule[7]]/lambda_trip)^w_trip)* cov_urban[7]
      df_urban$prot_quadr[t]<- exp(-(df_urban$time[t-schedule[7]]/lambda_quadr)^w_quadr)* cov_urban[7]
      df_urban$prot_quint[t]<- exp(-(df_urban$time[t-schedule[7]]/lambda_quint)^w_quint)* cov_urban[7]
      df_urban$prot_sext[t]<- exp(-(df_urban$time[t-schedule[7]]/lambda_sext)^w_sext)* cov_urban[7]
      df_urban$prot_other[t]<- exp(-(df_urban$time[t-schedule[7]]/lambda_other)^w_other)* cov_urban[7]
      df_urban$prot_VAGKAA[t]<- exp(-(df_urban$time[t-schedule[7]]/lambda_VAGKAA)^w_VAGKAA)* cov_urban[7]
      df_urban$prot_VAGKGS[t]<- exp(-(df_urban$time[t-schedule[7]]/lambda_VAGKGS)^w_VAGKGS)* cov_urban[7]
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_urban))  {  
      df_urban$prot_trip[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_urban[length(cov_urban)]
      df_urban$prot_quadr[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_urban[length(cov_urban)]
      df_urban$prot_quint[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_urban[length(cov_urban)]
      df_urban$prot_sext[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_urban[length(cov_urban)]
      df_urban$prot_other[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKAA[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKGS[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_urban[length(cov_urban)]
    }    
  }
  
  if (length(schedule) > 8) {
    for (t in (schedule[8]+1) : nrow(df_urban))  {  # day 721 to end of simulation
      df_urban$prot_trip[t]<- exp(-(df_urban$time[t-schedule[8]]/lambda_trip)^w_trip)* cov_urban[8]
      df_urban$prot_quadr[t]<- exp(-(df_urban$time[t-schedule[8]]/lambda_quadr)^w_quadr)* cov_urban[8]
      df_urban$prot_quint[t]<- exp(-(df_urban$time[t-schedule[8]]/lambda_quint)^w_quint)* cov_urban[8]
      df_urban$prot_sext[t]<- exp(-(df_urban$time[t-schedule[8]]/lambda_sext)^w_sext)* cov_urban[8]
      df_urban$prot_other[t]<- exp(-(df_urban$time[t-schedule[8]]/lambda_other)^w_other)* cov_urban[8]
      df_urban$prot_VAGKAA[t]<- exp(-(df_urban$time[t-schedule[8]]/lambda_VAGKAA)^w_VAGKAA)* cov_urban[8]
      df_urban$prot_VAGKGS[t]<- exp(-(df_urban$time[t-schedule[8]]/lambda_VAGKGS)^w_VAGKGS)* cov_urban[8]
    }
  } else {
    for (t in (schedule[length(schedule)]+1) : nrow(df_urban))  {  
      df_urban$prot_trip[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov_urban[length(cov_urban)]
      df_urban$prot_quadr[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov_urban[length(cov_urban)]
      df_urban$prot_quint[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov_urban[length(cov_urban)]
      df_urban$prot_sext[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov_urban[length(cov_urban)]
      df_urban$prot_other[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKAA[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov_urban[length(cov_urban)]
      df_urban$prot_VAGKGS[t]<- exp(-(df_urban$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov_urban[length(cov_urban)]
    }    
  }
  
  # proportions of each haplotype for current admin-1 area 
  proportions <- haplotype_proportions %>% filter(iso_code == country_code, NAME_2 == area_names[i])
  
  # overall protection for current admin-1 area
  df_urban[paste0("prot_overall_", area_names[i])] <- as.double(proportions$I_AKA_)*df_urban$prot_trip +
    as.double(proportions$I_GKA_)*df_urban$prot_quadr +
    as.double(proportions$I_GEA_)*df_urban$prot_quint +
    as.double(proportions$I_GEG_)*df_urban$prot_sext +
    as.double(proportions$V_GKA_)*df_urban$prot_VAGKAA +
    as.double(proportions$V_GKG_)*df_urban$prot_VAGKGS +
    as.double(proportions$other)*df_urban$prot_other
  
  
  # save dataframe for current area
  list_of_df_urban[[i]] <- df_urban 
  
  # PMC impact on incidence (ppy)
  
  # replicate incidence (without PMC) dataframe
  new_PMC_impact_ppy_df_urban <- (incidence_ppy_df_urban %>% filter(area == area_names[i]))   
  
  # replace "value" column with new value when PMC is given 
  new_PMC_impact_ppy_df_urban$value <- as.double(new_PMC_impact_ppy_df_urban$value) * rep((1-df_urban[paste0("prot_overall_", area_names[i])][min(age_min):max(age_max),]),4) 
  
  PMC_impact_ppy_urban <- rbind(PMC_impact_ppy_urban, new_PMC_impact_ppy_df_urban)
  
}

names(list_of_df_urban) <- area_names


##### URBAN PMC IMPACT (additional PMC SCHEDULE) #####


# store rural PMC impact data for each admin-1 area
list_of_df_urban_additional <- vector(mode = "list", length(area_names))
PMC_impact_ppy_urban_additional <- data.frame()


# calculate incidence when PMC is given for all urban areas in country 
for (i in 1:length(area_names)) {
  
  # additional vaccine schedule in days
  schedule_additional_doses <- 7 * as.numeric((coverage_df_additional_schedule %>% filter(area == area_names[i]))$vaccine_weeks)
  
  # urban vaccine coverage 
  cov_additional_doses_urban <- as.numeric((coverage_df_additional_schedule %>% filter(area == area_names[i]))$cov_urban)
  
  # Get the order of indices that would sort schedule_additional_doses
  sorted_indices <- order(schedule_additional_doses)
  
  # Use sorted_indices to reorder both vectors
  sorted_schedule_additional_doses <- schedule_additional_doses[sorted_indices]
  sorted_cov_additional_doses_urban <- cov_additional_doses_urban[sorted_indices]
  
  schedule_additional_doses <- sorted_schedule_additional_doses
  cov_additional_doses_urban <- sorted_cov_additional_doses_urban
  
  # set up df with the timesteps 
  if (sim_length > length(age_in_days_midpoint)) {
    df_urban_additional<- data.frame(time=1:sim_length)
  }
  
  if (sim_length < length(age_in_days_midpoint)) {
    df_urban_additional<- data.frame(time=1:length(age_in_days_midpoint))
  }
  
  # initalise empty columns
  df_urban_additional$prot_trip<-NA
  df_urban_additional$prot_quadr<-NA
  df_urban_additional$prot_quint<-NA
  df_urban_additional$prot_sext<-NA
  df_urban_additional$prot_other<-NA
  df_urban_additional$prot_VAGKAA<-NA
  df_urban_additional$prot_VAGKGS<-NA
  
  
  # construct weibull curves to model the efficacy of SP through time for 
  # each haplotype 
  
  if (length(schedule_additional_doses) > 0) {
    for (t in 1: schedule_additional_doses[1]) {  # day 0 to dose 1 on day 70 
      df_urban_additional$prot_trip[t]<-0
      df_urban_additional$prot_quadr[t]<-0
      df_urban_additional$prot_quint[t]<-0
      df_urban_additional$prot_sext[t]<- 0
      df_urban_additional$prot_other[t]<-0
      df_urban_additional$prot_VAGKAA[t]<- 0
      df_urban_additional$prot_VAGKGS[t]<- 0  
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_urban_additional))  {  
      df_urban_additional$prot_trip[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quadr[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quint[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_sext[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_other[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKAA[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKGS[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
    }    
  }
  
  if (length(schedule_additional_doses) > 1) {
    for (t in (schedule_additional_doses[1]+1) : schedule_additional_doses[2])  {   # day 71 to dose 2 on day 98
      df_urban_additional$prot_trip[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[1]]/lambda_trip)^w_trip) * cov_additional_doses_urban[1]
      df_urban_additional$prot_quadr[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[1]]/lambda_quadr)^w_quadr)* cov_additional_doses_urban[1]
      df_urban_additional$prot_quint[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[1]]/lambda_quint)^w_quint)* cov_additional_doses_urban[1]
      df_urban_additional$prot_sext[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[1]]/lambda_sext)^w_sext)* cov_additional_doses_urban[1]
      df_urban_additional$prot_other[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[1]]/lambda_other)^w_other)* cov_additional_doses_urban[1]
      df_urban_additional$prot_VAGKAA[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[1]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_urban[1]
      df_urban_additional$prot_VAGKGS[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[1]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_urban[1]
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_urban_additional))  {  
      df_urban_additional$prot_trip[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quadr[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quint[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_sext[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_other[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKAA[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKGS[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
    }    
  } 
  
  if (length(schedule_additional_doses) > 2) {
    for (t in (schedule_additional_doses[2]+1) : schedule_additional_doses[3])  {  #  day 99 to dose 3 on day 180
      df_urban_additional$prot_trip[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[2]]/lambda_trip)^w_trip)* cov_additional_doses_urban[2]
      df_urban_additional$prot_quadr[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[2]]/lambda_quadr)^w_quadr)* cov_additional_doses_urban[2]
      df_urban_additional$prot_quint[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[2]]/lambda_quint)^w_quint)* cov_additional_doses_urban[2]
      df_urban_additional$prot_sext[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[2]]/lambda_sext)^w_sext)* cov_additional_doses_urban[2]
      df_urban_additional$prot_other[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[2]]/lambda_other)^w_other)* cov_additional_doses_urban[2]
      df_urban_additional$prot_VAGKAA[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[2]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_urban[2]
      df_urban_additional$prot_VAGKGS[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[2]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_urban[2]
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_urban_additional))  {  
      df_urban_additional$prot_trip[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quadr[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quint[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_sext[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_other[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKAA[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKGS[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
    }    
  }
  
  if (length(schedule_additional_doses) > 3) {
    for (t in (schedule_additional_doses[3]+1) : schedule_additional_doses[4])  {  # day 181 to dose 4 on day 270
      df_urban_additional$prot_trip[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[3]]/lambda_trip)^w_trip)* cov_additional_doses_urban[3]
      df_urban_additional$prot_quadr[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[3]]/lambda_quadr)^w_quadr)* cov_additional_doses_urban[3]
      df_urban_additional$prot_quint[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[3]]/lambda_quint)^w_quint)* cov_additional_doses_urban[3]
      df_urban_additional$prot_sext[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[3]]/lambda_sext)^w_sext)* cov_additional_doses_urban[3]
      df_urban_additional$prot_other[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[3]]/lambda_other)^w_other)* cov_additional_doses_urban[3]
      df_urban_additional$prot_VAGKAA[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[3]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_urban[3]
      df_urban_additional$prot_VAGKGS[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[3]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_urban[3]
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_urban_additional))  {  
      df_urban_additional$prot_trip[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quadr[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quint[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_sext[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_other[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKAA[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKGS[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
    }    
  }
  
  if (length(schedule_additional_doses) > 4) {
    for (t in (schedule_additional_doses[4]+1) : schedule_additional_doses[5])  {  # day 271 to dose 5 on day 360 
      df_urban_additional$prot_trip[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[4]]/lambda_trip)^w_trip)* cov_additional_doses_urban[4]
      df_urban_additional$prot_quadr[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[4]]/lambda_quadr)^w_quadr)* cov_additional_doses_urban[4]
      df_urban_additional$prot_quint[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[4]]/lambda_quint)^w_quint)* cov_additional_doses_urban[4]
      df_urban_additional$prot_sext[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[4]]/lambda_sext)^w_sext)* cov_additional_doses_urban[4]
      df_urban_additional$prot_other[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[4]]/lambda_other)^w_other)* cov_additional_doses_urban[4]
      df_urban_additional$prot_VAGKAA[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[4]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_urban[4]
      df_urban_additional$prot_VAGKGS[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[4]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_urban[4]
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_urban_additional))  {  
      df_urban_additional$prot_trip[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quadr[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quint[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_sext[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_other[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKAA[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKGS[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
    }    
  }
  
  if (length(schedule_additional_doses) > 5) {
    for (t in (schedule_additional_doses[5]+1) : schedule_additional_doses[6])  {  # day 361 to dose 6 on day 450 
      df_urban_additional$prot_trip[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[5]]/lambda_trip)^w_trip)* cov_additional_doses_urban[5]
      df_urban_additional$prot_quadr[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[5]]/lambda_quadr)^w_quadr)* cov_additional_doses_urban[5]
      df_urban_additional$prot_quint[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[5]]/lambda_quint)^w_quint)* cov_additional_doses_urban[5]
      df_urban_additional$prot_sext[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[5]]/lambda_sext)^w_sext)* cov_additional_doses_urban[5]
      df_urban_additional$prot_other[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[5]]/lambda_other)^w_other)* cov_additional_doses_urban[5]
      df_urban_additional$prot_VAGKAA[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[5]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_urban[5]
      df_urban_additional$prot_VAGKGS[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[5]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_urban[5]
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_urban_additional))  {  
      df_urban_additional$prot_trip[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quadr[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quint[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_sext[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_other[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKAA[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKGS[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
    }    
  }
  
  if (length(schedule_additional_doses) > 6) {
    for (t in (schedule_additional_doses[6]+1) : schedule_additional_doses[7])  {  # day 451 to dose 7 on day 540 
      df_urban_additional$prot_trip[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[6]]/lambda_trip)^w_trip)* cov_additional_doses_urban[6]
      df_urban_additional$prot_quadr[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[6]]/lambda_quadr)^w_quadr)* cov_additional_doses_urban[6]
      df_urban_additional$prot_quint[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[6]]/lambda_quint)^w_quint)* cov_additional_doses_urban[6]
      df_urban_additional$prot_sext[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[6]]/lambda_sext)^w_sext)* cov_additional_doses_urban[6]
      df_urban_additional$prot_other[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[6]]/lambda_other)^w_other)* cov_additional_doses_urban[6]
      df_urban_additional$prot_VAGKAA[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[6]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_urban[6]
      df_urban_additional$prot_VAGKGS[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[6]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_urban[6]
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_urban_additional))  {  
      df_urban_additional$prot_trip[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quadr[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quint[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_sext[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_other[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKAA[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKGS[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
    }    
  }
  
  if (length(schedule_additional_doses) > 7) {
    for (t in (schedule_additional_doses[7]+1) : schedule_additional_doses[8])  {  # day 541 to dose 8 on day 720
      df_urban_additional$prot_trip[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[7]]/lambda_trip)^w_trip)* cov_additional_doses_urban[7]
      df_urban_additional$prot_quadr[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[7]]/lambda_quadr)^w_quadr)* cov_additional_doses_urban[7]
      df_urban_additional$prot_quint[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[7]]/lambda_quint)^w_quint)* cov_additional_doses_urban[7]
      df_urban_additional$prot_sext[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[7]]/lambda_sext)^w_sext)* cov_additional_doses_urban[7]
      df_urban_additional$prot_other[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[7]]/lambda_other)^w_other)* cov_additional_doses_urban[7]
      df_urban_additional$prot_VAGKAA[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[7]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_urban[7]
      df_urban_additional$prot_VAGKGS[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[7]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_urban[7]
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_urban_additional))  {  
      df_urban_additional$prot_trip[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quadr[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quint[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_sext[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_other[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKAA[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKGS[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
    }    
  }
  
  if (length(schedule_additional_doses) > 8) {
    for (t in (schedule_additional_doses[8]+1) : nrow(df_urban_additional))  {  # day 721 to end of simulation
      df_urban_additional$prot_trip[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[8]]/lambda_trip)^w_trip)* cov_additional_doses_urban[8]
      df_urban_additional$prot_quadr[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[8]]/lambda_quadr)^w_quadr)* cov_additional_doses_urban[8]
      df_urban_additional$prot_quint[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[8]]/lambda_quint)^w_quint)* cov_additional_doses_urban[8]
      df_urban_additional$prot_sext[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[8]]/lambda_sext)^w_sext)* cov_additional_doses_urban[8]
      df_urban_additional$prot_other[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[8]]/lambda_other)^w_other)* cov_additional_doses_urban[8]
      df_urban_additional$prot_VAGKAA[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[8]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_urban[8]
      df_urban_additional$prot_VAGKGS[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[8]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_urban[8]
    }
  } else {
    for (t in (schedule_additional_doses[length(schedule_additional_doses)]+1) : nrow(df_urban_additional))  {  
      df_urban_additional$prot_trip[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_trip)^w_trip)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quadr[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quadr)^w_quadr)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_quint[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_quint)^w_quint)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_sext[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_sext)^w_sext)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_other[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_other)^w_other)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKAA[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKAA)^w_VAGKAA)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
      df_urban_additional$prot_VAGKGS[t]<- exp(-(df_urban_additional$time[t-schedule_additional_doses[length(schedule_additional_doses)]]/lambda_VAGKGS)^w_VAGKGS)* cov_additional_doses_urban[length(cov_additional_doses_urban)]
    }    
  }
  
  # proportions of each haplotype for current admin-1 area 
  proportions <- haplotype_proportions %>% filter(iso_code == country_code, NAME_2 == area_names[i])
  
  # overall protection for current admin-1 area
  df_urban_additional[paste0("prot_overall_", area_names[i])] <- as.double(proportions$I_AKA_)*df_urban_additional$prot_trip +
    as.double(proportions$I_GKA_)*df_urban_additional$prot_quadr +
    as.double(proportions$I_GEA_)*df_urban_additional$prot_quint +
    as.double(proportions$I_GEG_)*df_urban_additional$prot_sext +
    as.double(proportions$V_GKA_)*df_urban_additional$prot_VAGKAA +
    as.double(proportions$V_GKG_)*df_urban_additional$prot_VAGKGS +
    as.double(proportions$other)*df_urban_additional$prot_other
  
  
  # save dataframe for current area
  list_of_df_urban_additional[[i]] <- df_urban_additional 
  
  # PMC impact on incidence (ppy)
  
  # replicate incidence (without PMC) dataframe
  new_PMC_impact_ppy_df_urban_additional <- (incidence_ppy_df_urban %>% filter(area == area_names[i]))   
  
  
  # replace "value" column with new value when PMC is given 
  new_PMC_impact_ppy_df_urban_additional$value <- as.double(new_PMC_impact_ppy_df_urban_additional$value) * rep((1-df_urban_additional[paste0("prot_overall_", area_names[i])][min(age_min):max(age_max),]),4) 
  
  PMC_impact_ppy_urban_additional <- rbind(PMC_impact_ppy_urban_additional, new_PMC_impact_ppy_df_urban_additional)
  
}

names(list_of_df_urban_additional) <- area_names


##### COUNT NUMBER OF PMC DOSES #####

PMC_doses_given <- data.frame()
PMC_doses_given_additional_doses <- data.frame()

for (i in 1:length(area_names)) {
  
  for (j in 1:length(schedule)) {
    
    if (schedule[j] < max(age_in_days_midpoint)) {
      
      pop_rural <- (mean(c(sum(((population_df_rural %>% filter(area==area_names[i]))[5+schedule[j]])[1:365,]), sum(((population_df_rural %>% filter(area==area_names[i]))[5+schedule[j]])[366:730,]))) / 1e6) * sum((full_data$population %>% 
                                                                                                                                                                                                                filter(name_2 == area_names[i], year==2023, urban_rural=="rural"))$pop)
      
      pop_urban <- (mean(c(sum(((population_df_urban %>% filter(area==area_names[i]))[5+schedule[j]])[1:365,]), sum(((population_df_urban %>% filter(area==area_names[i]))[5+schedule[j]])[366:730,]))) / 1e6) * sum((full_data$population %>% 
                                                                                                                                                                                                                filter(name_2 == area_names[i], year==2023, urban_rural=="urban"))$pop)
      
      cov_df <- (coverage_df %>% filter(area==area_names[i]))
      
      # Get the order of indices that would sort schedule_additional_doses
      sorted_indices <- order(cov_df$vaccine_days)
      
      # Use sorted_indices to reorder both vectors
      sorted_cov_rural <- as.numeric(cov_df$cov_rural[sorted_indices])
      sorted_cov_urban <- as.numeric(cov_df$cov_urban[sorted_indices])
      sorted_vaccine <- cov_df$vaccine[sorted_indices]
      sorted_vaccine_weeks <- as.numeric(cov_df$vaccine_weeks[sorted_indices])
      sorted_vaccine_days <- as.numeric(cov_df$vaccine_days[sorted_indices])
      
      cov_df$cov_rural <- sorted_cov_rural
      cov_df$cov_urban <- sorted_cov_urban
      cov_df$vaccine <- sorted_vaccine
      cov_df$vaccine_weeks <- sorted_vaccine_weeks
      cov_df$vaccine_days <- sorted_vaccine_days
      
      
      PMC_doses_rural <- pop_rural * (cov_df$cov_rural[j])
      PMC_doses_urban <- pop_urban * (cov_df$cov_urban[j])
      
      
    } else {
      PMC_doses_rural <- NA
      PMC_doses_urban <- NA
    }
    
    PMC_doses_given <- rbind(PMC_doses_given, c(PMC_doses_rural, PMC_doses_urban))
    
  }
  
  for (j in 1:length(schedule_additional_doses)) {
    
    if (schedule_additional_doses[j] < max(age_in_days_midpoint)) {

      pop_rural <- (mean(c(sum(((population_df_rural %>% filter(area==area_names[i]))[5+schedule_additional_doses[j]])[1:365,]), sum(((population_df_rural %>% filter(area==area_names[i]))[5+schedule_additional_doses[j]])[366:730,]))) / 1e6) * sum((full_data$population %>% 
                                                                                                                                                                                                                        filter(name_2 == area_names[i], year==2023, urban_rural=="rural"))$pop)
      
      pop_urban <- (mean(c(sum(((population_df_urban %>% filter(area==area_names[i]))[5+schedule_additional_doses[j]])[1:365,]), sum(((population_df_urban %>% filter(area==area_names[i]))[5+schedule_additional_doses[j]])[366:730,]))) / 1e6) * sum((full_data$population %>% 
                                                                                                                                                                                                                        filter(name_2 == area_names[i], year==2023, urban_rural=="urban"))$pop)
      
      cov_df <- (coverage_df_additional_schedule %>% filter(area==area_names[i]))
      
      # Get the order of indices that would sort schedule_additional_doses
      sorted_indices <- order(cov_df$vaccine_days)
      
      # Use sorted_indices to reorder both vectors
      sorted_cov_rural <- as.numeric(cov_df$cov_rural[sorted_indices])
      sorted_cov_urban <- as.numeric(cov_df$cov_urban[sorted_indices])
      sorted_vaccine <- cov_df$vaccine[sorted_indices]
      sorted_vaccine_weeks <- as.numeric(cov_df$vaccine_weeks[sorted_indices])
      sorted_vaccine_days <- as.numeric(cov_df$vaccine_days[sorted_indices])
      
      cov_df$cov_rural <- sorted_cov_rural
      cov_df$cov_urban <- sorted_cov_urban
      cov_df$vaccine <- sorted_vaccine
      cov_df$vaccine_weeks <- sorted_vaccine_weeks
      cov_df$vaccine_days <- sorted_vaccine_days
      
      
      
      PMC_doses_rural_additional_doses <- pop_rural * (cov_df$cov_rural[j])
      PMC_doses_urban_additional_doses <- pop_urban * (cov_df$cov_urban[j])

      } else {
      PMC_doses_rural_additional_doses <- NA
      PMC_doses_urban_additional_doses <- NA
    }
    
    PMC_doses_given_additional_doses <- rbind(PMC_doses_given_additional_doses, c(PMC_doses_rural_additional_doses, PMC_doses_urban_additional_doses))
    
    
  }
}
  

colnames(PMC_doses_given) <- c("rural", "urban")
PMC_doses_given$schedule <- rep(length(schedule), times=length(area_names))
PMC_doses_given$number_dose <- rep(1:length(schedule), times=length(area_names))
PMC_doses_given$PMC_dose_time <- schedule
PMC_doses_given$iso_code <- rep(country_code, dim(PMC_doses_given)[1])
PMC_doses_given$area <- rep(area_names, each=length(schedule))
PMC_doses_given$merged <- PMC_doses_given$rural + PMC_doses_given$urban
PMC_doses_given <- PMC_doses_given %>%
  relocate(iso_code, area, schedule, number_dose, PMC_dose_time, rural, urban, merged)


colnames(PMC_doses_given_additional_doses) <- c("rural", "urban")
PMC_doses_given_additional_doses$schedule <- rep(length(schedule_additional_doses), times=length(area_names))
PMC_doses_given_additional_doses$number_dose <- rep(1:length(schedule_additional_doses), times=length(area_names))
PMC_doses_given_additional_doses$PMC_dose_time <- schedule_additional_doses
PMC_doses_given_additional_doses$iso_code <- rep(country_code, dim(PMC_doses_given_additional_doses)[1])
PMC_doses_given_additional_doses$area <- rep(area_names, each=length(schedule_additional_doses))
PMC_doses_given_additional_doses$merged <- PMC_doses_given_additional_doses$rural + PMC_doses_given_additional_doses$urban
PMC_doses_given_additional_doses <- PMC_doses_given_additional_doses %>%
  relocate(iso_code, area, schedule, number_dose, PMC_dose_time, rural, urban, merged)


##### SAVE PMC IMPACT DATAFRAMES #####

write.csv(PMC_impact_ppy_rural, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_rural.csv"), row.names=FALSE)
write.csv(PMC_impact_ppy_urban, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_urban.csv"), row.names=FALSE)
write.csv(PMC_impact_ppy_rural_additional, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_additional_rural.csv"), row.names=FALSE)
write.csv(PMC_impact_ppy_urban_additional, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_additional_urban.csv"), row.names=FALSE)
write.csv(PMC_doses_given, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_doses_given.csv"), row.names=FALSE)
write.csv(PMC_doses_given_additional_doses, paste0(getwd(), "/", country_code, "/sim_results_DHS_coverage_levels/PMC_doses_given_additional_doses.csv"), row.names=FALSE)



##### SP PROTECTION GRAPHS #####

# Can change area by filtering for different admin-1 area 

# rural
ggplot(data=list_of_df_rural$Gaza)+ theme_bw() +
  geom_line(aes(x=time, y=prot_trip,color="I_AKA_")) +
  geom_line(aes(x=time, y=prot_quadr,color="I_GKA_")) +
  geom_line(aes(x=time, y=prot_quint,color="I_GEA_")) +
  geom_line(aes(x=time, y=prot_VAGKAA,color= "V_GKA_")) +
  geom_line(aes(x=time, y=prot_sext, color="I_GEG_")) +
  geom_line(aes(x=time, y=prot_VAGKGS, color="V_GKG_")) +
  geom_line(aes(x=time, y=prot_other, color="other")) +
  geom_vline(xintercept = schedule, color="blue", linetype="dashed") +
  labs(color="Resistance genotype") +
  ylab("Probability of drug protection")+xlab("Age(days)")

# urban
ggplot(data=list_of_df_urban$Gaza)+ theme_bw() +
  geom_line(aes(x=time, y=prot_trip,color="I_AKA_")) +
  geom_line(aes(x=time, y=prot_quadr,color="I_GKA_")) +
  geom_line(aes(x=time, y=prot_quint,color="I_GEA_")) +
  geom_line(aes(x=time, y=prot_VAGKAA,color= "V_GKA_")) +
  geom_line(aes(x=time, y=prot_sext, color="I_GEG_")) +
  geom_line(aes(x=time, y=prot_VAGKGS, color="V_GKG_")) +
  geom_line(aes(x=time, y=prot_other, color="other")) +
  geom_vline(xintercept = schedule, color="blue", linetype="dashed") +
  labs(color="Resistance genotype") +
  ylab("Probability of drug protection")+xlab("Age(days)")

# rural (additional PMC schedule)
ggplot(data=list_of_df_rural_additional$Gaza)+ theme_bw() +
  geom_line(aes(x=time, y=prot_trip,color="I_AKA_")) +
  geom_line(aes(x=time, y=prot_quadr,color="I_GKA_")) +
  geom_line(aes(x=time, y=prot_quint,color="I_GEA_")) +
  geom_line(aes(x=time, y=prot_VAGKAA,color= "V_GKA_")) +
  geom_line(aes(x=time, y=prot_sext, color="I_GEG_")) +
  geom_line(aes(x=time, y=prot_VAGKGS, color="V_GKG_")) +
  geom_line(aes(x=time, y=prot_other, color="other")) +
  geom_vline(xintercept = schedule_additional_doses, color="blue", linetype="dashed") +
  labs(color="Resistance genotype") +
  ylab("Probability of drug protection")+xlab("Age(days)")

# urban (additional PMC schedule)
ggplot(data=list_of_df_urban_additional$Gaza)+ theme_bw() +
  geom_line(aes(x=time, y=prot_trip,color="I_AKA_")) +
  geom_line(aes(x=time, y=prot_quadr,color="I_GKA_")) +
  geom_line(aes(x=time, y=prot_quint,color="I_GEA_")) +
  geom_line(aes(x=time, y=prot_VAGKAA,color= "V_GKA_")) +
  geom_line(aes(x=time, y=prot_sext, color="I_GEG_")) +
  geom_line(aes(x=time, y=prot_VAGKGS, color="V_GKG_")) +
  geom_line(aes(x=time, y=prot_other, color="other")) +
  geom_vline(xintercept = schedule_additional_doses, color="blue", linetype="dashed") +
  labs(color="Resistance genotype") +
  ylab("Probability of drug protection")+xlab("Age(days)")


