

process_WA_data <- function(raw_hospital_counts,
                            raw_ww_data,
                            pop_data,
                            sites = "all",                      #If listing individual sites, combine with ; (i.e. 2;3;4)
                            WA_population = 7786000,
                            forecast_horizon,                   #Number of days you are going to predict to
                            spatial = NA
){
  
  #Subset to specific sites
  sites_unpacked <- if(sites == "all"){
    as.numeric(unique(raw_ww_data$treatment_plant))
  } else if(grepl(";", sites)){
    as.numeric(unlist(strsplit(sites, ";")))
  } else {
    as.numeric(sites)
  }
  
  ww_data_subset <- raw_ww_data %>% 
    subset(treatment_plant %in% sites_unpacked)
  
  #Set up spatial data
  #Set up distance matrix
  if(!all(is.na(spatial))){
    
    dist_mat <- matrix(ncol = length(sites_unpacked),
                       nrow = length(sites_unpacked))
    
    row.names(dist_mat) <- sites_unpacked
    colnames(dist_mat) <- sites_unpacked
    
    #Loop through matrix to put in distances
    for(i in sites_unpacked){
      for(j in sites_unpacked){
        if(i == j){
          dist_mat[which(row.names(dist_mat) == i), which(colnames(dist_mat) == j)] <- 0
        } else {
          value <- subset(spatial, treatment_plant.1 == i & treatment_plant.2 == j)
          if(nrow(value) == 0){
            value <- subset(spatial, treatment_plant.1 == j & treatment_plant.2 == i)
          }
          dist_mat[which(row.names(dist_mat) == i), which(colnames(dist_mat) == j)] <- value$distance
        }
      }
    }
  }
  
  #Set up parameters
  unique_treatment_plant <- sort(unique(ww_data$treatment_plant))
  n_sites <- length(unique_treatment_plant)
  site <- 1:n_sites
  lab <- rep(1, n_sites)
  
  #Clean data and set up in the format that wwinference wants
  #Set up data in the format it is expected
  #Wastewater data
  ww_data_clean <- ww_data_subset %>%
    rename(
      date = sample_date,
      lab = treatment_plant
    ) %>%
    mutate(
      log_genome_copies_per_ml = log(simulated_raw_concentration),
      log_lod = 5,
      site = lab
    ) %>%
    left_join(
      pop_data %>%
        select(-c(V1)) %>%
        rename(
          lab = treatment_plant,
          site_pop = catchment_population
        ),
      by = c("lab")
    ) %>%
    select(-c(V1))
  
  #Hospitalization data
  #Lab == 0 is the state level data
  hosp_data_clean <- raw_hospital_counts %>%
    select(date = admit_date,
           lab = treatment_plant,
           daily_hosp_admits = simulated_count) %>%
    filter(lab == 0) %>%
    mutate(state_pop = WA_population) %>%
    group_by(date, lab, state_pop) %>%
    summarise(daily_hosp_admits = sum(daily_hosp_admits),
              .groups = "drop") %>%
    ungroup()
  
  #What data to eval
  #Remove forecast_horizon days from the main dataset to create a dataset to train to
  hosp_data_eval <- hosp_data_clean
  
  hosp_data_clean_subset <- hosp_data_clean %>%
    subset(date <= max(date) - forecast_horizon)
  
  ww_data_clean_subset <- ww_data_clean %>%
    subset(date <= max(hosp_data_eval$date))
  
  #Load in parameters
  params <- get_params(
    system.file("extdata", "example_params.toml",
                package = "wwinference"
    )
  )
  
  #Pre-process_data
  ww_data_preprocessed <- preprocess_ww_data(
    ww_data_clean_subset,
    conc_col_name = "log_genome_copies_per_ml",
    lod_col_name = "log_lod"
  ) %>%
    mutate(date = as.Date(date))
  
  hosp_data_preprocessed <- preprocess_count_data(
    hosp_data_clean_subset,
    count_col_name = "daily_hosp_admits",
    pop_size_col_name = "state_pop"
  ) %>%
    mutate(date = as.Date(date))
  
  ww_data_to_fit <- indicate_ww_exclusions(
    ww_data_preprocessed,
    outlier_col_name = "flag_as_ww_outlier",
    remove_outliers = TRUE
  ) %>%
    mutate(date = as.Date(date))
  
  #Line up dates
  min_date <- pmax(min(ww_data_to_fit$date),
                   min(hosp_data_preprocessed$date))
  
  max_date <- pmin(max(ww_data_to_fit$date),
                   max(hosp_data_preprocessed$date))
  
  ww_data_to_fit <- ww_data_to_fit %>%
    filter(date >= min_date & date <= max_date)
  
  hosp_data_preprocessed <- hosp_data_preprocessed %>%
    filter(date >= min_date & date <= max_date)
  
  hosp_data_eval <- hosp_data_eval %>%
    filter(date >= min_date)
  
  #Set up model calibration and forecast time
  generation_interval <- wwinference::default_covid_gi
  inf_to_hosp <- wwinference::default_covid_inf_to_hosp
  
  # Assign infection feedback equal to the generation interval
  infection_feedback_pmf <- generation_interval
  
  #Run model
  time_start <- pmax(min(hosp_data_preprocessed$date), min(ww_data_clean_subset$date))
  forecast_date <- max(hosp_data_preprocessed$date)
  
  #Export
  list(
    ww_data_fit = ww_data_to_fit,
    hosp_data_fit = hosp_data_preprocessed,
    hosp_data_eval = hosp_data_eval,
    time_start = time_start,
    forecast_date = forecast_date,
    forecast_horizon = forecast_horizon,
    generation_interval = generation_interval,
    inf_to_hosp = inf_to_hosp,
    infection_feedback_pmf = infection_feedback_pmf,
    params = params,
    dist_mat = if(all(is.na(spatial))) NA else dist_mat
  )
  
}