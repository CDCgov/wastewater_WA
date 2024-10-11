#Install pacman if missing
if(!require(pacman)) install.packages("pacman")

#Load packages
pacman::p_load(
  EpiNow2,
  rio,
  here,
  data.table,
  arrow,
  wwinference,
  patchwork,
  epiparameter,
  scoringutils,
  ggh4x,
  tidybayes,
  cmdstanr,
  tidyverse
)

#Load in functions
invisible(sapply(list.files(here("R", "functions"), full.names = T), function(x) source(x)))

#Import data - these data sets will be missing if you have pulled this from github
#Hospital count data
hospital_counts <- import(here("data", "WA", "simulated_hospital_counts.csv"))
#Wastewater sample data
ww_data <- import(here("data", "WA", "simulated_ww_data.csv"))
#Population data for each catchment
pop_data <- import(here("data", "WA", "simulated_catchment_populations.csv"))
#Normalized distance between each wastewater facility
facility_distance <- import(here("data", "WA", "simulated_facility_distances.csv"))

#Process data and compile model - custom function to do things neatly behind the scenes
processed_data <- process_WA_data(
  #The raw hospital count data
  raw_hospital_counts = hospital_counts,
  #The raw wastewater data
  raw_ww_data = ww_data,
  #Population data
  pop_data = pop_data,
  #Sites - this can be specified as a single site,( = 1), as multiple sites linked by a ;, (= "1;2;3") or as all sites (= "all")
  sites = "all",
  #How far to forecast
  forecast_horizon = 28,
  #Spatial data, if missing either omit this argument or set spatial = NA (the default) 
  spatial = facility_distance
)

#Compile model - have to specify a specific location for windows computers where there is no space (i.e. no /Program Files/)
if(Sys.info()[[1]] == "Windows"){
  model <- wwinference::compile_model(
    model_filepath = "C:/R/wwinference.stan",         #My custom location for the stan files 
    include_paths = "C:/R/"
  )
} else {
  model <- wwinference::compile_model()
}

#Specify fit options
fit_this <- get_mcmc_options(
  iter_warmup = 250,
  iter_sampling = 500,
  seed = 123
)

#Fit models
#No spatial information
ww_fit <- wwinference(
  ww_data = processed_data$ww_data_fit,
  count_data = processed_data$hosp_data_fit,
  forecast_date = processed_data$forecast_date,
  calibration_time = as.numeric((processed_data$forecast_date - processed_data$forecast_horizon) - processed_data$time_start),
  forecast_horizon = processed_data$forecast_horizon,
  model_spec = get_model_spec(
    generation_interval = processed_data$generation_interval,
    inf_to_count_delay = processed_data$inf_to_hosp,
    infection_feedback_pmf = processed_data$infection_feedback_pmf,
    params = processed_data$params,
    include_ww = TRUE
  ),
  fit_opts = fit_this,
  compiled_model = model
)

#Exponential fit
ww_exp_fit <- wwinference(
  ww_data = processed_data$ww_data_fit,
  count_data = processed_data$hosp_data_fit,
  forecast_date = processed_data$forecast_date,
  calibration_time = as.numeric((processed_data$forecast_date - processed_data$forecast_horizon) - processed_data$time_start),
  forecast_horizon = processed_data$forecast_horizon,
  model_spec = get_model_spec(
    generation_interval = processed_data$generation_interval,
    inf_to_count_delay = processed_data$inf_to_hosp,
    infection_feedback_pmf = processed_data$infection_feedback_pmf,
    params = processed_data$params,
    include_ww = TRUE
  ),
  fit_opts = fit_this,
  compiled_model = model,
  dist_matrix = as.matrix(processed_data$dist_mat),
  corr_structure_switch = 1
)

#No correlation structure
ww_nocor_fit <- wwinference(
  ww_data = processed_data$ww_data_fit,
  count_data = processed_data$hosp_data_fit,
  forecast_date = processed_data$forecast_date,
  calibration_time = as.numeric((processed_data$forecast_date - processed_data$forecast_horizon) - processed_data$time_start),
  forecast_horizon = processed_data$forecast_horizon,
  model_spec = get_model_spec(
    generation_interval = processed_data$generation_interval,
    inf_to_count_delay = processed_data$inf_to_hosp,
    infection_feedback_pmf = processed_data$infection_feedback_pmf,
    params = processed_data$params,
    include_ww = TRUE
  ),
  fit_opts = fit_this,
  compiled_model = model,
  dist_matrix = as.matrix(processed_data$dist_mat),
  corr_structure_switch = 0
)







