#Install pacman if missing
if(!require(pacman)) install.packages("pacman")

#If you have installed the NON-SPATIAL version of the package, then re-install the spatial branch
if(packageDescription("wwinference")$GithubRef != "spatial-main"){
  devtools::install_github("CDCgov/ww-inference-model@spatial-main")
}

#Load packages
pacman::p_load(
  rio,
  here,
  arrow,
  wwinference,
  ggnewscale,
  scoringutils,
  data.table,
  ggh4x,
  openxlsx,
  gtsummary,
  tidyverse
)

#Load in functions
invisible(sapply(list.files(here("R", "functions"), full.names = T), function(x) source(x)))

#Import data - these data sets will be missing if you have pulled this from github
#Hospital count data
hospital_counts <- import(here("data", "simulated", "simulated_hospital_counts.csv"))
#Wastewater sample data
ww_data <- import(here("data", "simulated", "simulated_ww_data.csv"))
#Population data for each catchment
pop_data <- import(here("data", "simulated", "simulated_catchment_populations.csv"))
#Normalized distance between each wastewater facility
facility_distance <- import(here("data", "simulated", "simulated_facility_distances.csv"))

#Compile model - have to specify a specific location for windows computers where there is no space (i.e. no /Program Files/)
compiled_model <- compile_model_upd(update_files = T)

#Specify fit options
fit_this <- get_mcmc_options(
  iter_warmup = 250,
  iter_sampling = 500,
  seed = 1
)

#Now we want to loop through sections of the data to evaluate the fit
#We are going to split the dataset into 118 day chunks (90 days calibration, 28 days prediction), and then run the model on each chunk and evaluate the fit
set.seed(1)

#Set the number of cores
options(mc.cores = min(8, parallel::detectCores()))

#Run through the shuffle of dates
WA_spatial_run(
  #Raw hospital data
  raw_hospital_counts = hospital_counts,         
  #Raw wastewater data
  raw_ww_data = ww_data,  
  #Population by catchment
  pop_data,
  #If listing individual sites, combine with ; (i.e. 2;3;4)
  sites = "all",                         
  #WA population
  WA_population = 7786000,    
  #Number of days you are going to predict to
  forecast_horizon = 28,    
  #How long we want to calibrate for
  calibration_time = 90,               
  #How many shuffles of data we want to repeat this on
  runs = 10,  
  #Save name modifier for file output
  savename = "spatial_realdata",    
  #Model fit options - specified above
  fit_options = fit_this,
  #The compiled model
  compiled_model = compiled_model,
  #Spatial data
  spatial = facility_distance
)



