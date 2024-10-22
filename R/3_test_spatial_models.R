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
  scoringutils,
  ggh4x,
  openxlsx,
  gtsummary,
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

#Compile model - have to specify a specific location for windows computers where there is no space (i.e. no /Program Files/)
model <- compile_model_upd()

#Specify fit options
fit_this <- get_mcmc_options(
  iter_warmup = 250,
  iter_sampling = 500,
  seed = 1
)

#Now we want to loop through sections of the data to evaluate the fit
#We are going to split the dataset into 120 day chunks (90 days calibration, 30 days prediction), and then run the model on each chunk and evaluate the fit
set.seed(1)

#Run through the shuffle of dates
test_model_date_shuffle(
  raw_hospital_counts = hospital_counts,             #Raw hospital data
  raw_ww_data = ww_data,                             #Raw wastewater data
  pop_data,                                          #Population by catchment
  sites = "all",                                     #If listing individual sites, combine with ; (i.e. 2;3;4)
  WA_population = 7786000,                           #WA population
  forecast_horizon = 28,                             #Number of days you are going to predict to
  spatial = facility_distance,                       #Spatial data values
  calibration_time = 90,                             #How long we want to calibrate for
  repeats = 10,                                      #How many shuffles of data we want to repeat this on
  savename = "spatial_full_run",                     #Savename modifier for file output
  fit_options = fit_this
)



