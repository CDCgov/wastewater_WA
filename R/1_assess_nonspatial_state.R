#Install pacman if missing
if(!require(pacman)) install.packages("pacman")

#If you have installed a NON-MAIN branch version of the package, then re-install the main branch
prior <- packageDescription("wwinference")$GithubRef
if(packageDescription("wwinference")$GithubRef != "HEAD"){
  devtools::install_github("CDCgov/ww-inference-model")
}
current <- packageDescription("wwinference")$GithubRef

#Load packages
pacman::p_load(
  rio,
  here,
  arrow,
  wwinference,
  scoringutils,
  ggnewscale,
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
hospital_counts <- import(here("data", "WA", "simulated_hospital_counts.csv"))
#Wastewater sample data
ww_data <- import(here("data", "WA", "simulated_ww_data.csv"))
#Population data for each catchment
pop_data <- import(here("data", "WA", "simulated_catchment_populations.csv"))

#Compile model - have to specify a specific location for windows computers where there is no space (i.e. no /Program Files/)
#Added the argument update_files so that if the prior version of the package is different to the current, it does a fresh
#move of the files. The stan files can be different between package versions.
compiled_model <- compile_model_upd(update_files = prior != current)

#Specify fit options
fit_this <- get_mcmc_options(
  iter_warmup = 250,
  iter_sampling = 500,
  seed = 1
)

#Now we want to loop through sections of the data to evaluate the fit
#We are going to split the dataset into 118 day chunks (90 days calibration, 28 days prediction), and then run the model on each chunk and evaluate the fit
set.seed(1)

#Run through the shuffle of dates
WA_nonspatial_run(
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
  repeats = 10,  
  #Savename modifier for file output
  savename = "nonspatial_state",    
  #Model fit options - specified above
  fit_options = fit_this,
  #The compiled model
  compiled_model = compiled_model
)



