#Install pacman if missing
if(!require(pacman)) install.packages("pacman")

#If you have installed a NON-MAIN branch version of the package, then re-install the main branch
if(packageDescription("wwinference")$GithubRef != "HEAD"){
  devtools::install_github("CDCgov/ww-inference-model")
}

#Load packages
pacman::p_load(
  rio,
  here,
  arrow,
  wwinference,
  scoringutils,
  ggnewscale,
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
#Loop by site
site_vector <- unique(ww_data$treatment_plant)

all_runs <- sapply(site_vector, function(x){
  
  print(x)
  
  #Run through individual sites, wrap to prevent breaking and save a file if it fails
  try(
    test_model_date_shuffle_nonspatial(
      raw_hospital_counts = hospital_counts,             
      raw_ww_data = ww_data,                             
      pop_data,                                          
      sites = x,                                    
      WA_population = 7786000,                           
      forecast_horizon = 28,                             
      calibration_time = 90,                             
      repeats = 10,
      savename = paste0("substate_estimates/site_", x),                        
      fit_options = fit_this
    )
  )

}, simplify = FALSE)

#Look at all runs and see what % actually worked
completed_runs <- do.call(rbind, sapply(site_vector, function(x){
  
  #Create filepath
  look_here <- paste0("figs/date_shuffle/substate_estimates/site_", x)
  results <- list.files(look_here, recursive = T, pattern = "diagnostics")
  results <- results[grepl("_of_10", results)]
  
  #See how many were created
  data.frame(
    site = x,
    runs_completed = length(results),
    prop_runs_completed = length(results)/10
  )
  
}, simplify = FALSE))

#Add in population for plot
completed_runs_upd <- completed_runs %>%
  left_join(pop_data %>%
              select(-V1),
            by = c("site" = "treatment_plant"))

#Overall completed
overall_completed <- sum(completed_runs$runs_completed)/(length(completed_runs$runs_completed) * 10)

ggplot(data = completed_runs_upd,
       mapping = aes(
         x = as.factor(site),
         y = 100 * prop_runs_completed,
         fill = log(catchment_population))
       ) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Site",
       fill = "Site population\n(log scale)",
       y = "% of runs completed",
       title = "% of runs completed per site, for 10 randomly selected date windows.",
       subtitle = "Dashed line is the overall % of runs completed for all sites.") +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_continuous(labels = scales::comma) +
  geom_hline(yintercept = 100 * overall_completed, linetype = "dashed", color = "red")
  

ggsave("figs/date_shuffle/substate_estimates/percent_site_runs_completed.jpg", width = 7, height = 4)



#Re-do missing
redo_these <- data.frame(
  sites = 1,
  runs = as.character(11)
)

all_runs <- sapply(1:nrow(redo_these), function(x){
  
  print(x)
  
  #Run through individual sites, wrap to prevent breaking and save a file if it fails
  try(
    test_model_date_shuffle_nonspatial(
      raw_hospital_counts = hospital_counts,             
      raw_ww_data = ww_data,                             
      pop_data,                                          
      sites = redo_these$sites[x],                                    
      WA_population = 7786000,                           
      forecast_horizon = 28,                             
      calibration_time = 90,                             
      repeats = 10,
      repeat_subset = redo_these$runs[x],
      savename = paste0("substate_estimates/site_", redo_these$sites[x]),                        
      fit_options = fit_this
    )
  )
  
}, simplify = FALSE)









