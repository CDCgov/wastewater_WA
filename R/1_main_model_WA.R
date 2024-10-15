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

#Process data and compile model - custom function to do things neatly behind the scenes
processed_data <- process_WA_data(
  #The raw hospital count data
  raw_hospital_counts = hospital_counts,
  #The raw wastewater data
  raw_ww_data = ww_data,
  #Population data
  pop_data = pop_data,
  #Sites - this can be specified as a single site,( = 1), as multiple sites linked by a ;, (= "1;2;3") or as all sites (= "all")
  sites = "13;8",
  #How far to forecast
  forecast_horizon = 28
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

#Fit models
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
    params = processed_data$params
  ),
  # fit_opts = list(seed = 123,
  #                 iter_warmup = 250,
  #                 iter_sampling = 500),
  fit_options <- get_mcmc_options(
    iter_warmup = 750,
    iter_sampling = 750,
    seed = 1
  ),
  compiled_model = model
)

hosp_fit_only <- wwinference(
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
    include_ww = FALSE
  ),
  fit_opts = list(seed = 123),
  compiled_model = model
)

#Extract draws for analysis and plotting
ww_draw <- get_draws(ww_fit)
hosp_draw <- get_draws(hosp_fit_only)

plot(ww_draw,
     what = "predicted_counts",
     count_data_eval = processed_data$hosp_data_eval,
     count_data_eval_col_name = "daily_hosp_admits",
     forecast_date = processed_data$forecast_date
)

plot(hosp_draw,
     what = "predicted_counts",
     count_data_eval = processed_data$hosp_data_eval,
     count_data_eval_col_name = "daily_hosp_admits",
     forecast_date = processed_data$forecast_date
)


#Plot
plot_ww <- get_plot_ww_conc(ww_draw$predicted_ww, processed_data$forecast_date)

plot_state_rt_ww <- get_plot_global_rt(ww_draw$global_rt, processed_data$forecast_date)
plot_state_rt_hosp <- get_plot_global_rt(hosp_draw$global_rt, processed_data$forecast_date)

#Evaluate
all_pred_draws_df <- rbind(
  ww_draw$predicted_counts %>%
    mutate(
      model = "ww"
    ),
  hosp_draw$predicted_counts %>%
    mutate(
      model = "hosp"
    )
) %>%
  rename(
    true_value = observed_value,
    prediction = pred_value,
    sample = draw
  )

#Overall score
score_models <- all_pred_draws_df %>%
  filter(!is.na(true_value)) %>%
  score() %>%
  summarise_scores(by = "model")

#Nowcasting scores
eval_data <- processed_data$hosp_data_eval %>%
  filter(date > processed_data$hosp_data_fit %>% 
           pull(date) %>% 
           max())

for(i in eval_data$date){
  all_pred_draws_df[which(all_pred_draws_df$date == i), ]$true_value <- eval_data[which(eval_data$date == i), ]$daily_hosp_admits
}
  
#Score it up
all_pred_draws_df %>%
  filter(date > max(processed_data$hosp_data_fit$date)) %>%
  score() %>%
  summarise_scores(by = "model")




  

