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

#Import data
hospital_counts <- import(here("data", "WA", "simulated_hospital_counts.csv"))
ww_data <- import(here("data", "WA", "simulated_ww_data.csv"))
pop_data <- import(here("data", "WA", "simulated_catchment_populations.csv"))
facility_distance <- import(here("data", "WA", "simulated_facility_distances.csv"))

#Process data and compile model
processed_data <- process_WA_data(
  raw_hospital_counts = hospital_counts,
  raw_ww_data = ww_data,
  pop_data = pop_data,
  sites = "all",
  forecast_horizon = 28,
  spatial = facility_distance
)

#Compile model
model <- wwinference::compile_model(
  model_filepath = "C:/R/wwinference.stan",
  include_paths = "C:/R/"
)

#Specify fit options
fit_this <- get_mcmc_options(
  iter_warmup = 250,
  iter_sampling = 500,
  seed = 123
)

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
  fit_opts = fit_this,
  compiled_model = model
)

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
    include_ww = FALSE
  ),
  fit_opts = fit_this,
  compiled_model = model,
  dist_matrix = as.matrix(processed_data$dist_mat),
  corr_structure_switch = 1
)

get_model_diagnostic_flags(ww_exp_fit) 

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
    include_ww = FALSE
  ),
  fit_opts = fit_this,
  compiled_model = model,
  dist_matrix = as.matrix(processed_data$dist_mat),
  corr_structure_switch = 0
)


#Save outputs
# save(ww_fit, file = "output_ww_WA_04102024.Rdata")
# save(hosp_fit_only, file = "output_hospital_only_WA_04102024.Rdata")

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






