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

#Look at how many cases per treatment plant
hospital_counts %>%
  group_by(treatment_plant) %>%
  summarise(n = sum(simulated_count)) %>%
  arrange(n)

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
model <- compile_model_upd()

#Specify fit options
fit_this <- get_mcmc_options(
  iter_warmup = 250,
  iter_sampling = 500,
  seed = 1
)

#Fit models
#No spatial information
ww_fit <- wwinference(
  ww_data = processed_data$ww_data_fit,
  count_data = processed_data$hosp_data_fit,
  forecast_date = processed_data$forecast_date,
  calibration_time = 90,
  forecast_horizon = processed_data$forecast_horizon,
  model_spec = get_model_spec(
    generation_interval = processed_data$generation_interval,
    inf_to_count_delay = processed_data$inf_to_hosp,
    infection_feedback_pmf = processed_data$infection_feedback_pmf,
    params = processed_data$params,
    include_ww = T
  ),
  fit_opts = fit_this,
  compiled_model = model
)

#No correlation structure
ww_nocor_fit <- wwinference(
  ww_data = processed_data$ww_data_fit,
  count_data = processed_data$hosp_data_fit,
  forecast_date = processed_data$forecast_date,
  calibration_time = 90,
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

#Exponential fit
ww_exp_fit <- wwinference(
  ww_data = processed_data$ww_data_fit,
  count_data = processed_data$hosp_data_fit,
  forecast_date = processed_data$forecast_date,
  calibration_time = 90,
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

#LKJ fit
ww_lkj_fit <- wwinference(
  ww_data = processed_data$ww_data_fit,
  count_data = processed_data$hosp_data_fit,
  forecast_date = processed_data$forecast_date,
  calibration_time = 90,
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
  corr_structure_switch = 2
)

#Get draws
ww_draw_nocor <- get_draws_df(ww_nocor_fit)
ww_draw_normal <- get_draws_df(ww_fit)
ww_draw_exp <- get_draws_df(ww_exp_fit)
ww_draw_lkj <- get_draws_df(ww_lkj_fit)

all_pred_draws_df <- rbind(
  ww_draw_nocor %>%
    mutate(model_type = "no_cor"),
  ww_draw_normal %>%
    mutate(model_type = "no_spatial"),
  ww_draw_exp %>%
    mutate(model_type = "exp"),
  ww_draw_lkj %>%
    mutate(model_type = "lkj")
)

#Summarise for plot
hosp_pred <- all_pred_draws_df %>%
  filter(name == "predicted counts") %>%
  group_by(
    date,
    model_type
  ) %>%
  summarise(
    lower = quantile(pred_value, 0.025, na.rm = TRUE),
    median = median(pred_value),
    upper = quantile(pred_value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

#Plot
model_hosp_forecast <- ggplot() +
  geom_line(data = hosp_pred,
            mapping = aes(
              x = date,
              y = median,
              color = model_type
            )) +
  geom_ribbon(data = hosp_pred,
              mapping = aes(
                x = date,
                ymin = lower,
                ymax = upper,
                y = median,
                fill = model_type
              ),
              alpha = 0.15) +
  geom_point(
    data = processed_data$hosp_data_eval %>%
      filter(date %in% hosp_pred$date),
    mapping = aes(
      x = date,
      y = daily_hosp_admits
    ),
    shape = 21,
    color = "black",
    fill = "white"
  ) +
  geom_vline(
    xintercept = processed_data$forecast_date,
    linetype = "dashed"
  ) +
  # facet_wrap(~model_type) +
  theme_bw() +
  labs(x = "",
       y = "Hospital admissions",
       color = "Model type",
       fill = "Model type",
       title = "Hospital admissions forecasts by model type",
       caption = "Dashed line indicates when the model forecast started and white circles the data.")

ggsave("figs/model_hosp_forecast.jpg", height = 4, width = 7)

#Look at correlation matricies
cor_mat <- get_cor_mat(model_fit = list(ww_nocor_fit, ww_fit, ww_exp_fit, ww_lkj_fit),
                       model_names = c("no_correlation", "no_spatial", "exp", "lkj"))

#Plot correlations
model_correlations <- ggplot(data = cor_mat %>%
                               filter(!model_type %in% c("no_correlation",
                                                         "no_spatial")),
                             mapping = aes(
                               x = as.factor(as.numeric(gsub("Site ", "", Column))),
                               y = as.factor(as.numeric(gsub("Site ", "", Row))),
                               fill = median
                             )) +
  geom_tile() +
  facet_wrap(~model_type) +
  theme_bw() +
  labs(x = "",
       y = "",
       fill = "Correlation") +
  scale_fill_viridis_c()

ggsave("figs/model_correlations.jpg", model_correlations, height = 4, width = 8)

#Evaluate models
#Get draws
ww_draw_nocor <- get_draws_df(ww_nocor_fit)
ww_draw_normal <- get_draws_df(ww_fit)
ww_draw_exp <- get_draws_df(ww_exp_fit)
ww_draw_lkj <- get_draws_df(ww_lkj_fit)

#All predictions
all_pred_draws_df <- rbind(
  ww_draw_nocor %>%
    filter(name == "predicted counts") %>%
    mutate(
      model = "no_cor"
    ),
  ww_draw_normal %>%
    filter(name == "predicted counts") %>%
    mutate(
      model = "no_spatial"
    ),
  ww_draw_exp %>%
    filter(name == "predicted counts") %>%
    mutate(
      model = "exp"
    ),
  ww_draw_lkj %>%
    filter(name == "predicted counts") %>%
    mutate(
      model = "lkj"
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
model_score <- all_pred_draws_df %>%
  filter(date > max(processed_data$hosp_data_fit$date)) %>%
  score() %>%
  summarise_scores(by = "model")

write.csv(x = model_score,
          file = "output/spatial_model_score.csv",
          row.names = FALSE)


