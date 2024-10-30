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
  ggh4x,
  openxlsx,
  gtsummary,
  patchwork,
  tidyverse
)

#Load in functions
invisible(sapply(list.files(here("R", "functions"), full.names = T), function(x) source(x)))

#Import data - these data sets will be missing if you have pulled this from github
#Hospital count data
hospital_counts <- import(here("data", "WA", "simulated_hospital_counts.csv"))
#Wastewater sample data
ww_data <- import(here("data", "WA", "simulated_ww_data.csv")) %>%
  mutate(log_concentration = log(simulated_raw_concentration)) %>%
  mutate(treatment_plant = as.character(treatment_plant))
#Population data for each catchment
pop_data <- import(here("data", "WA", "simulated_catchment_populations.csv"))
#Normalized distance between each wastewater facility
facility_distance <- import(here("data", "WA", "simulated_facility_distances.csv"))

#Plot hospital counts over time
levels <- sort(unique(hospital_counts$treatment_plant))
levels[which(levels == 0)] <- "State total"

ww_data <- ww_data %>%
  left_join(
    pop_data %>%
      mutate(treatment_plant = as.character(treatment_plant)),
    by = "treatment_plant"
  ) %>%
  mutate(
    prop_pop = catchment_population/sum(pop_data$catchment_population)
  )
  

ww_data <- ww_data %>%
  plyr::rbind.fill(
    ww_data %>%
      group_by(sample_date) %>%
      summarise(log_concentration = log(sum(sum(simulated_raw_concentration ) * prop_pop))) %>%
      mutate(treatment_plant = "State total")
  ) %>%
  mutate(
    treatment_plant = factor(treatment_plant,
                             levels = levels)
  )


hosp_agg <- hospital_counts %>%
  mutate(treatment_plant = case_when(
    treatment_plant == 0 ~ "State total",
    TRUE ~ as.character(treatment_plant)
  )) %>%
  mutate(
    treatment_plant = factor(treatment_plant,
                             levels = levels)
  ) %>%
  group_by(admit_date, treatment_plant) %>%
  summarise(count = sum(simulated_count))

hosp <- ggplot(
  data = hosp_agg,
  mapping = aes(
    x = admit_date,
    y = count,
    group = treatment_plant
  )
) +
  geom_point(color = "skyblue") +
  theme_bw() +
  labs(
    x = "",
    y = "Hospital admissions"
  ) +
  scale_x_date(
    date_labels = "%b %y"
  ) +
  facet_wrap(~treatment_plant,
             scales = "free_y")

hosp

#Plot wastewater data over time
ww <- ggplot(
  data = ww_data,
  mapping = aes(
    x = sample_date,
    y = log_concentration,
    group = treatment_plant
  )
) +
  geom_point(color = "firebrick1") +
  theme_bw() +
  labs(
    x = "",
    y = "Wastewater concentration (log)"
  ) +
  scale_x_date(
    date_labels = "%b %y"
  ) +
  facet_wrap(~treatment_plant,
             scales = "free_y")

ww

ggsave("figs/hosp_data_allsites.jpg", hosp, height = 5, width = 9)
ggsave("figs/ww_data_allsites.jpg", ww, height = 5, width = 9)


hosp_state <- ggplot() +
  geom_point(
    data = hosp_agg %>%
      filter(treatment_plant == "State total"),
    mapping = aes(
      x = admit_date,
      y = count,
      group = treatment_plant
    ),
    color = "skyblue") +
  theme_bw() +
  labs(
    x = "",
    y = "Hospital admissions"
  ) +
  scale_x_date(
    date_labels = "%b %y"
  ) +
  facet_wrap(~treatment_plant,
             scales = "free_y")

#Plot wastewater data over time
ww_state <- ggplot(
  data = ww_data %>%
    filter(treatment_plant == "State total"),
  mapping = aes(
    x = sample_date,
    y = log_concentration,
    group = treatment_plant
  )
) +
  geom_point(color = "firebrick1") +
  theme_bw() +
  labs(
    x = "",
    y = "Wastewater concentration (log)"
  ) +
  scale_x_date(
    date_labels = "%b %y"
  ) +
  facet_wrap(~treatment_plant,
             scales = "free_y")


ggsave("figs/ww_and_hosp_data_state.jpg", hosp_state + ww_state, height = 3, width = 7)


#Generate random date ranges
unique_dates <- unique(hosp_agg$admit_date)
#Remove all of those which are less than calibration_time + forecast_horizon from the end
unique_subset <- unique_dates[-c((length(unique_dates)-(1 + 90 + 28)):length(unique_dates))]
#Randomly sample
random_dates <- sample(unique_subset, 10)

date_df <- data.frame(
  start = random_dates,
  end = random_dates + 119
) %>%
  arrange(start) %>%
  mutate(
    y = seq(6, 60, by = 6)
  )

date_df$group <- 1:10


hosp_error <- hosp_state +
  geom_errorbarh(
    data = date_df,
    mapping = aes(
      xmin = start, 
      y = y, 
      xmax = end),
    lty = "solid",
    lineend = "round") 

ggsave("figs/hosp_data_state_errorbars.jpg", hosp_error, height = 3, width = 7)


#Set up to split by time 
all_plot <- sapply(1:nrow(date_df), function(x){
  
  ggplot() +
    geom_point(
      data = hosp_state_upd %>%
        filter(treatment_plant == "State total" &
                 admit_date >= date_df$start[x] & admit_date <= date_df$end[x]),
      mapping = aes(
        x = admit_date,
        y = count,
        group = treatment_plant
      ),
      color = "skyblue") +
    theme_bw() +
    labs(
      x = "",
      y = "Hospital admissions"
    ) +
    scale_x_date(
      # limits = range(hosp_state_upd$admit_date),
      date_labels = "%b %y"
    ) +
    scale_y_continuous(
      limits = c(0, 70)
    )

}, simplify = FALSE)

ggsave("figs/hosp_data_state_different_timeperiods.jpg", 
       wrap_plots(all_plot) +
         plot_layout(axis_titles = "collect"),
       height = 5, width = 9)

#Plot all substate plots
find_here <- here("figs", "date_shuffle", "substate_estimates")
estimate_data <- list.files(path = find_here, pattern = "_rawpredictions.csv", recursive = T, full.names = T)
estimate_data <- estimate_data[grepl("_of_10", estimate_data)]

#Import them all in
rawpred_all <- data.table::rbindlist(sapply(estimate_data, function(x){
  
  import(x) %>%
    mutate(site = strsplit(x, "/")[[1]][grepl("site", strsplit(x, "/")[[1]])],
           run = strsplit(x, "/")[[1]][grepl("run_", strsplit(x, "/")[[1]])],
           site = gsub("site_", "", site),
           run = gsub("run_|_of_10_rawpredictions.csv", "", run)) %>%
    filter(forecast_or_fit == "Forecast") %>%
    select(-total_pop, forecast_or_fit)

}, simplify = FALSE), fill = T)

#Giga table
scores_go <- rawpred_all %>%
  score() %>%
  summarise_scores(by = c("model", "site"))

scores_go_one <- rawpred_all %>%
  score() %>%
  summarise_scores(by = c("model"))

scores_go_gather <- scores_go %>%
  gather(mad:se_mean,
         key = "metric",
         value = "value")

scores_go_gather_upd <- scores_go_gather %>%
  group_by(site, metric) %>%
  mutate(difference = 100 * (value[model == "No wastewater"] - value[model == "Wastewater"])/value[model == "Wastewater"]) %>%
  filter(model == "Wastewater") %>%
  mutate(metric_full = case_when(
    metric == "mad" ~ "Mean absolute deviation",
    metric == "bias" ~ "Bias",
    metric == "dss" ~ "Dawid-Sebastiani score",
    metric == "crps" ~ "Ranked Probability score",
    metric == "ae_median" ~ "Absolute error (median)",
    metric == "se_mean" ~ "Standard error (mean)"
  )) %>%
  mutate(
    site = factor(site, levels = sort(as.numeric(unique(scores_go_gather$site))))
  )

#Generate overall plot
metric_better_plot <- ggplot(
  data = scores_go_gather_upd,
  mapping = aes(
    x = site,
    y = metric_full,
    fill = difference
  )
) +
  geom_tile() +
  labs("% wastewater improvement") +
  colorspace::scale_fill_continuous_divergingx(
    palette = 'RdBu', 
    mid = 0.0, 
    p1 = 0.5,
    p2 = 0.75,
    # l3 = 0,
    # p3 = .5,
    p4 = 1,
    rev = T
    ) +
  labs(x = "Site",
       y = "",
       fill = "% metric improvement\nfrom wastewater data") +
  theme_bw()


metric_better_plot

ggsave("figs/metric_better_plot_allsites.jpg", 
       metric_better_plot,
       height = 4, width = 9)



#Example compare
run1 <- import(here("figs", "date_shuffle", "nonspatial_v2", "individual_runs", "run_1_of_10_rawpredictions.csv"))
run10 <- import(here("figs", "date_shuffle", "nonspatial_v2", "individual_runs", "run_10_of_10_rawpredictions.csv"))


pred_sum <- rbind(run1 %>%
                    mutate(run = 1,
                           model = "One"), 
                  run10 %>%
                    mutate(run = 10,
                           model = "Two")) %>%
  group_by(
    date,
    model,
    forecast_or_fit,
    run
  ) %>%
  summarise(
    true_value = median(true_value),
    prediction_median = median(prediction),
    lower = quantile(prediction, 0.025),
    upper = quantile(prediction, 0.975)
  )

all_fit_go <- ggplot(
  data = pred_sum,
) +
  geom_ribbon(
    mapping = aes(
      x = date,
      ymin = lower,
      ymax = upper,
      fill = model
    ),
    alpha = 0.2
  ) +
  geom_point(
    mapping = aes(
      x = date,
      y = true_value
    ),
    fill = "white",
    color = "black",
    shape = 21,
    alpha = 0.25
  ) +
  geom_line(
    mapping = aes(
      x = date,
      y = prediction_median,
      color = model
    )
  ) +
  theme_minimal() +
  labs(x = "",
       y = "Hospitalizations",
       fill = "",
       color = "",
       title = "Model fits for different time-periods") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.position = "none"
  )

ggsave("figs/example_plots_compare.jpg", all_fit_go, width = 4, height = 3)

