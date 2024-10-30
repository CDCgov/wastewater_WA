#Install pacman if missing
if(!require(pacman)) install.packages("pacman")

#Load packages
pacman::p_load(
  rio,
  here,
  wwinference,
  tidyverse
)

#Import data
ww_data <- import(here("output", "example_data", "example_ww.csv")) %>%
  mutate(date = ymd(date))
hosp_data <- import(here("output", "example_data", "example_hosp.csv")) %>%
  mutate(date = ymd(date))

#Plot data - to show missingness/presence
first_date <- min(ww_data$date, hosp_data$date)
last_date <- max(ww_data$date, hosp_data$date)

#Specify fit options
fit_this <- get_mcmc_options(
  iter_warmup = 250,
  iter_sampling = 500,
  seed = 1
)

#Compile model
#These stan files are the default ones as part of the package, I just need to include this as a Windows workaround to bypass the space in "Program Files"
model <- wwinference::compile_model(
  model_filepath = "C:/wastewater_stan_files/wwinference.stan",         
  include_paths = "C:/wastewater_stan_files"
)

#Set up parameters
params <- get_params(
  system.file("extdata", "example_params.toml",
              package = "wwinference"
  )
)

#Run model
ww_fit <- wwinference(
  ww_data = ww_data,
  count_data = hosp_data,
  forecast_date = ("2023-04-08"),
  calibration_time = 90,
  forecast_horizon = 28,
  model_spec = get_model_spec(
    generation_interval = wwinference::default_covid_gi,
    inf_to_count_delay = wwinference::default_covid_inf_to_hosp,
    infection_feedback_pmf = wwinference::default_covid_gi,
    params = params,
    include_ww = T
  ),
  fit_opts = fit_this,
  compiled_model = model
)


#Code to generate plots
data_ww <- ww_data_to_fit_almost #ww_data$date
data_hosp <- hosp_data_clean #hosp_data$date

 

first_date <- min(data_ww$date, data_hosp$date)
last_date <- max(data_ww$date, data_hosp$date)

data_present <- rbind(
  data.frame(date = data_ww$date,
             present = "Yes",
             type = "Wastewater"),
  data.frame(date = data_hosp$date,
             present = "Yes",
             type = "Hospital")
) %>%
  complete(date = seq.Date(first_date, last_date, by = "days"),
           type,
           fill = list(present = "No"))

ggplot(
  data = data_present,
  mapping = aes(
    x = date,
    y = type,
    fill = as.factor(present)
  )
) +
  geom_tile() +
  theme_bw() +
  labs(
    x = "Date",
    y = "Data type",
    fill = "Data present?",
    title = "Availability of data by data type",
    subtitle = "0 indicates data is missing for that date, 1 indicates data is present"
  )

ggsave("figs/not_enough_data.jpg", height = 3, width = 7)

