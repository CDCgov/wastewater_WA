WA_nonspatial_run <- function(
    #Raw hospital data as provided by WA
  raw_hospital_counts,
  #Raw wastewater data as provided by WA
  raw_ww_data,
  #Raw population data as provided by WA
  pop_data,
  #Sites that you want to run the model on, you can use "all" for all sites in the dataset, individual sites, or a vector of sites separate with ; (i.e. 2;13;22)
  sites = "all",
  #The population of Washington state
  WA_population = 7786000,
  #The number of days you want to forecast
  forecast_horizon = 28,                   
  #The number of days you are calibrating the model on
  calibration_time = 90,
  #The number of randomly selected date windows to run the model on
  runs = 5,
  #Specific runs you want to run the model on. For example if you had partly run the model on runs 1-4, but had stopped before running 5, you could set this as 5 and run just the 5th subset of the date windows. Primarily used for testing, or picking up larger jobs if your computer has crashed.
  repeat_subset = NA,
  #Savename - this dictates what your folders are called
  savename = "test",
  #The fit options specified by the function get_mcmc_options()
  fit_options,
  #Compiled model
  compiled_model
){
  
  
  # Subset data, create folders and data key --------------------------------
  
  #Set unique dates
  unique_dates <- unique(raw_hospital_counts$admit_date)
  #Remove all of those which are less than calibration_time + forecast_horizon from the end
  unique_subset <- unique_dates[-c((length(unique_dates)-(1 + calibration_time + forecast_horizon)):length(unique_dates))]
  #Randomly sample
  random_dates <- sample(unique_subset, runs)
  
  #Create dataframe and save so we know whats happened
  key <- data.frame(
    sites,
    forecast_horizon,
    calibration_time,
    runs,
    savename,
    random_dates
  )
  
  #Save folder for outputs
  folder_full <- paste0("output/model_runs/full_data/", savename, "/")
  folder_summary <- paste0("output/model_runs/summary/", savename, "/")
  
  if(!dir.exists(folder_full)) dir.create(folder_full, recursive = T)
  if(!dir.exists(folder_summary)) dir.create(folder_summary, recursive = T)
  
  #Write key
  write.csv(x = key,
            file = paste0(folder_summary, "key.csv"),
            row.names = FALSE)
  
  #Loop through dates
  these_runs <- 1:length(random_dates)
  
  if(!is.na(repeat_subset)){
    these_runs <- as.numeric(unlist(strsplit(repeat_subset, ";")))
  }
  
  # Loop through runs ----------------------------------------------------
  all_model_runs <- sapply(these_runs, function(x){
    
    #Set try to allow the model to keep running if an individual run fails
    try({
      
      #Time start
      time_start <- Sys.time()
      
      #Set dates of interest
      date_to_date <- seq.Date(random_dates[x], random_dates[x] + calibration_time + forecast_horizon, by = "days")    
      
      #Process data and compile model - custom function to do things neatly behind the scenes
      processed_data <- process_WA_data(
        #The raw hospital count data
        raw_hospital_counts = hospital_counts %>%
          filter(admit_date %in% date_to_date),
        #The raw wastewater data
        raw_ww_data = ww_data %>%
          filter(sample_date %in% date_to_date),
        #Population data
        pop_data = pop_data,
        #Sites - this can be specified as a single site,( = 1), as multiple sites linked by a ;, (= "1;2;3") or as all sites (= "all")
        sites = sites,
        #How far to forecast
        forecast_horizon = forecast_horizon
      )
      
      # Fit models --------------------------------------------------------------
      
      hosp_fit_only <- wwinference(
        #The processed wastewater data
        ww_data = processed_data$ww_data_fit,
        #The processed hospital data
        count_data = processed_data$hosp_data_fit,
        #The forecast date
        forecast_date = processed_data$forecast_date,
        #The calibration time
        calibration_time = calibration_time,
        #The forecast horizon
        forecast_horizon = processed_data$forecast_horizon,
        #The model specifications
        model_spec = get_model_spec(
          generation_interval = processed_data$generation_interval,
          inf_to_count_delay = processed_data$inf_to_hosp,
          infection_feedback_pmf = processed_data$infection_feedback_pmf,
          params = processed_data$params,
          include_ww = F
        ),
        #Fitting options
        fit_opts = fit_options,
        #Pre-compiled model
        compiled_model = compiled_model
      )
      
      ww_fit <- suppressWarnings(wwinference(
        #The processed wastewater data
        ww_data = processed_data$ww_data_fit,
        #The processed hospital data
        count_data = processed_data$hosp_data_fit,
        #The forecast date
        forecast_date = processed_data$forecast_date,
        #The calibration time
        calibration_time = calibration_time,
        #The forecast horizon
        forecast_horizon = processed_data$forecast_horizon,
        #The model specifications
        model_spec = get_model_spec(
          generation_interval = processed_data$generation_interval,
          inf_to_count_delay = processed_data$inf_to_hosp,
          infection_feedback_pmf = processed_data$infection_feedback_pmf,
          params = processed_data$params,
          include_ww = T
        ),
        fit_opts = fit_options,
        compiled_model = compiled_model
      ))
      
      # Process model outputs ---------------------------------------------------
      processed_outputs <- summarize_outputs(
        #List of models run - it is important to name them as this is used to identify which model run was carried out
        model_list = list(
          "No wastewater" = hosp_fit_only, 
          "Wastewater" = ww_fit
        ),
        #Data previously processed
        processed_data = processed_data
      )
      
      # Create figures and score models -----------------------------------------
      hospitalization_forecasts_object <- processed_outputs[[1]] %>%
        rename(
          observed = true_value,
          predicted = prediction,
          sample_id = sample
        ) %>%
        as_forecast_sample(
          forecast_unit = c(
            "date", "model", "forecast_or_fit"
          )
        )
      
      wastewater_forecasts_object <- processed_outputs[[4]] %>%
        rename(
          observed = true_value,
          predicted = prediction,
          sample_id = sample
        ) %>%
        as_forecast_sample(
          forecast_unit = c(
            "date", "model", "forecast_or_fit", "site"
          )
        ) 
      
      attr(hospitalization_forecasts_object, "metrics") <- c(
        "bias", "dss", "crps", "overprediction",
        "underprediction", "dispersion", "log_score",
        "mad", "ae_median", "se_mean"
      )
      
      attr(wastewater_forecasts_object, "metrics") <- c(
        "bias", "dss", "crps", "overprediction",
        "underprediction", "dispersion", "log_score",
        "mad", "ae_median", "se_mean"
      )
      
      
      #Score models on hospitalization forecasting
      hospitalization_forecasts <- hospitalization_forecasts_object %>%
        score() %>%
        summarise_scores(by = c("model",
                                "forecast_or_fit"))
      
      #Score model on wastewater forecasting
      wastewater_forecasts <- wastewater_forecasts_object %>%
        score() %>%
        summarise_scores(by = c("model", "forecast_or_fit"))
      
      #Plot hospitalization forecast
      hospitalization_forecast <- ggplot(
        data = processed_outputs[[2]],
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
        labs(fill = "") + 
        new_scale_fill() +
        geom_point(
          mapping = aes(
            x = date,
            y = true_value,
            fill = forecast_or_fit
          ),
          shape = 21,
          alpha = 0.75,
          size = .95,
        ) +
        scale_fill_manual(values=c("gray70", "white")) +
        geom_line(
          mapping = aes(
            x = date,
            y = median,
            color = model
          ),
          show.legend = F
        ) +
        theme_minimal() +
        labs(x = "",
             y = "Hospitalizations",
             fill = "",
             color = "",
             fill = "",
             title = "Model fits for different time-periods") +
        theme(
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          legend.position = "bottom"
        ) +
        geom_vline( 
          xintercept = processed_data$forecast_date,
          linetype = "dashed"
        )
      
      #Plot wastewater forecast
      wastewater_forecast <- ggplot(
        data = processed_outputs[[5]],
      ) +
        geom_ribbon(
          mapping = aes(
            x = date,
            ymin = lower,
            ymax = upper,
            fill = as.factor(site)
          ),
          alpha = 0.2,
          show.legend = F
        ) +
        labs(fill = "") + 
        new_scale_fill() +
        geom_point(
          mapping = aes(
            x = date,
            y = true_value,
            fill = forecast_or_fit
          ),
          shape = 21,
          alpha = 0.75,
          size = .95,
        ) +
        scale_fill_manual(values=c("gray70", "white")) +
        geom_line(
          mapping = aes(
            x = date,
            y = median,
            color = as.factor(site)
          ),
          show.legend = F
        ) +
        theme_minimal() +
        labs(x = "",
             y = "Wasterwater concentration",
             fill = "",
             color = "",
             fill = "",
             title = "Model fits for different time-periods") +
        theme(
          legend.position = "bottom"
        ) +
        geom_vline( 
          xintercept = processed_data$forecast_date,
          linetype = "dashed"
        ) +
        facet_wrap(
          ~site,
          scales = "free_y"
        )
      
      # Output individual runs --------------------------------------------------
      
      #Output model diagnostics
      write.csv(x = processed_outputs[[3]] %>%
                  mutate(
                    run = x
                  ),
                file = paste0(folder_full, "run_", x, "_of_", length(random_dates), "_diagnostics.csv"),
                row.names = FALSE)
      
      #Output full raw predictions for wastewater data - using fwrite and gzip compression to reduce filesize by 100x
      #however it means that you cant open the file directly in excel
      fwrite(x = processed_outputs[[1]] %>%
                  mutate(
                    run = x
                  ),
                file = paste0(folder_full, "run_", x, "_of_", length(random_dates), "_hosp_rawpredictions.csv"),
             compress = "gzip")
      
      #Output full raw predictions for wastewater data - using fwrite and gzip compression to reduce filesize by 100x
      #however it means that you cant open the file directly in excel
      fwrite(x = processed_outputs[[4]] %>%
                  mutate(
                    run = x
                  ),
                file = paste0(folder_full, "run_", x, "_of_", length(random_dates), "_ww_rawpredictions.csv"),
             compress = "gzip")
      
      #Output model scores
      write.csv(x = hospitalization_forecasts %>%
                  mutate(
                    run = x
                  ),
                file = paste0(folder_full, "run_", x, "_of_", length(random_dates), "_hosp_modelscore.csv"),
                row.names = FALSE)
      
      write.csv(x = wastewater_forecasts %>%
                  mutate(
                    run = x
                  ),
                file = paste0(folder_full, "run_", x, "_of_", length(random_dates), "_ww_modelscore.csv"),
                row.names = FALSE)
      
      #Output forecast figures
      ggsave(paste0(folder_full, "run_", x, "_of_", length(random_dates), "_hospforecast.jpg"), 
             hospitalization_forecast, 
             height = 4, 
             width = 7)
      
      ggsave(paste0(folder_full, "run_", x, "_of_", length(random_dates), "_wwforecast.jpg"), 
             wastewater_forecast, 
             height = 12, 
             width = 21)
      
      time_end <- Sys.time()
      
      #Output time taken
      write.csv(x = data.frame(run = x,
                               time = time_end - time_start),
                file = paste0(folder_full, "run_", x, "_of_", length(random_dates), "_timetaken.csv"),
                row.names = FALSE)
      
    })
    
  }, simplify = FALSE)
  
  
  # Combine run data --------------------------------------------------------
  
  #Load model diagnostics
  diagnostics <- list.files(folder_full, pattern = "diag", full.names = T)
  all_diag <- do.call(rbind, sapply(diagnostics[grepl(paste0("_of_", runs), diagnostics)], function(y){
    import(y)
  }, simplify = FALSE))
  
  #Load in overall performance
  predictions_hosp <- list.files(folder_full, pattern = "hosp_rawpredictions", full.names = T)
  all_rawpred_hosp <- do.call(rbind, sapply(predictions_hosp[grepl(paste0("_of_", runs), predictions_hosp)], function(y){
    import(y)
  }, simplify = FALSE))
  
  predictions_ww <- list.files(folder_full, pattern = "ww_rawpredictions", full.names = T)
  all_rawpred_ww <- do.call(rbind, sapply(predictions_ww[grepl(paste0("_of_", runs), predictions_ww)], function(y){
    import(y)
  }, simplify = FALSE))
  
  #Model score 
  #Model score 
  all_rawpred_hosp_object <- all_rawpred_hosp %>%
    rename(
      observed = true_value,
      predicted = prediction,
      sample_id = sample
    ) %>%
    as_forecast_sample(
      forecast_unit = c(
        "date", "model", "forecast_or_fit", "run"
      )
    ) 
  
  attr(all_rawpred_hosp_object, "metrics") <- c(
    "bias", "dss", "crps", "overprediction",
    "underprediction", "dispersion", "log_score",
    "mad", "ae_median", "se_mean"
  )
  
  model_score_raw_hosp <- all_rawpred_hosp_object %>%
    score() %>%
    summarise_scores(by = c("model", "forecast_or_fit")) %>%
    arrange(forecast_or_fit, model)
  
  #Now for wastewater
  all_rawpred_ww_object <- all_rawpred_ww %>%
    rename(
      observed = true_value,
      predicted = prediction,
      sample_id = sample
    ) %>%
    as_forecast_sample(
      forecast_unit = c(
        "date", "model", "forecast_or_fit", "run", "site"
      )
    ) 
  
  model_score_raw_ww <- all_rawpred_ww_object %>%
    score() %>%
    summarise_scores(by = c("site", "forecast_or_fit", "model")) %>%
    arrange(forecast_or_fit, site, model)
  
  model_score_raw_all <- all_rawpred_ww_object %>%
    score() %>%
    summarise_scores(by = c("forecast_or_fit", "model")) %>%
    mutate(site = "All") %>%
    arrange(forecast_or_fit, site, model)
  
  model_score_raw_ww <- rbind(
    model_score_raw_ww,
    model_score_raw_all
  )
  
  #What is the order of runs in time
  order_of_runs_date <- data.frame(run = all_rawpred_hosp$run, date = all_rawpred_hosp$date) %>%
    group_by(run) %>%
    summarise(date = min(date)) %>%
    arrange(date) %>%
    ungroup()
  
  #Plot all forecasts
  pred_sum_hosp <- all_rawpred_hosp %>%
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
    ) %>%
    mutate(
      run = factor(run, levels = order_of_runs_date$run)
    ) %>%
    group_by(run) %>%
    mutate(data_split = min(date[forecast_or_fit == "Forecast"]))
  
  #Plot hospitalization forecast
  hospitalization_forecast <- ggplot(
    data = pred_sum_hosp,
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
    labs(fill = "") + 
    new_scale_fill() +
    geom_point(
      mapping = aes(
        x = date,
        y = true_value,
        fill = forecast_or_fit
      ),
      shape = 21,
      alpha = 0.75,
      size = .95,
    ) +
    scale_fill_manual(values=c("gray70", "white")) +
    geom_line(
      mapping = aes(
        x = date,
        y = prediction_median ,
        color = model
      ),
      show.legend = F
    ) +
    theme_minimal() +
    labs(x = "",
         y = "Hospitalizations",
         fill = "",
         color = "",
         fill = "",
         title = "Model fits for different time-periods") +
    theme(
      legend.position = "bottom"
    ) +
    geom_vline( 
      mapping = aes(xintercept = data_split),
      linetype = "dashed"
    ) +
    facet_wrap(
      ~run, scales = "free"
    )
  
  # Output summary plots and statistics -------------------------------------
  
  #Output
  ggsave(paste0(folder_summary, "all_hospitalization_forecasts.jpg"), hospitalization_forecast, width = 10, height = 6)
  
  openxlsx::write.xlsx(x = as_tibble(model_score_raw_hosp),
                       file = paste0(folder_summary, "scoring_hospitalization.xlsx"))
  
  openxlsx::write.xlsx(x = as_tibble(model_score_raw_ww),
                       file = paste0(folder_summary, "scoring_wastewater.xlsx"))
  
  openxlsx::write.xlsx(x = as_tibble(all_diag),
                       file = paste0(folder_summary, "overall_model_diagnostics.xlsx"))  

}