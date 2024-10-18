
test_model_date_shuffle_nonspatial <- function(
    raw_hospital_counts,
    raw_ww_data,
    pop_data,
    sites = "all",                           #If listing individual sites, combine with ; (i.e. 2;3;4)
    WA_population = 7786000,
    forecast_horizon = 28,                   #Number of days you are going to predict to
    calibration_time = 90,
    repeats = 5,
    savename = "test",
    fit_options){
  
  #Set unique dates
  unique_dates <- unique(raw_hospital_counts$admit_date)
  #Remove all of those which are less than calibration_time + forecast_horizon from the end
  unique_subset <- unique_dates[-c((length(unique_dates)-(1 + calibration_time + forecast_horizon)):length(unique_dates))]
  #Randomly sample
  random_dates <- sample(unique_subset, repeats)
  
  #Create dataframe and save so we know whats happened
  key <- data.frame(
    sites,
    forecast_horizon,
    calibration_time,
    repeats,
    savename,
    random_dates
  )
  
  #Save folder for outputs
  folder <- paste0("figs/date_shuffle/", savename, "/")
  if(!dir.exists(folder)) dir.create(folder)
  
  #Write key
  write.csv(x = key,
            file = paste0(folder, "key.csv"),
            row.names = FALSE)

  #Loop through dates
  all_done <- do.call(rbind, sapply(1:length(random_dates), function(x){
    
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
 
    message("Fitting model WITHOUT wastewater data")
    
    #No correlation structure
    hosp_fit_only <- wwinference(
      ww_data = processed_data$ww_data_fit,
      count_data = processed_data$hosp_data_fit,
      forecast_date = processed_data$forecast_date,
      calibration_time = calibration_time,
      forecast_horizon = processed_data$forecast_horizon,
      model_spec = get_model_spec(
        generation_interval = processed_data$generation_interval,
        inf_to_count_delay = processed_data$inf_to_hosp,
        infection_feedback_pmf = processed_data$infection_feedback_pmf,
        params = processed_data$params,
        include_ww = F
      ),
      fit_opts = fit_this,
      compiled_model = model
    )
    
    message("Fitting model WITH wastewater data")
    
    #Exponential fit
    ww_fit <- wwinference(
      ww_data = processed_data$ww_data_fit,
      count_data = processed_data$hosp_data_fit,
      forecast_date = processed_data$forecast_date,
      calibration_time = calibration_time,
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
    
    #Extract draws for analysis and plotting
    ww_draw <- get_draws(ww_fit)$predicted_counts
    hosp_draw <- get_draws(hosp_fit_only)$predicted_counts
    
    all_pred_draws_df <- rbind(
      ww_draw %>%
        mutate(model_type = "Wastewater"),
      hosp_draw %>%
        mutate(model_type = "No wastewater")
    )
    
    #Summarise for plot
    hosp_pred <- all_pred_draws_df %>%
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
    
    #Output
    ggsave(paste0(folder, "run_", x, "_of_", length(random_dates), "_hospforecast.jpg"), model_hosp_forecast, height = 4, width = 7)
    
    #All predictions
    evaluate_data <- all_pred_draws_df %>%
      rename(
        model = model_type,
        true_value = observed_value,
        prediction = pred_value,
        sample = draw
      )
    
    #Overall score
    score_models <- evaluate_data %>%
      filter(!is.na(true_value)) %>%
      score() %>%
      summarise_scores(by = "model")
    
    #Nowcasting scores
    eval_data <- processed_data$hosp_data_eval %>%
      filter(date > processed_data$hosp_data_fit %>% 
               pull(date) %>% 
               max())
    
    for(i in eval_data$date){
      evaluate_data[which(evaluate_data$date == i), ]$true_value <- eval_data[which(eval_data$date == i), ]$daily_hosp_admits
    }
    
    #Score it up
    model_score <- evaluate_data %>%
      filter(date > max(processed_data$hosp_data_fit$date)) %>%
      score() %>%
      summarise_scores(by = "model")
    
    write.csv(x = model_score,
              file = paste0(folder, "run_", x, "_of_", length(random_dates), "_modelscore.csv"),
              row.names = FALSE)
    
    #Time end
    time_end <- Sys.time()

    write.csv(x = data.frame(run = x,
                             time = time_end - time_start),
              file = paste0(folder, "run_", x, "_of_", length(random_dates), "_timetaken.csv"),
              row.names = FALSE)
    
    message(paste0("Done ", x, " of ", length(random_dates)))
    
    
  }, simplify = FALSE))
  
  #Calculate overall performance
  all_runs <- do.call(rbind, sapply(list.files(folder, pattern = "modelscore", full.names = T), function(x){
    import(x)
  }, simplify = FALSE))
  
  #Model score 
  mod_score_agg <-  all_runs %>%
    group_by(model) %>%
    summarise_all(median)
  
  #Difference
  sig_diff <- all_runs %>%
    tbl_summary(
      by = model
    ) %>%
    add_p() %>%
    bold_p()
  
  #Output
  gt::gtsave(as_gt(sig_diff), file = paste0(folder, "overall_median_score.png"))
  
  openxlsx::write.xlsx(x = as_tibble(sig_diff),
            file = paste0(folder, "overall_median_score.xlsx"))

}
