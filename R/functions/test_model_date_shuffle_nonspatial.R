
test_model_date_shuffle_nonspatial <- function(
    raw_hospital_counts,
    raw_ww_data,
    pop_data,
    sites = "all",                           #If listing individual sites, combine with ; (i.e. 2;3;4)
    WA_population = 7786000,
    forecast_horizon = 28,                   #Number of days you are going to predict to
    calibration_time = 90,
    repeats = 5,
    repeat_subset = NA,
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
  folder <- paste0("figs/date_shuffle/", savename, "/individual_runs/")
  if(!dir.exists(folder)) dir.create(folder, recursive = T)
  
  #Write key
  write.csv(x = key,
            file = paste0(folder, "key.csv"),
            row.names = FALSE)

  #Loop through dates
  these_runs <- 1:length(random_dates)
  
  if(!is.na(repeat_subset)){
    these_runs <- as.numeric(unlist(strsplit(repeat_subset, ";")))
  }
  
  all_done <- do.call(rbind, sapply(these_runs, function(x){
    
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
    ww_fit <- suppressWarnings(wwinference(
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
    ))
    
    #Get model diagnostics
    ww_diag <- get_model_diagnostic_flags(ww_fit)
    hosp_diag <- get_model_diagnostic_flags(hosp_fit_only)
    
    combo_diag <- ww_diag %>%
      mutate(
        run = x,
        model_type = "Wastewater"
      ) %>%
      rbind(hosp_diag %>%
              mutate(
                run = x,
                model_type = "No wastewater"
              ))
    
    write.csv(x = combo_diag,
              file = paste0(folder, "run_", x, "_of_", length(random_dates), "_diagnostics.csv"),
              row.names = FALSE)
    
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
    
    #Nowcasting scores
    eval_data <- processed_data$hosp_data_eval %>%
      filter(date > processed_data$hosp_data_fit %>% 
               pull(date) %>% 
               max())
    
    for(i in eval_data$date){
      evaluate_data[which(evaluate_data$date == i), ]$true_value <- eval_data[which(eval_data$date == i), ]$daily_hosp_admits
    }
    
    #Score it up
    evaluate_data_upd <- evaluate_data %>%
      mutate(
        forecast_or_fit = case_when(
          date > max(processed_data$hosp_data_fit$date) ~ "Forecast",
          date <= max(processed_data$hosp_data_fit$date) ~ "Fit",
        ),
        run = x
      )
      
    model_score <- evaluate_data_upd %>%
      filter(date > max(processed_data$hosp_data_fit$date)) %>%
      score() %>%
      summarise_scores(by = "model")
    
    write.csv(x = model_score,
              file = paste0(folder, "run_", x, "_of_", length(random_dates), "_modelscore.csv"),
              row.names = FALSE)
    
    write.csv(x = evaluate_data_upd,
              file = paste0(folder, "run_", x, "_of_", length(random_dates), "_rawpredictions.csv"),
              row.names = FALSE)
    
    #Time end
    time_end <- Sys.time()

    write.csv(x = data.frame(run = x,
                             time = time_end - time_start),
              file = paste0(folder, "run_", x, "_of_", length(random_dates), "_timetaken.csv"),
              row.names = FALSE)
    
    message(paste0("Done ", x, " of ", length(random_dates)))
    
    })
    
  }, simplify = FALSE))
  
  #Load model diagnostics
  diagnostics <- list.files(folder, pattern = "diag", full.names = T)
  all_diag <- do.call(rbind, sapply(diagnostics[grepl(paste0("_of_", repeats), diagnostics)], function(y){
    import(y)
  }, simplify = FALSE))
  
  row.names(all_diag) <- NULL
  
  #Calculate overall performance
  predictions <- list.files(folder, pattern = "rawpredictions", full.names = T)
  
  all_rawpred <- do.call(rbind, sapply(predictions[grepl(paste0("_of_", repeats), predictions)], function(y){
    unlisted <- unlist(strsplit(y, "_"))
    import(y) %>%
      mutate(run = unlisted[which(unlisted == "of")-1])
  }, simplify = FALSE))
  
  row.names(all_rawpred) <- NULL
  
  #Model score 
  model_score_raw <- all_rawpred %>%
    score() %>%
    summarise_scores(by = c("model", "forecast_or_fit")) %>%
    rbind(all_rawpred %>%
            score() %>%
            summarise_scores(by = "model") %>%
            mutate(forecast_or_fit = "All")) %>%
    arrange(forecast_or_fit, model)
  
  #Calculate overall performance
  modelscores <- list.files(folder, pattern = "modelscore", full.names = T)
  all_runs <- do.call(rbind, sapply(modelscores[grepl(paste0("_of_", repeats), modelscores)], function(x){
    import(x)
  }, simplify = FALSE))
  
  #Difference
  sig_diff <- all_runs %>%
    tbl_summary(
      by = model,
      type = list(mad:se_mean ~ "continuous")
    ) %>%
    add_p() %>%
    bold_p()
  
  #What is the order of runs in time
  order_of_runs_date <- data.frame(run = all_rawpred$run, date = all_rawpred$date) %>%
    group_by(run) %>%
    summarise(date = min(date)) %>%
    arrange(date) %>%
    ungroup()

  
  #Plot all forecasts
  pred_sum <- all_rawpred %>%
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
    )
  
  pred_sum <- pred_sum %>%
    group_by(run) %>%
    mutate(data_split = min(date[forecast_or_fit == "Forecast"]))
  

  #Plot
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
    new_scale_fill() +
    geom_point(
      mapping = aes(
        x = date,
        y = true_value,
        fill = forecast_or_fit
      ),
      shape = 21,
      alpha = 0.25,
      size = .75,
    ) +
    scale_fill_manual(values=c("gray70", "white")) +
    geom_line(
      mapping = aes(
        x = date,
        y = prediction_median,
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
    facet_wrap(
      ~run,
      scales = "free"
    ) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      legend.position = "bottom"
    ) +
    geom_vline( 
      mapping = aes(xintercept = data_split),
      linetype = "dashed"
    )
  
  
  #Output
  ggsave(paste0(dirname(folder), "/all_forecasts_one_plot.jpg"), all_fit_go, width = 10, height = 6)
  
  gt::gtsave(as_gt(sig_diff), file = paste0(dirname(folder), "/overall_median_score.png"))
  
  openxlsx::write.xlsx(x = as_tibble(sig_diff),
                       file = paste0(dirname(folder), "/overall_median_score.xlsx"))
  
  openxlsx::write.xlsx(x = as_tibble(model_score_raw),
                       file = paste0(dirname(folder), "/overall_median_score_fromraw.xlsx"))
  
  openxlsx::write.xlsx(x = as_tibble(all_diag),
                       file = paste0(dirname(folder), "/overall_model_diagnostics.xlsx"))

}
