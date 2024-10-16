
test_model_date_shuffle <- function(
    raw_hospital_counts,
    raw_ww_data,
    pop_data,
    sites = "all",                           #If listing individual sites, combine with ; (i.e. 2;3;4)
    WA_population = 7786000,
    forecast_horizon = 28,                   #Number of days you are going to predict to
    spatial = NA,
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
      forecast_horizon = forecast_horizon,
      #Spatial data, if missing either omit this argument or set spatial = NA (the default) 
      spatial = facility_distance
    )
    
    message("Fitting model without spatial information")
    
    #Fit models
    #No spatial information
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
      fit_opts = fit_options,
      compiled_model = model
    )
    
    message("Fitting model without correlation structure")
    
    #No correlation structure
    ww_nocor_fit <- wwinference(
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
        include_ww = TRUE
      ),
      fit_opts = fit_options,
      compiled_model = model,
      dist_matrix = as.matrix(processed_data$dist_mat),
      corr_structure_switch = 0
    )
    
    message("Fitting model with exponential fit")
    
    #Exponential fit
    ww_exp_fit <- wwinference(
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
        include_ww = TRUE
      ),
      fit_opts = fit_options,
      compiled_model = model,
      dist_matrix = as.matrix(processed_data$dist_mat),
      corr_structure_switch = 1
    )
    
    message("Fitting model with LKJ distribution")
    
    #LKJ fit
    ww_lkj_fit <- wwinference(
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
        include_ww = TRUE
      ),
      fit_opts = fit_options,
      compiled_model = model,
      dist_matrix = as.matrix(processed_data$dist_mat),
      corr_structure_switch = 2
    )
    
    message("Getting draws, plotting and testing")
    
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
    
    #Output
    ggsave(paste0(folder, "run_", x, "_of_", length(random_dates), "_hospforecast.jpg"), model_hosp_forecast, height = 4, width = 7)
    
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
    
    #output
    ggsave(paste0(folder, "run_", x, "_of_", length(random_dates), "_modelcorrelations.jpg"), model_correlations, height = 4, width = 8)
    
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
              file = paste0(folder, "run_", x, "_of_", length(random_dates), "_modelscore.csv"),
              row.names = FALSE)
    
    message(paste0("Done ", x, " of ", length(random_dates)))
    

  }, simplify = FALSE))
  
}
