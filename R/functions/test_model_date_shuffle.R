
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
  folder <- paste0("figs/date_shuffle/", savename, "/individual_runs/")
  if(!dir.exists(folder)) dir.create(folder, recursive = T)
  
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
      forecast_horizon = forecast_horizon,
      #Spatial data, if missing either omit this argument or set spatial = NA (the default) 
      spatial = facility_distance
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
    
    #Get model diagnostics
    nocor_diag <- get_model_diagnostic_flags(ww_nocor_fit)
    exp_diag <- get_model_diagnostic_flags(ww_exp_fit)
    lkj_diag <- get_model_diagnostic_flags(ww_lkj_fit)
    
    combo_diag <- rbind(
      nocor_diag %>%
        mutate(
          run = x,
          model_type = "no_cor",
        ),
      exp_diag %>%
        mutate(
          run = x,
          model_type = "exp",
        ),
      lkj_diag %>%
        mutate(
          run = x,
          model_type = "lkj",
        )
    )

    write.csv(x = combo_diag,
              file = paste0(folder, "run_", x, "_of_", length(random_dates), "_diagnostics.csv"),
              row.names = FALSE)
    
    #Get draws
    ww_draw_nocor <- get_draws_df(ww_nocor_fit)
    ww_draw_exp <- get_draws_df(ww_exp_fit)
    ww_draw_lkj <- get_draws_df(ww_lkj_fit)
    
    all_pred_draws_df <- rbind(
      ww_draw_nocor %>%
        mutate(model_type = "no_cor"),
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
    cor_mat <- get_cor_mat(model_fit = list(ww_nocor_fit, ww_exp_fit, ww_lkj_fit),
                           model_names = c("no_correlation", "exp", "lkj"))
    
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
    
    #All predictions
    all_pred_draws_df <- rbind(
      ww_draw_nocor %>%
        filter(name == "predicted counts") %>%
        mutate(
          model = "no_cor"
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
    
    #Nowcasting scores
    eval_data <- processed_data$hosp_data_eval %>%
      filter(date > processed_data$hosp_data_fit %>% 
               pull(date) %>% 
               max())
    
    for(i in eval_data$date){
      all_pred_draws_df[which(all_pred_draws_df$date == i), ]$true_value <- eval_data[which(eval_data$date == i), ]$daily_hosp_admits
    }
    
    #Score it up
    evaluate_data_upd <- all_pred_draws_df %>%
      mutate(forecast_or_fit = case_when(
        date > max(processed_data$hosp_data_fit$date) ~ "Forecast",
        date <= max(processed_data$hosp_data_fit$date) ~ "Fit",
      ))
    
    model_score <- evaluate_data_upd %>%
      filter(date > max(processed_data$hosp_data_fit$date)) %>%
      score() %>%
      summarise_scores(by = "model")
    

    #Output
    write.csv(x = model_score,
              file = paste0(folder, "run_", x, "_of_", length(random_dates), "_modelscore.csv"),
              row.names = FALSE)
    
    write.csv(x = evaluate_data_upd %>% 
                select(where(~!all(is.na(.x)))) %>%
                select(-c(name, observation_type, type_of_quantity, total_pop)),
              file = paste0(folder, "run_", x, "_of_", length(random_dates), "_rawpredictions.csv"),
              row.names = FALSE)

    #Time end
    time_end <- Sys.time()

    write.csv(x = data.frame(run = x,
                             time = time_end - time_start),
              file = paste0(folder, "run_", x, "_of_", length(random_dates), "_timetaken.csv"),
              row.names = FALSE)
    
    message(paste0("Done ", x, " of ", length(random_dates)))
    
  }, simplify = FALSE))
  
  #Load model diagnostics
  all_diag <- do.call(rbind, sapply(list.files(folder, pattern = "diag", full.names = T), function(y){
    import(y)
  }, simplify = FALSE))
  
  row.names(all_diag) <- NULL
  
  #Calculate overall performance
  all_rawpred <- do.call(rbind, sapply(list.files(folder, pattern = "rawpredictions", full.names = T), function(y){
    import(y) %>%
      mutate(run = unlist(strsplit(y, "_"))[6])
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
  all_runs <- do.call(rbind, sapply(list.files(folder, pattern = "modelscore", full.names = T), function(x){
    import(x)
  }, simplify = FALSE))
  
  row.names(all_runs) <- NULL
  
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
  ggsave(paste0(dirname(folder), "/all_forecasts_one_plot.jpg"), all_fit_go, width = 14, height = 7)
  
  gt::gtsave(as_gt(sig_diff), file = paste0(dirname(folder), "/overall_median_score.png"))
  
  openxlsx::write.xlsx(x = as_tibble(sig_diff),
                       file = paste0(dirname(folder), "/overall_median_score.xlsx"))
  
  openxlsx::write.xlsx(x = as_tibble(model_score_raw),
                       file = paste0(dirname(folder), "/overall_median_score_fromraw.xlsx"))
  
  openxlsx::write.xlsx(x = as_tibble(all_diag),
                       file = paste0(dirname(folder), "/overall_model_diagnostics.xlsx"))
  

}
