
summarize_outputs <- function(
  #List of models
  model_list,
  #Processed data
  processed_data
  ){
  
  #Loop through each of the models
  model_looped_outputs <- sapply(1:length(model_list), function(x){
    
    #If wastewater data exists, extract
    does_ww_data_exist <- length(model_list[[x]]$stan_data_list$ww_sampled_lab_sites) != 0
    if(does_ww_data_exist){

      #Set up wastewater dataset
      ww_raw_data <- if(packageDescription("wwinference")$GithubRef == "spatial-main"){
        get_draws_df(model_list[[x]]) %>% filter(name == "predicted wastewater") %>%
          rename(
            site = lab
          )
      } else {
        get_draws(model_list[[x]])$predicted_ww
      }
      
      ww_draws <- ww_raw_data %>%
        rename(
          true_value = observed_value,
          prediction = pred_value,
          sample = draw
        ) %>%
        mutate(
          forecast_or_fit = case_when(
            date < processed_data$forecast_date ~ "Fit",
            date >= processed_data$forecast_date ~ "Forecast"
          ),
          model = names(model_list)[x]
        )
      
      #Re-enter missing data into wastewater draws
      #Extra step needed here because wastewater data is reported and evaluated at the site level
      ww_present_dates <- processed_data$ww_data_eval$date

      #Loop through and re-add data to the correct site and date
        for(j in unique(ww_draws$site)){
          this_wastewater_site <- processed_data$ww_data_eval %>% 
            filter(site == j)
          for(i in this_wastewater_site$date){
          ww_draws[ww_draws$site == j & ww_draws$date == i, ]$true_value <- this_wastewater_site %>% 
            filter(date == i) %>%
            pull(log_genome_copies_per_ml)
        }
        }
      
      summarised_wastewater_draws <- ww_draws %>%
        group_by(
          date,
          model,
          site
        ) %>%
        summarise(
          lower = quantile(prediction, 0.025, na.rm = TRUE),
          median = median(prediction),
          upper = quantile(prediction, 0.975, na.rm = TRUE),
          true_value = median(true_value),
          forecast_or_fit = unique(forecast_or_fit),
          .groups = "drop"
        )
      
    }
    
    #Get model diagnostics
    diagnostics <- get_model_diagnostic_flags(model_list[[x]]) %>%
      mutate(
        model = names(model_list)[x]
      )
    
    #Get model draws
    hosp_raw_data <- if(packageDescription("wwinference")$GithubRef == "spatial-main"){
      get_draws_df(model_list[[x]]) %>% filter(name == "predicted counts") %>%
        rename(
          site = lab
        )
    } else {
      get_draws(model_list[[x]])$predicted_counts
    }
    
    draws <- hosp_raw_data %>%
      mutate(
        model = names(model_list)[x]
      ) %>%
      rename(
        true_value = observed_value,
        prediction = pred_value,
        sample = draw
      ) %>%
      mutate(
        forecast_or_fit = case_when(
          date < processed_data$forecast_date ~ "Fit",
          date >= processed_data$forecast_date ~ "Forecast"
        )
      )
    
    #Re-enter missing data into draws
    dates_missing_data <- unique(draws$date[is.na(draws$true_value)])
    
    #Loop through and re-add
    for(i in dates_missing_data){
      draws[draws$date == i, ]$true_value <- processed_data$hosp_data_eval$daily_hosp_admits[processed_data$hosp_data_eval$date == i]
    }
    
   #Summarise draws for plotting
   summarised_draws <- draws %>%
      group_by(
        date,
        model
      ) %>%
      summarise(
        lower = quantile(prediction, 0.025, na.rm = TRUE),
        median = median(prediction),
        upper = quantile(prediction, 0.975, na.rm = TRUE),
        true_value = median(true_value),
        forecast_or_fit = unique(forecast_or_fit),
        .groups = "drop"
      )
   
   list(
     draws,
     summarised_draws,
     diagnostics,
     if(does_ww_data_exist == FALSE) NULL else ww_draws,
     if(does_ww_data_exist == FALSE) NULL else summarised_wastewater_draws
   )

  }, simplify = FALSE)
  
  #Unpack list
  combined_draws <- do.call(rbind, sapply(model_looped_outputs, function(x) x[[1]], simplify = FALSE))
  combined_summarised_draws <- do.call(rbind, sapply(model_looped_outputs, function(x) x[[2]], simplify = FALSE))
  combined_diagnostics <- do.call(rbind, sapply(model_looped_outputs, function(x) x[[3]], simplify = FALSE))
  combined_ww_draws <- do.call(rbind, sapply(model_looped_outputs, function(x) x[[4]], simplify = FALSE))
  combined_summarised_ww_draws <- do.call(rbind, sapply(model_looped_outputs, function(x) x[[5]], simplify = FALSE))
  
  list(
    combined_draws,
    combined_summarised_draws,
    combined_diagnostics,
    combined_ww_draws,
    combined_summarised_ww_draws
  )
  
}