
get_cor_mat <- function(model_list){
  
  #Set up names
  model_names <- if(is.null(names(model_list))) LETTERS[1:length(model_list)] else names(model_list)
  
  #Loop through different fits
  matrix_all <- do.call(rbind, sapply(1:length(model_list), function(x){
    
    #Extract and rearrange
    model_list[[x]]$fit$result$draws(
      variables = "non_norm_omega",
      format = "draws_array"
    ) %>%
      as.table() %>%
      as.data.frame() %>%
      mutate(
        Row = sub(
          "non_norm_omega\\[(\\d+),\\d+\\]",
          "Site \\1",
          variable
        ),
        Column = sub(
          "non_norm_omega\\[\\d+,(\\d+)\\]",
          "Site \\1",
          variable
        ),
        model_type = model_names[x]
      ) %>%
      select(
        -variable
      )

  }, simplify = FALSE))
  
  #Summarise and return
  matrix_all %>%
    group_by(Row,
             Column,
             model_type) %>%
    summarise(
      lower = quantile(Freq, 0.025, na.rm = TRUE),
      median = median(Freq),
      upper = quantile(Freq, 0.975, na.rm = TRUE),
      .groups = "drop"
    )
  
}