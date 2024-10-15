
get_cor_mat <- function(model_fits,
                        model_names = NA){
  
  #Set up names
  model_names <- if(all(is.na(model_names))) LETTERS[1:length(model_fits)] else model_names
  
  #Loop through different fits
  matrix_all <- do.call(rbind, sapply(1:length(model_fits), function(x){
    
    #Extract and rearrange
    model_fits[[x]]$fit$result$draws(
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