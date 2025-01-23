
compile_model_upd <- function(custom_location = F, update_files = F){
  
  if(Sys.info()[[1]] == "Windows"){
    
    #Identify where files are found
    stan_location <- system.file("stan", "wwinference.stan", 
                                 package = "wwinference")
    
    #Do they include a space in the file path?
    if(grepl(" ", stan_location) & update_files == T){
      #Deconstruct path
      deconstructed_path <- unlist(strsplit(stan_location, "/"))
      #Reconstructed path
      reconstructed_path <- deconstructed_path[1:which(grepl(" ", deconstructed_path))-1]
      
      #Put back together above the portion with a 
      new_folder <- paste0(if(custom_location == T) custom_location else reconstructed_path, "/wastewater_stan_files/")
      
      #Delete directory if it exists
      unlink(new_folder, recursive = TRUE)
      #Recreate directory
      dir.create(new_folder)

      #List files to move
      these_files_to_move <- list.files(dirname(stan_location), full.names = T)
      
      #Move if missing
      if(length(list.files(new_folder)) != 0 & all(list.files(new_folder) %in% gsub("/", "", gsub(unique(dirname(these_files_to_move)), "", these_files_to_move)))){
        message("Files already present")
      } else {
        files_moved <- file.copy(from = these_files_to_move,
                                 to = new_folder,
                                 recursive = T)
        
        #Print message
        message(paste(these_files_to_move, " has been moved? ", files_moved, collapse = "\n"))
      }
      
      #List new file locations
      stan_file_new_location <- list.files(new_folder, 
                                           pattern = ".stan",
                                           full.names = T)
      
      #Compile model
      wwinference::compile_model(
        model_filepath = stan_file_new_location,         
        include_paths = dirname(stan_file_new_location)
      )

    } else {
      wwinference::compile_model()
    }
    
  } else {
    wwinference::compile_model()
  }
  
}