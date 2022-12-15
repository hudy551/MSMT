msmt_names <- function(data_list,
                       var_sel = NULL,
                       data_labels = NULL){
  
  require(dplyr)
  require(tidyr)
  require(tibble)
  require(readr)
  
  if(is.null(data_labels)){
    data_labels <- msmt_labels()
  }
  if(is.null(var_sel)){
    var_sel <- colnames(data_list$wide)
  }
  
  lab_vec_0 <- data_labels[[data_list$form]]
  missing_vars <- var_sel[!var_sel %in% names(lab_vec_0)]
  missing_vec <- paste0("missing_name_", missing_vars) %>%
    `names<-`(missing_vars)
  lab_vec <- c(lab_vec_0, missing_vec)
  
  outputs <- data_list$wide %>%
    select(all_of(var_sel)) %>%
    `colnames<-`(lab_vec[colnames(.)])
  
  return(outputs)
}

msmt_names()

