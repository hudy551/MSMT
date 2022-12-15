#' Reads and combines local copies of MSMT data

#' @param data_locations Named character vector with file locations of MSMT data in .xlsx format. 
#' Names contain the form (F) and year (Y) of the data in the following format FFF_YYYY. 
#' Can be also the output of msmt_download_data() function.
#' Only enter data from one form at a time.
#' 
#' @return A list with one tibble made from wide data format sheets and a list of tibbles made from wide data format sheets.
#' The tibbles combine all years, indicated by the original variable 'rok' and an added variable 'year'.
#' @examples 
#' data_locations <- msmt_download_data(form = "M03", years = 2015:2022)
#' data_list <- msmt_read_data(data_locations = data_locations)

msmt_read_data <- function(data_locations, 
                           id_vec = c("red_izo",
                                      "izo",
                                      "p_izo",
                                      "year"),
                           var_match = "^r[0-9]"){
  
  require(dplyr)
  require(tidyr)
  require(tibble)
  require(readxl)
  
  if(!(is.character(data_locations))){
    warning("Argument data_location must be a character vector")
  }

  temp_names <- names(data_locations)
  temp_forms <- gsub("_.+", "", temp_names)
  temp_years <- gsub(".+_", "", temp_names)
  temp_sheets <- lapply(data_locations, function(x){readxl::excel_sheets(x)})
  temp_sheet_names <- lapply(temp_sheets,
                             function(x){
                               ifelse(grepl("_", x), 
                                      gsub(".+_|.+_0", "", x), 
                                      substr(x, nchar(x), nchar(x))) %>%
                                 tolower() %>%
                                 paste0("sheet_", .)
                             }
  )
  
  total_1 <- sum(lengths(temp_sheets))
  
  cat("\nReading data\n")
  
  temp_list_data <- lapply(seq_along(data_locations), 
                           function(x){
                             temp_sets <- lapply(seq_along(temp_sheets[[x]]), 
                                                 function(y){
                                                   temp_sheet <- read_excel(path = data_locations[x],
                                                                            sheet = temp_sheets[[x]][y],
                                                                            .name_repair = tolower,
                                                                            col_types = "text") %>%
                                                     mutate(form = temp_forms[x],
                                                            year = temp_years[x])
                                                   
                                                   progress <- sum(lengths(temp_sheets)[(1:x)-1]) + y
                                                   progress_bar_1 <- txtProgressBar(min = 0, 
                                                                                    max = total_1, 
                                                                                    style = 3)
                                                   setTxtProgressBar(progress_bar_1, progress)
                                                   
                                                   return(temp_sheet)
                                                 }) %>%
                               `names<-`(temp_sheet_names[[x]])
                           }) %>%
    `names<-`(temp_names)
  
  total_2 <- length(temp_list_data)

  cat("\nProcessing\n")
  
  temp_list_data_2 <- lapply(seq_along(temp_list_data),
                             function(x){
                               
                               x2 <- temp_list_data[[x]]
                               
                               outputs <- list()
                               
                               temp_long <- sapply(x2,  
                                                   function(z){is_long(z, id_vec)})
                               
                               temp_data_list_wide <- x2[!temp_long]
                               temp_data_list_long <- x2[temp_long]
                               
                               if(length(temp_data_list_wide) > 0){
                                 
                                 temp_data_wide <- temp_data_list_wide[[1]] %>%
                                   select(all_of(id_vec), matches(var_match))
                                 
                                 temp_data_wide_ids <- temp_data_list_wide[[1]] %>%
                                   select(-matches(var_match))
                                 
                                 for(i in 2:length(temp_data_list_wide)){
                                   
                                   temp_data_add <- temp_data_list_wide[[i]] %>%
                                     select(all_of(id_vec), matches("^r[0-9]"))
                                   
                                   temp_data_wide <- dplyr::inner_join(temp_data_wide, 
                                                                       temp_data_add,
                                                                       id_vec)
                                   
                                   temp_data_wide_ids <- bind_rows(temp_data_wide_ids, 
                                                                     temp_data_list_wide[[i]] %>%
                                                                       select(-matches("^r[0-9]")))
                                 }
                                 
                                 outputs$data_wide <- temp_data_wide
                                 outputs$data_wide_ids <- temp_data_wide_ids %>%
                                   distinct()
                                 
                               }
                               
                               if(length(temp_data_list_long) > 0){
                                 outputs$data_long <- temp_data_list_long
                               }
                               
                               progress_2 <- x
                               progress_bar_2 <- txtProgressBar(min = 0, 
                                                                max = total_2, 
                                                                style = 3)
                               setTxtProgressBar(progress_bar_2, progress_2)
                               
                               
                               return(outputs)
                             })
  
  temp_data_wide <- lapply(temp_list_data_2,
                           function(x){
                             x[["data_wide"]]
                           }) %>%
    bind_rows()
  
  temp_data_wide_ids <- lapply(temp_list_data_2,
                           function(x){
                             x[["data_wide_ids"]]
                           }) %>%
    bind_rows() %>%
    distinct()
  
  sheet_types <- temp_sheet_names %>%
    unlist(recursive = TRUE) %>%
    unique() %>%
    sort()
  
  sheet_types_long <- sheet_types[grepl("_[a-z]+", sheet_types)]
  
  temp_list_data_long <- lapply(sheet_types_long,
                                function(x){
                                  lapply(temp_list_data_2,
                                         function(y){
                                           y[["data_long"]][[x]]
                                         }) %>%
                                    bind_rows()
                                }) %>%
    `names<-`(sheet_types_long)
  
  
  outputs <- list(form = unique(temp_forms),
                  wide = temp_data_wide,
                  wide_id = temp_data_wide_ids,
                  long = temp_list_data_long,
                  raw = temp_list_data)
  
  return(outputs)
}
