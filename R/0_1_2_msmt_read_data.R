#' Reads and combines local copies of MSMT data

#' @param data_locations Named character vector with file locations of MSMT data in .xlsx format.
#' @param id_vec A character vector, containing the names of variables to be used as identifiers. Defaults to c("red_izo", "izo", "p_izo", "year").
#'
#' @details
#' \describe{
#' \item{data_locations}{Can be an output of \code{msmt_download_data()}. The characters contain file locations of .xlsx files with MSMT form data.
#' Names contain the form (F) and year (Y) of the data in the following format FFF_YYYY.
#' Only read data belonging to one form at a time (e.g. any time range from form S01).}
#' }
#' @return A named list, containing:
#' \describe{
#'    \item{form}{A single character, indicating MSMT form.}
#'    \item{wide}{A single tibble, containing merged data originally represented in a wide format.}
#'    \item{wide_id}{A single tibble, id data, such as addresses etc., detached from original wide format data.}
#'    \item{long}{A named list, containing separate tibbles of long data. Each tibble represents a separate excel sheet in the original data. The sheets are merged over years.}
#'    \item{row}{A named list, containing separate tibbles of all data. Each tibble represents a separate excel sheet in the original data.}
#' }
#'
#' @examples
#' data_locations <- msmt_download_data(form = "M03", years = 2015:2022)
#' data_list <- msmt_read_data(data_locations = data_locations)
#'
#' @importFrom dplyr group_by select all_of summarise distinct inner_join full_join bind_rows mutate mutate_if
#' @importFrom readxl excel_sheets read_excel
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @export
#'
msmt_read_data <- function(data_locations,
                           id_vec = c("red_izo",
                                      "izo",
                                      "p_izo",
                                      "year")){

  if(!(is.character(data_locations))){
    warning("Argument data_location must be a character vector")
  }

  temp_names <- names(data_locations)
  temp_forms <- gsub("_.+", "", temp_names)

  if(length(unique(temp_forms)) != 1){
    stop("Only data belonging to one form at a time can be processed")
  }

  temp_years <- gsub(".+_", "", temp_names)
  temp_sheets <- lapply(data_locations, function(x){readxl::excel_sheets(x)})
  temp_sheet_names <- lapply(temp_sheets,
                             function(x){
                               ifelse(grepl("_", x),
                                      gsub(".+_|.+_0", "", x),
                                      ifelse(grepl("^.+[0-9]{2}$", x),
                                             "pyr",
                                             substr(x, nchar(x), nchar(x)))) %>%
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
                                                     `names<-`(ifelse(names(.) == "", paste0("no_name_", 1:ncol(.)), names(.))) %>%
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

                                 temp_msmt_vars_0_0 <- names(temp_data_list_wide[[1]])
                                 temp_msmt_vars_0 <- temp_msmt_vars_0_0[is_msmt_var(temp_msmt_vars_0_0)]
                                 id_vec_filt_0 <- id_vec[id_vec %in% temp_msmt_vars_0_0]

                                 temp_data_wide <- temp_data_list_wide[[1]] %>%
                                   select(all_of(c(id_vec_filt_0, temp_msmt_vars_0)))

                                 temp_data_wide_ids <- temp_data_list_wide[[1]] %>%
                                   select(-all_of(temp_msmt_vars_0))

                                 if(length(temp_data_list_wide) > 1){

                                   for(i in 2:length(temp_data_list_wide)){

                                     temp_msmt_vars_1_0 <- names(temp_data_list_wide[[i]])
                                     temp_msmt_vars_1 <- temp_msmt_vars_1_0[is_msmt_var(temp_msmt_vars_1_0)]
                                     id_vec_filt_1 <- id_vec[id_vec %in% temp_msmt_vars_1_0]

                                     temp_data_add <- temp_data_list_wide[[i]] %>%
                                       select(all_of(c(id_vec, temp_msmt_vars_1)))

                                     temp_data_wide <- full_join(temp_data_wide,
                                                                 temp_data_add,
                                                                 id_vec)

                                     temp_data_wide_ids <- bind_rows(temp_data_wide_ids,
                                                                     temp_data_list_wide[[i]] %>%
                                                                       select(-all_of(temp_msmt_vars_1)))
                                   }
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

                               if(length(outputs) == 0){
                                 outputs = list()
                               }

                               return(outputs)
                             })

  temp_data_wide_0 <- lapply(temp_list_data_2,
                           function(x){
                             x[["data_wide"]]
                           }) %>%
    bind_rows() %>%
    mutate_if(is_msmt_var(names(.)), as.numeric)

  id_vec_filt_0 <- id_vec[id_vec %in% colnames(temp_data_wide_0)]

  temp_data_wide <- temp_data_wide_0 %>%
    select(all_of(id_vec_filt_0), everything())

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
                                    bind_rows() %>%
                                    mutate_if(grepl("^r[0-9]{1,2}", names(.)), as.numeric)
                                }) %>%
    `names<-`(sheet_types_long)

  outputs <- list(form = unique(temp_forms),
                  wide = temp_data_wide,
                  wide_id = temp_data_wide_ids,
                  long = temp_list_data_long,
                  raw = temp_list_data)

  return(outputs)
}
