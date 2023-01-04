#' Replace variable names with intelligible labels
#'
#' @param data A tibble with msmt data.
#' @param form A single character, specifying which MSMT form the data belong to (see \code{msmt_forms()}).
#' @param language Language of output, currently available are Czech original ("cs") and english translation from DeepL ("en")
#' @param id_vec A character vector, containing the names of variables used as identifiers. Defaults to c("red_izo", "izo", "p_izo", "year"). Identifiers are not renamed.
#' @param var_sel A character vector, containing the names of variables to be renamed. If NULL (default), all variables will be renamed.
#' @param data_labels A named character vector, containing the labels to be assigned to variables, named with the respective variable names. If NULL (default), the default labels of \code{MSMT} package will be used.
#'
#' @return A tibble with renamed data
#'
#' @examples
#' data_locations <- msmt_download_data(form = "M03", years = 2015:2022)
#' data_list <- msmt_read_data(data_locations = data_locations)
#' named_tibble <- msmt_names(data = data_list$wide, form = data_list$form)
#'
#' @export

msmt_names <- function(data,
                       form,
                       language = "cs",
                       id_vec = c("red_izo",
                                  "izo",
                                  "p_izo",
                                  "year"),
                       var_sel = NULL,
                       data_labels = NULL){

  if(is.null(data_labels)){
    if(language == "cs"){
      data_labels <- MSMT:::msmt_labels_cs[[form]]
    }
    if(language == "en"){
      data_labels <- MSMT:::msmt_labels_en[[form]]
    }
  }
  if(is.null(var_sel)){
    var_sel <- colnames(data)
  }else{
    var_sel <- c(id_vec, var_sel)
  }

  missing_vars <- var_sel[!var_sel %in% names(data_labels)]
  missing_vec <- paste0("missing_name_", missing_vars) %>%
    `names<-`(missing_vars)
  lab_vec_0 <- c(data_labels, missing_vec)
  lab_vec <- paste0(lab_vec_0, " (",form,"; ", names(lab_vec_0) ,")") %>%
    `names<-`(names(lab_vec_0))

  lab_vec[id_vec] <- id_vec

  outputs <- data %>%
    select(all_of(var_sel)) %>%
    `colnames<-`(lab_vec[colnames(.)])

  return(outputs)
}
