#' Returns variable labels (in czech)

#' @param what a character vector indicating, what format of labels should be returned.
#' "labels" stands for a list of named vectors,
#' "tables" for a list of tables, imitating the format of original MSMT forms
#' @param forms a character vector containing the specification of MSMT forms, if NULL, return all forms
#' @param tabs a named list specification of tables in MSMT forms, if NULL, return all tables in all forms
#'
#' @return A list containing (depending on "what" parameter):
#' a list of named lists under "labels". Lists are named according to forms. These lists contain named vectors with labels. Their names correspond to MSMT variables.
#' a list of named lists under "tables". Lists are named according to forms. These lists contain tibbles with labels, similar to tables in MSMT
#' a list of named lists under "browser". Lists are named according to forms. These lists contain list named with table labels, each containing named lists, where the name of each list is the variable label and content is variable name. Can be useful for browsing available data using str()
#'
#' @examples
#' forms_selection <- c("S01", "M03")
#' tables_selection <- list(S01 = c("02", "03", "08"))
#' data_tables <- msmt_labels(what = "tables", forms = "S01")

msmt_labels <- function(what = c("labels",
                                 "tables",
                                 "browser"),
                        forms_selection = NULL,
                        tables_selection = NULL){

  outputs <- list()

  if(!is.null(forms_selection)){
    if(!is.null(tables_selection)){
      forms_absent <- forms_selection[!forms_selection %in% names(tables_selection)]
      tables_add <- lapply(forms_absent, function(x){
        names(tables[[x]])
      }) %>%
        `names<-`(forms_absent)

    tables_selection <- c(tables_selection, tables_add)
    }
  }

  if("labels" %in% what){
    if(!is.null(forms_selections)){
      temp_labels <- MSMT:::labels[forms_selections]
    }else{
      temp_labels <- MSMT:::labels
    }
  }

  if("tables" %in% what){
    if(!is.null(forms_selections)){
      temp_tables <- MSMT:::tables[forms_selections]
    }else{
      temp_tables <- MSMT:::tables
    }
    if(!is.null(tables_selection)){
      outputs$tables <- lapply(seq_along(tables_selection),
                               function(x){temp_tables[[names(tables_selection)[x]]][tables_selection[[x]]]})
    }
  }
  return(outputs)
}
