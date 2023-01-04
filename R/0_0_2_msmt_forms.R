#' A function to browse and select MSMT datasets

#' @return A tibble with columns form (form codes)
#'
#' @param what What information would you like to receive. Defaults to "forms". See details for more
#' @param language Language of output, currently available are Czech original ("cs") and english translation from DeepL ("en")
#' @param search_string Optional. A perl regular expression containing a search term to use
#' @param exact_match Should an exact match, or an approximate match be used if parameter \code{what = "search"}. Defaults to true.
#' @param ... Additional arguments that can be passed to \code{agrepl()} if \code{exact_match = FALSE}.
#'
#' @details
#' \describe{
#' \item{what}{takes single character input, indicating, which information to return. Available options are:}
#' \describe{
#' \item{forms}{A tibble containing form codes, numbers, names, the year of the first and the currently last available form.}
#' \item{tables}{A nested named list, containing the reconstruction of form tables. This might be useful for interpreting some variable tables.}
#' \item{browser}{A nested named list divided into forms, tables in forms and variables in tables. Names in the list represent form names, illustrative first variable label of each table and variable labels inside the table. Elements of the list are variable names. This can be useful for brosing available variables using the '$' operator.}
#' \item{labels}{A nested named list of variable labels, divided into forms. The names in the list represent form codes and variable names. Can be used for labelling variables.}
#' \item{availability}{A tibble, showing detailed form availability in years.}
#' \item{search_string}{Used in combination with \code{search_string} parameter. Returns a tibble with instances of variables and their forms, matching the search term.}
#' }
#' }
#'
#' @importFrom tibble tibble
#' @importFrom dplyr select all_of
#'
#' @examples
#' \dontrun{form_tibble <- msmt_forms(what = "search", search_string = "znevýhodnění")}
#'
#' @export

msmt_forms <- function(what = "forms",
                       language = "cs",
                       search_string = NULL,
                       exact_match = TRUE,
                       ...){

  if(language == "cs"){
    MSMT::set_utf8()
  }


  if(length(what) > 1){
    warning("Parameter 'what' contains multiple elements, using the first")
    what <- what[1]
  }

  if(!what %in% c("forms",
                  "tables",
                  "browser",
                  "labels",
                  "availability",
                  "search")){
    stop("Argument 'what' must be one of the following: 'forms', 'tables', 'browser', 'labels', 'availability', 'search'.")
  }

  if(!language %in% c("cs", "en")){
    stop("Argument 'what' must be one of the following: 'cs', 'en'.")
  }

  if((length(search_string) < 1) & what == "search"){
    stop("'search_string' is missing with no default")
  }

  if(length(search_string) > 1){
    search_string <- paste0(search_string, collapse = "|")
  }

  if(what == "forms"){
    if(language == "cs"){
      output <- MSMT:::msmt_forms_cs
    }
    if(language == "en"){
      output <- MSMT:::msmt_forms_en
    }
  }

  if(what == "tables"){
    if(language == "cs"){
      output <- MSMT:::msmt_tables_cs
    }
    if(language == "en"){
      output <- MSMT:::msmt_tables_en
    }
  }

  if(what == "browser"){
    if(language == "cs"){
      output <- MSMT:::msmt_browser_cs
    }
    if(language == "en"){
      output <- MSMT:::msmt_browser_en
    }
  }

  if(what == "labels"){
    if(language == "cs"){
      output <- MSMT:::msmt_labels_cs
    }
    if(language == "en"){
      output <- MSMT:::msmt_labels_en
    }
  }

  if(what == "availability"){
    output <- MSMT:::msmt_forms_availability
  }

  if(what == "search"){
    if(language == "cs"){
      temp_output <- MSMT:::msmt_inds %>%
        select(-all_of(c("eng_name",
                       "eng_zkr")))
    }
    if(language == "en"){
      temp_output <- MSMT:::msmt_inds %>%
        mutate(name = eng_name,
               zkr = eng_zkr) %>%
        select(-all_of(c("eng_name",
                       "eng_zkr")))
    }
    if(exact_match){
      output <- temp_output %>%
        filter(grepl(search_string,
                     zkr))
    }
    if(!exact_match){
      output <- temp_output %>%
        filter(agrepl(search_string,
                      zkr,
                      ...))
    }
  }
  return(output)
}
