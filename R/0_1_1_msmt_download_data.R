#' Downloads and creates local copies of requested MSMT data
#'
#' @param form String, indicating the id of a required form. See variable \code{form} in a tibble returned by \code{msmt_forms()}.
#' @param years A numeric or a character vector with requested years in format YYYY, defaults to the range 2015-2022
#' @return A named vector containing the temp directory locations of every successfully downloaded file
#' @examples
#' data_locations <- msmt_download_data(form = "S01", years = 2015:2022)
#'
#' @importFrom utils download.file txtProgressBar setTxtProgressBar
#'
#' @export

msmt_download_data <- function(form,
                               years = 2015:2022){

  if(!(is.character(form) & (length(form) == 1))){
    warning("Argument form must be a single character")
  }

  if(!form %in% MSMT:::msmt_forms_en$form){
    warning("Form code is not in the list of existing codes")
  }

  if(!(is.character(years)|is.numeric(years))){
    warning("Argument years must be a character or a numeric vector")
  }

  if(!all(nchar(years) == 4)){
    warning("All elements of the years argument must have four characters")
  }

  temp_dir <- tempdir()
  temp_filenames <- paste0(form,"_", years)
  temp_urls <- paste0(MSMT:::dsia, temp_filenames, ".xlsx")
  temp_files <- paste0(temp_dir, "\\", temp_filenames, ".xlsx")

  total_1 <- length(temp_urls)

  vec_success  <- lapply(seq_along(temp_files),
                         function(x){

                           ind_success <- tryCatch(expr = download.file(url = temp_urls[x],
                                                                        destfile = temp_files[x],
                                                                        quiet = TRUE,
                                                                        mode = "wb"),
                                                   warning = function(e){
                                                     warning(paste0("File ", temp_filenames[x], " not found online"))
                                                     return(1)
                                                   },
                                                   error = function(e){
                                                     warning(paste0("File ", temp_filenames[x], " not found online"))
                                                     return(1)
                                                   })

                           progress_bar_1 <- txtProgressBar(min = 0,
                                                            max = total_1,
                                                            style = 3)

                           progress <- x
                           setTxtProgressBar(progress_bar_1, progress)
                           cat("\n\n")

                           return(ind_success)

                         })

  names(temp_files) <- temp_filenames
  outputs <- temp_files[!unlist(vec_success)]

  return(outputs)
}
