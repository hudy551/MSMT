#' Set native encoding to UTF-8
#'
#' For the session duration, \code{set_utf8} changes native encoding to UTF-8,
#' in order to accommodate special Czech characters
#' @export

set_utf8 <- function(){
  if (.Platform$OS.type == "windows") {
    Sys.setlocale(category = "LC_ALL", "English_United States.1250")
  }  else {
    Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
  }
}

#' Indicate, whether a tibble containing MSMT is in a long or a wide format
#'
#' This function can be used to determine the optimal mode of joining MSMT data tibbles.
#' For these purposes, data is considered to be long, whennever it either contains
#' more than one instance of every observed combination of indicator values per or, or
#' when at least one of the selected indicators is missing in the data.
#'
#' @param data A tibble contining MSMT data
#' @param id_vec A character vector containing variable names used for matching cases
#'
#' @importFrom dplyr group_by select all_of summarise distinct n
#'
#' @export


is_long <- function(data,
                    id_vec){

  if(all(id_vec %in% colnames(data))){

    outputs <- data %>%
      group_by(select(data, all_of(id_vec))) %>%
      summarise(cnt = n(),.groups = "drop") %>%
      select(all_of("cnt")) %>%
      distinct() %>%
      nrow() %>%
      `!=`(1)

  }else{
    outputs <- TRUE
  }
  return(outputs)
}

#' Construct a MSMT table with variable names
#'
#' This function builds a table of variable names that copies the cells of a variable table in MSMT forms.
#'
#' @param table_ind a single numeric or character, index of table, e.g. 1 or "01" or "01B"
#' @param row_inds a numeric or a character vector, containing indices of rows, e.g. c(1,2,3)
#' @param col_inds a numeric or a character vector, containing indices of columns, e.g. c(1,2,3)
#' @param extra_row a character vector, containing indices of special rows, e.g c("07a", "07b")
#' @param extra_col a character vector, containing indices of special columns, e.g c("07a", "07b")
#' @param omits full variable names that should be ommited (empty cells in a form table)
#' @param prefix a prefix for variable names, defaults to "r"
#' @param column_names assign column names to output table?
#' @param rows_names assign column names to output table?

#' @return A matrix replicating a form table, containing variable names
#' @examples
#' msmt_table_names(table_ind = "03",
#'                  row_inds = 1:7,
#'                  col_inds = 1:4,
#'                  extra_row = "07a",
#'                  omits = c("r03051",
#'                            "r03052"))
#'
#' @export

msmt_table_names <- function (table_ind,
                              row_inds,
                              col_inds,
                              extra_row = NULL,
                              extra_col = NULL,
                              omits = NULL,
                              prefix = "r",
                              column_names = NULL,
                              rows_names = NULL){

  row_inds <- as.numeric(row_inds)
  col_inds <- as.numeric(col_inds)
  if (is.numeric(table_ind)) {
    table_ind_mod <- ifelse(table_ind < 10, paste0(0, table_ind),
                            as.character(table_ind))
  }
  else {
    table_ind_mod <- table_ind
  }

  row_ind_mod <- c(ifelse(row_inds < 10, paste0(0, row_inds),
                          as.character(row_inds)), extra_row)

  col_ind_mod <- c(as.character(col_inds), extra_col)

  row_ind_ord <- tolower(row_ind_mod)

  for (i in 1:20) {
    temp_locs <- grepl(letters[i], row_ind_ord)
    row_ind_ord <- gsub(letters[i], "", row_ind_ord)
    row_ind_ord[temp_locs] <- as.numeric(row_ind_ord[temp_locs]) +
      (1/i)
  }
  row_ind_mod <- row_ind_mod[order(as.numeric(row_ind_ord))]
  col_ind_ord <- tolower(col_ind_mod)
  for (i in 1:20) {
    temp_locs <- grepl(letters[i], col_ind_ord)
    col_ind_ord <- gsub(letters[i], "", col_ind_ord)
    col_ind_ord[temp_locs] <- as.numeric(col_ind_ord[temp_locs]) +
      (1/i)
  }
  col_ind_mod <- col_ind_mod[order(as.numeric(col_ind_ord))]
  rows_r <- paste0("r", table_ind_mod, row_ind_mod)
  cols_r <- sapply(col_ind_mod, function(x) {
    paste0(rows_r, x)
  })
  colnames(cols_r) <- column_names
  rownames(cols_r) <- rows_names
  if (!is.null(omits)) {
    cols_r[cols_r %in% omits] <- NA
  }
  output <- tolower(cols_r)
  return(output)
}

#' Does a variable name match MSMT data name format?
#'
#' @param x a single character or a character vector, containing the variable name(s)
#' @param temp_expr a regex expression, containing MSMT variable name format
#'
#' @export


is_msmt_var <- function(x,
                        temp_expr = "^(r|R)(.{2})([0-9]{2}[A-Za-z]{0,1})([0-9]{1,2}[A-Za-z]{0,1})$"){

  output <- grepl(temp_expr, x)

  return(output)
}

#' Return a tibble, decomposing MSMT variable name into a table, row and column indices
#'
#' @param x a single character or a character vector, containing MSMT variable name(s)
#' @param temp_expr a regex expression, containing MSMT variable name format
#' @importFrom tibble tibble
#'
#' @export

get_msmt_inds <- function(x,
                          temp_expr = "^(r|R)(.{2})([0-9]{2}[A-Za-z]{0,1})([0-9]{1,2}[A-Za-z]{0,1})$"){
  tibble(item = x,
         tab_ind = gsub(temp_expr, "\\2", x),
         row_ind = gsub(temp_expr, "\\3", x),
         col_ind = gsub(temp_expr, "\\4", x)
  )

}
