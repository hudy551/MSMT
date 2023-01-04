#' Guess highest level of education from titles included in name

#' This function guesses the highest level of education from titles recorded in full name (e.g. "Mgr."). Can be used in conjunction with \code{msmt_addressess()}.
#'
#' @param names_vec A character vector containing full names (inclusing surnames and titles)
#'
#' @return A tibble containing the original name and an ordered variable indication the guessed education, as well as a general idea of the observed title.
#'
#' @importFrom stringr str_match
#' @importFrom dplyr as_tibble mutate_all mutate select all_of everything bind_cols
#'
#' @examples
#' data_addresses <- msmt_addresses(NUTS3 = "CZ010")
#' data_titles <- msmt_titles(data_addresses$ReditelJmeno)
#'
#' @export

msmt_titles <- function(names_vec){

  temp_names <- names_vec %>%
    gsub("plk\\.Mgr\\.", "plk., Mgr.", .) %>%
    gsub("plk\\. g\u0161t\\.", "plkg\u0161t.", .) %>%
    gsub("Ing\\.Bc\\.", "Ing., Bc.", .) %>%
    gsub("Mgr\\.Bc\\.", "Mgr., Bc.", .) %>%
    gsub(" et| et\\.", ",", .) %>%
    gsub("(, [a-zA-Z]+\\.) ([a-zA-Z]+\\.)", "\\1\\2", .) %>%
    gsub("ak\\.|Ak\\.|Akad\\.|akad\\.", "Ak.", ., ignore.case = TRUE) %>%
    gsub("Ak. ", "Ak.", ., ignore.case = TRUE) %>%
    gsub(" a\\.| A\\.| art\\.", "a.", ., ignore.case = TRUE) %>%
    gsub("dipl. ", "dipl.", ., ignore.case = TRUE) %>%
    gsub("Sc", "sc", .) %>%
    gsub("Dr", "dr", .) %>%
    gsub("([a-zA-Z]{1})\\.([a-zA-Z]{1})[\\.]{0,1}([a-zA-Z]{0,1})", "\\1\\2\\3", .) %>%
    gsub("([A-Z]{2,}) |([A-Z]{2,})$", "\\1.", .) %>%
    gsub("([A-Z]{2,})$", "\\1.", .) %>%
    gsub("PhD\\.", "PhD", .) %>%
    gsub("PhD", "PhD.", .)

  temp_list <- list()
  mark <- 1
  tracker <- TRUE

  while(tracker){

    temp_list[[mark]] <- stringr::str_match(temp_names, "[a-zA-Z\\.]+\\. |[a-zA-Z\\.]+\\.$")[, 1] %>%
      gsub(" ", "", .)

    temp_names <- lapply(seq_along(names_vec),
                         function(x){gsub(gsub("\\.", "\\\\.", temp_list[[mark]][x]), "", temp_names[x])}) %>%
      unlist() %>%
      gsub("  ", " ", .) %>%
      gsub(",", "", .) %>%
      gsub("^ | $", "", .)

    tracker <- !sum(!is.na(temp_list[[mark]])) == 0

    mark <- mark + 1

  }

  title_tibble <- temp_list[-(mark - 1)] %>%
    `names<-`(paste0("t_", seq_along(.))) %>%
    as_tibble() %>%
    mutate_all(tolower)

  temp_levels <- MSMT:::title_level

  level_tibble <- lapply(temp_levels,
                         function(x){
                           lapply(1:ncol(title_tibble),
                                  function(y){unlist(title_tibble[,y]) %in% x}) %>%
                             `names<-`(names(title_tibble)) %>%
                             as_tibble() %>%
                             rowSums()
                         }) %>%
    `names<-`(paste0("level_", names(temp_levels))) %>%
    as_tibble()

  temp_areas <- MSMT:::title_area

  area_tibble <- lapply(temp_areas,
                         function(x){
                           lapply(1:ncol(title_tibble),
                                  function(y){unlist(title_tibble[,y]) %in% x}) %>%
                             `names<-`(names(title_tibble)) %>%
                             as_tibble() %>%
                             rowSums()
                         }) %>%
    `names<-`(paste0("area_", names(temp_areas))) %>%
    as_tibble()

  max_ed <- level_tibble %>%
    apply(MARGIN = 1, FUN = function(x){
      max(c(0, c(1:length(x))[x>0]))})

  ed_levels <- c("None", names(temp_levels))

  data_titles <- level_tibble %>%
    mutate(highest_education = ed_levels[max_ed + 1] %>%
             ordered(ed_levels),
           name = names_vec) %>%
    select(all_of("name"), everything()) %>%
    bind_cols(area_tibble)

  return(data_titles)
}
