#' Guess gender from name

#' This function guesses the assigned gender of a person from his/her name. Can be used in conjunction with msmt_addressess().
#'
#' To cite the database used in the function, use:
#' Martínez, G. L., de Juano-i-Ribes, H. S., Yin, D., Le Feuvre, B., Hamdan-Livramento, I., Saito, K., & Raffo, J. (2021). Expanding the World Gender-Name Dictionary: WGND 2.0.
#'  
#' @param names_vec A character vector containing full names (inclusing surnames)
#' @param match_char A character containing a regular expression indicatig female. Default is a Czech ending letter "á". 
#' To ommit, use "YYYY", or some other regular expression that won't produce any match.
#' @param countries Country codes to be used in matching names from the database. Note that adding too many countries might result in a very long computation
#' 
#' @return A tibble matching original names to guessed genders. Also includes variables indicating, 
#' how many names belonging to a gender in a given languages have been matched to the original name.
#' Also contains a dummy, indicating a match with the match character 'match_char'.
#' 
#' @examples 
#' data_addresses <- msmt_addresses()
#' data_genders <- msmt_gender_names(data_addresses$ReditelJmeno)

msmt_gender_names <- function(names_vec,
                              match_char = "á$",
                              countries = c("CZ", "SK", "PL")){
  
  require(readr)
  require(tibble)
  require(tidyr)
  require(dplyr)
  
  temp_file <- tempfile()
  
  download.file("https://dataverse.harvard.edu/api/access/datafile/4750348",
                destfile = temp_file,
                mode = "wb")
  
  tab_names <- read.table(temp_file,
                          header = TRUE,
                          encoding = "UTF-8") %>%
    as_tibble() %>%
    mutate(expr = name %>%
             paste0("^", ., "$|", 
                    "^", ., " |", 
                    " ", .," |"))
  
  temp_exprs <- lapply(countries,
                       function(x){
                         temp_tab <- tab_names %>%
                           filter(code == x)
                         
                         temp_male <- temp_tab %>%
                           filter(gender == "M") %>%
                           select(expr) %>%
                           unlist()
                         temp_female <- temp_tab %>%
                           filter(gender == "F") %>%
                           select(expr) %>%
                           unlist()
                         
                         list(temp_male, temp_female) %>%
                           `names<-`(paste0(x, c("_male", "_female")))
                       }) %>%
    unlist(recursive = FALSE)
  
  temp_names_0 <- names_vec %>%
    gsub("[ ]+", " ", .) %>%
    gsub("Th\\. D\\.", "thd\\.", .) %>%
    gsub("Mgr,", "Mgr.", .) %>%
    gsub("BcA", "bca", .) %>%
    gsub("bca ", "bca. ", .) %>%
    gsub("^.+\\. ([A-ZĚŠČŘŽÝÁÍÉŮÚŤĎĽ]{1})|,.+$", "\\1", .)
  
  temp_names <- temp_names_0 %>%
    unique() %>%
    tolower()
  
  n_expr <- sum(lengths(temp_exprs))
  ns_expr <- c(0, cumsum(lengths(temp_exprs)))
  total_1 <- length(temp_names) * sum(lengths(temp_exprs))
  
  progress_bar_1 <- txtProgressBar(min = 0, 
                                   max = total_1, 
                                   style = 3)
  
  temp_matches <- lapply(seq_along(temp_names), 
                         function(x){
                           lapply(seq_along(temp_exprs),
                                  function(y){
                                    lapply(seq_along(temp_exprs[[y]]), function(z){
                                      
                                      progress <- ((x-1)*n_expr) + (ns_expr[y]) + z 
                                      
                                      setTxtProgressBar(progress_bar_1, progress)
                                      
                                      output <- grepl(temp_exprs[[y]][z], temp_names[x])
                                      
                                      return(output)
                                    }) %>%
                                      unlist() %>%
                                      sum()
                                  }) %>%
                             `names<-`(names(temp_exprs))
                         }) %>%
    bind_rows()
  
  
  data_names_0 <- temp_matches %>%
    mutate(match_char_female = as.numeric(grepl(match_char, temp_names)),
           name = temp_names) %>%
    mutate(male_score = rowSums(select(., ends_with("_male"))),
           female_score = rowSums(select(., ends_with("_female"))) + match_char_female,
           gender = case_when(male_score > female_score ~ "male",
                              male_score < female_score ~ "female",
                              male_score == female_score ~ "N/A"))
  
  data_names <- tibble(
    name_original = names_vec,
    name_clean = temp_names_0) %>%
    mutate(name = tolower(name_clean)) %>%
    left_join(data_names_0, by = "name") %>%
    select(-name)
  
  return(data_names)
}