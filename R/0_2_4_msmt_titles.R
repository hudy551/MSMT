#' Guess highest level of education from titles included in name

#' This function guesses the highest level of education from titles recorded in full name (e.g. "Mgr."). Can be used in conjunction with msmt_addressess().
#'
#' @param names_vec A character vector containing full names (inclusing surnames and titles)
#' 
#' @return A tibble containing the original name and an ordered variable indication the guessed education.
#' 
#' @examples 
#' data_addresses <- msmt_addresses()
#' data_titles <- msmt_titles(data_addresses$ReditelJmeno)

msmt_titles <- function(names_vec){
  
  require(stringi)
  
  temp_titles_1 <- stringr::str_match(names_vec, "[a-zA-Z\\.]+\\.|[A-Z]{2,}")[, 1] %>% 
    unique() %>%
    na.exclude() %>%
    gsub("\\.", "\\\\.", .)
  
  temp_titles_2 <- names_vec %>%
    gsub(paste0("^", temp_titles_1, collapse = " |"), "", .) %>%
    stringr::str_match("[a-zA-Z\\.]+\\.") %>%
    .[, 1] %>% 
    unique() %>%
    na.exclude() %>%
    gsub("\\.", "\\\\.", .)
  
  temp_titles_0 <- c(temp_titles_1, temp_titles_2) %>%
    unique()
  
  temp_titles <- lapply(temp_titles_0, function(x){tibble(z = grepl(x, names_vec) %>% 
                                                            as.numeric()) %>% 
      `names<-`(gsub("\\\\", "", x))}) %>%
    bind_cols() %>%
    mutate(name = names_vec) %>%
    select(names(.)[!names(.)%in%c("A.", "P.", "S.", "v.z.", 
                                   "et.", "um.", "t.", "odst.", 
                                   "dipl.", "Ak.", "akad.", "pov.",
                                   "SM.", "art.", "Th.", "Arch.", "Ph.")]) %>%
    mutate(Spec = DiS.|ak.mal.|Dis.|dipl.um.|plk.|mal.,
           Bc = Bc.|BcA.|BA|B.A.,
           Mgr = Mgr.|Ing.|Mag.|MgrA.|Mgr.Bc.|Ing.Bc.|MBA|lic.|Mg.|M.Mus.|Ing.arch.|M.S.|M.A.|LL.M.|MBA.,
           Dr = PhDr.|PaedDr.|RNDr.|MVDr.|MUDr.|JUDr.|ThDr.,
           PhD = ThLic.|Dr.|Ph.D.|ArtD.|Th.D.|PhD.,
           Prof = doc.|Doc.|CSc.)
  
  max_ed <- temp_titles %>%
    select(Bc, Mgr, Dr, PhD, Prof) %>%
    apply(MARGIN = 1, FUN = function(x){
      max(c(0, c(1:5)[x]))})
  
  data_titles <- tibble(edu = case_when(max_ed == 0 ~ "No academic title",
                                        max_ed == 1 ~ "Bachelor's",
                                        max_ed %in% c(2, 3) ~ "Master's or equivalent",
                                        max_ed >= 4 ~ "PhD") %>%
                          ordered(c("No academic title", 
                                    "Bachelor's", 
                                    "Master's or equivalent", 
                                    "PhD")),
                        name = names_vec)
  
  return(data_titles)
}