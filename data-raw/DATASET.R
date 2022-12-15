## code to prepare `DATASET` dataset goes here

require(tidyr)
require(readr)
require(dplyr)
require(tibble)
require(readxl)

# Setup -------------------------------------------------------------------

if (.Platform$OS.type == "windows") {
  Sys.setlocale(category = "LC_ALL", "English_United States.1250")
}  else {
  Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
}

is_msmt_var <- function(x,
                        temp_expr = "^(r|R)(.{2})([0-9]{2}[A-Za-z]{0,1})([0-9]{1,2}[A-Za-z]{0,1})$"){

  output <- grepl(temp_expr, x)

  return(output)
}

get_msmt_inds <- function(x,
                          temp_expr = "^(r|R)(.{2})([0-9]{2}[A-Za-z]{0,1})([0-9]{1,2}[A-Za-z]{0,1})$"){
  tibble(item = x,
         tab_ind = gsub(temp_expr, "\\2", x),
         row_ind = gsub(temp_expr, "\\3", x),
         col_ind = gsub(temp_expr, "\\4", x)
  )

}

# Read data ---------------------------------------------------------------

msmt_forms <- tibble(form = c("M03", "M08", "M09", "M10", "R13", "R43",
                                 "S01", "S04", "S18", "S24", "S4c", "S51",
                                 "S53", "Z02", "Z14", "Z15", "Z17", "Z19",
                                 "Z23", "Z27", "Z33", "Z34")) %>%
  mutate(vykaz = substr(form, 2, 3))

var_desc <- read_xlsx('data-raw/var_desc.xlsx')

# Prepare data ------------------------------------------------------------

# Label list --------------------------------------------------------------

lab_data <- var_desc %>%
  select(vykaz,
         polozka,
         zkr) %>%
  mutate(polozka = tolower(polozka),
         zkr = ifelse(is.na(zkr),
                      polozka,
                      zkr)) %>%
  distinct() %>%
  left_join(msmt_forms, "vykaz") %>%
  select(-vykaz) %>%
  mutate_all(enc2utf8)

forms <- lab_data$form %>%
  unique() %>%
  sort()

labels <- lapply(forms, function(x){

  temp_data <- lab_data %>%
    filter(form == x)

  lab_vec <- temp_data$zkr %>%
    `names<-`(temp_data$polozka) %>%
    c("p_izo" = "Identifikátor místa",
      "year" = "Rok",
      .)}) %>%
  `names<-`(forms)

# Tables ------------------------------------------------------------------

temp_inds <- lab_data %>%
  filter(is_msmt_var(polozka)) %>%
  bind_cols(get_msmt_inds(.$polozka))

tables <- lapply(na.omit(unique(temp_inds$form)), function(x){

  temp_inds_2 <- temp_inds %>%
    filter(form == x)

  lapply(na.omit(unique(temp_inds_2$tab_ind)), function(y){
    output <- temp_inds_2 %>%
      filter(tab_ind == y) %>%
      select(zkr, row_ind, col_ind) %>%
      arrange(parse_number(col_ind)) %>%
      pivot_wider(names_from = col_ind,
                  values_from = zkr,
                  id_cols = row_ind) %>%
      arrange(parse_number(row_ind))
    return(output)
  }) %>%
    `names<-`(na.omit(unique(temp_inds_2$tab_ind)))
}) %>%
  `names<-`(na.omit(unique(temp_inds$form)))


# Browser -----------------------------------------------------------------

temp_labs <- temp_inds %>%
  group_by(form, tab_ind) %>%
  arrange(parse_number(col_ind)) %>%
  arrange(row_ind) %>%
  mutate(filt = 1:n())

browser <- lapply(na.omit(unique(temp_labs$form)),
                  function(x){
                    temp_tib_1 <- temp_labs %>%
                      filter(form == x)

                    temp_tib_1_filt <- temp_tib_1 %>%
                      filter(filt == 1)

                    temp_names_1 <- paste0(temp_tib_1_filt$zkr, "; etc. (tab.", temp_tib_1_filt$tab_ind,")")

                    output <- lapply(temp_tib_1_filt$tab_ind,
                                     function(y){

                                       temp_tib_2 <- temp_tib_1 %>%
                                         filter(tab_ind == y)

                                       temp_names_2 <- paste0(temp_tib_2$zkr, " (", temp_tib_2$item,")")

                                       temp_vec <- as.list(temp_tib_2$item) %>%
                                         `names<-`(temp_names_2)

                                       return(temp_vec)

                                     }) %>%
                      `names<-`(temp_names_1)

                    return(output)
                  }) %>%
  `names<-`(na.omit(unique(temp_labs$form)))

usethis::use_data(tables, browser, labels, msmt_forms, internal = TRUE, overwrite = TRUE)
