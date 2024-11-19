## code to prepare `DATASET` dataset goes here
# devtools::install_github("zumbov2/deeplr")

require(deeplr)
require(tidyr)
require(readr)
require(dplyr)
require(tibble)
require(readxl)
require(pdftools)
require(httr)
require(rvest)

# Setup -------------------------------------------------------------------

update_translation <- FALSE

my_key <- "536d512c-2e60-8cda-fe63-e873327d8f71:fx"

dsia <- "https://dsia.msmt.cz/vystupy/region/data/"
rej <- "https://rejstriky.msmt.cz/opendata/"
ruj <- "https://vdp.cuzk.cz/vymenny_format/csv/20220131_OB_ADR_csv.zip"
res <- "https://opendata.czso.cz/data/od_org03/res_data.csv"
name_data <- "https://dataverse.harvard.edu/api/access/datafile/4750348"

if (.Platform$OS.type == "windows") {
  Sys.setlocale(category = "LC_ALL", "English_United States.1250")
}  else {
  Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
}

is_msmt_var_0 <- function(x,
                        temp_expr = "^(r|R)(.{2})([0-9]{2}[A-Za-z]{0,1})([0-9]{1,2}[A-Za-z]{0,1})$"){

  output <- grepl(temp_expr, x)

  return(output)
}

get_msmt_inds_0 <- function(x,
                          temp_expr = "^(r|R)(.{2})([0-9]{2}[A-Za-z]{0,1})([0-9]{1,2}[A-Za-z]{0,1})$"){
  tibble(item = x,
         tab_ind = gsub(temp_expr, "\\2", x),
         row_ind = gsub(temp_expr, "\\3", x),
         col_ind = gsub(temp_expr, "\\4", x)
  )

}

# Read data ---------------------------------------------------------------
this_year <- Sys.time() %>%
  format("%Y") %>%
  as.numeric()

if(as.numeric(format(Sys.time(), "%m")) < 10){
  this_year <- this_year - 1
}

dsia_list <- lapply(2015:this_year-1, function(z){

  data_dsia <- httr::GET(paste0("https://dsia.msmt.cz/vystupy/region/vu_region", z,".html")) %>%
    rvest::read_html() %>%
    rvest::html_table(header = TRUE) %>%
    .[[1]] %>%
    mutate(url = paste0("https://dsia.msmt.cz/vystupy/region/pokyny/", pokyny))

  titles <- lapply(1:nrow(data_dsia),
                   function(x){
                     con <- data_dsia$url[[x]]

                     suppressWarnings({output_0 <- tryCatch({pdftools::pdf_text(con)[[1]]},
                                                            error = function(e){"N/A"})})

                     if(output_0 == "N/A"){
                       output <- "N/A"
                     }else{
                       output <- output_0 %>%
                         gsub("[ \t]+", " ", .) %>%
                         gsub("[\n]+", "\n", .) %>%
                         strsplit("\n") %>%
                         unlist() %>%
                         .[2:(grep("podle stavu|k [0-9]{1,2}\\.", .)[1] - 1)] %>%
                         paste0(collapse = " ") %>%
                         gsub("[ \t]+", " ", .) %>%
                         gsub("^[ \t]+|[ \t]+$", "", .)
                     }

                     return(output)

                   }) %>%
    unlist()

  output <- data_dsia %>%
    mutate(name = titles,
           year = z) %>%
    rename(form = výkaz) %>%
    select(form, name, year)

})

msmt_forms_availability <- dsia_list %>%
  bind_rows() %>%
  select(-name) %>%
  mutate(av = 1) %>%
  pivot_wider(names_from = year, values_from = av, id_cols = form, values_fill = 0)

msmt_forms_0 <- dsia_list %>%
  bind_rows() %>%
  group_by(form) %>%
  mutate(first = min(year),
         last = max(year),
         mark = n():1) %>%
  filter(mark == 1) %>%
  ungroup() %>%
  select(-c(mark, year)) %>%
  arrange(parse_number(form)) %>%
  mutate(vykaz = substr(form, 2, 3)) %>%
  select(form, vykaz, everything())

var_desc_0 <- read_xlsx('data-raw/var_desc.xlsx')

if((!"translation.rds" %in% list.files("data-raw")) & (!update_translation)){

  form_eng <- deeplr::translate2(text = msmt_forms_0$name,
                                 target_lang = "EN",
                                 auth_key = my_key)

  msmt_forms <- msmt_forms_0 %>%
    mutate(eng = form_eng)

  var_cs_0 <- var_desc_0$zkr %>%
    ifelse(is.na(.), "N/A", .) %>%
    tibble(var_0 = .) %>%
    mutate(id = 1:n()) %>%
    group_by(var_0) %>%
    mutate(id_2 = id[1]) %>%
    ungroup()

  var_cs <- var_cs_0 %>%
    select(var_0, id_2) %>%
    distinct()

  inds_trans <- seq(1,length(var_cs$var_0), 40)

  trans_list <- lapply(1:(length(inds_trans)-1),
                       function(x){

                         ind_vec <- inds_trans[x]:inds_trans[x+1]

                         vals_cs <- var_cs$var_0[ind_vec]

                         vals_eng <- deeplr::translate2(text = vals_cs,
                                                        target_lang = "EN",
                                                        auth_key = my_key)

                         print(paste0(x, "/", length(inds_trans) - 1))

                         output <- tibble(ind = ind_vec,
                                          cs = vals_cs,
                                          eng = vals_eng)

                         Sys.sleep(3)

                         return(output)
                       })


  trans_tib <- bind_rows(trans_list) %>%
    select(-ind) %>%
    group_by(cs) %>%
    filter((1:n()) == 1) %>%
    ungroup()

  trans <- list(form = msmt_forms,
                labs = trans_tib)

  write_rds(trans,
            file = "data-raw/translation.rds")

}else{
  trans <- read_rds("data-raw/translation.rds")
  msmt_forms <- trans$form
  trans_tib <- trans$labs
}

msmt_forms_cs <- msmt_forms %>%
  select(-eng)

msmt_forms_en <- msmt_forms %>%
  mutate(name = eng) %>%
  select(-eng)

var_desc <- var_desc_0 %>%
  left_join(trans_tib, c("zkr" = "cs")) %>%
  arrange(parse_number(vykaz))

# Prepare data ------------------------------------------------------------

# Label list --------------------------------------------------------------

lab_data <- var_desc %>%
  select(vykaz,
         polozka,
         zkr,
         eng) %>%
  mutate(polozka = tolower(polozka),
         zkr = ifelse(is.na(zkr),
                      polozka,
                      zkr)) %>%
  distinct() %>%
  left_join(msmt_forms %>%
              select(-first, -last),suffix = c("_zkr", "_name"), "vykaz") %>%
  mutate(form = ifelse(vykaz == "4c", paste0("S", vykaz), form)) %>%
  select(-vykaz) %>%
  mutate_all(enc2utf8)

forms <- msmt_forms$form %>%
  unique()

msmt_labeler <- function(x, lang = "cs"){

  temp_data <- lab_data %>%
    filter(form == x)

  temp_appends <- c(
    "p_izo" = "Identifikátor místa",
    "year" = "Rok")

  if(lang == "en"){
    temp_data <- temp_data %>%
      mutate(zkr = eng_zkr)

    temp_appends <- c(
      "p_izo" = "Site identifier",
      "year" = "Year")

  }

  lab_vec <- temp_data$zkr %>%
    `names<-`(temp_data$polozka) %>%
    c(temp_appends,
      .)}

msmt_labels_cs <- lapply(forms, function(x){msmt_labeler(x)}) %>%
  `names<-`(forms)

msmt_labels_en <- lapply(forms, function(x){msmt_labeler(x, lang = "en")}) %>%
  `names<-`(forms)

# Tables ------------------------------------------------------------------

msmt_inds <- lab_data %>%
  filter(is_msmt_var_0(polozka)) %>%
  bind_cols(get_msmt_inds_0(.$polozka))

msmt_tabler <- function(x, lang = "cs"){

  msmt_inds_2 <- msmt_inds %>%
    filter(form == x)

  if(lang == "en"){
    msmt_inds_2 <- msmt_inds_2 %>%
      mutate(zkr = eng_zkr)
  }

  lapply(na.omit(unique(msmt_inds_2$tab_ind)), function(y){
    output <- msmt_inds_2 %>%
      filter(tab_ind == y) %>%
      select(zkr, row_ind, col_ind) %>%
      arrange(parse_number(col_ind)) %>%
      pivot_wider(names_from = col_ind,
                  values_from = zkr,
                  id_cols = row_ind) %>%
      arrange(parse_number(row_ind))
    return(output)
  }) %>%
    `names<-`(na.omit(unique(msmt_inds_2$tab_ind)))
}

table_forms <- forms[forms %in% na.omit(unique(msmt_inds$form))]

msmt_tables_cs <- lapply(table_forms, function(x){msmt_tabler(x, lang = "cs")}) %>%
  `names<-`(na.omit(unique(msmt_inds$form)))

msmt_tables_en <- lapply(table_forms, function(x){msmt_tabler(x, lang = "en")}) %>%
  `names<-`(na.omit(unique(msmt_inds$form)))

# Browser -----------------------------------------------------------------

temp_labs <- msmt_inds %>%
  group_by(form, tab_ind) %>%
  arrange(parse_number(col_ind)) %>%
  arrange(row_ind) %>%
  mutate(filt = 1:n()) %>%
  ungroup()

temp_flabs_1 <- msmt_forms %>%
  mutate(lab = paste0(name, "; form ", form)) %>%
  select(lab) %>%
  unlist() %>%
  `names<-`(msmt_forms$form)

temp_flabs_2 <- msmt_forms %>%
  mutate(lab = paste0(eng, "; form ", form)) %>%
  select(lab) %>%
  unlist() %>%
  `names<-`(msmt_forms$form)

msmt_browserer <- function(x, lang = "cs"){

  temp_tib_1 <- temp_labs %>%
    filter(form == x)

  if(lang == "en"){
    temp_tib_1 <- temp_tib_1 %>%
      mutate(zkr = eng_zkr)
  }

  temp_tib_1_filt <- temp_tib_1 %>%
    filter(filt == 1)

  temp_names_1 <- paste0(temp_tib_1_filt$zkr, "; etc. ; tab. ", temp_tib_1_filt$tab_ind)

  output <- lapply(temp_tib_1_filt$tab_ind,
                   function(y){

                     temp_tib_2 <- temp_tib_1 %>%
                       filter(tab_ind == y)

                     temp_names_2 <- paste0(temp_tib_2$zkr, "; var. ", temp_tib_2$item)

                     temp_vec <- as.list(temp_tib_2$item) %>%
                       `names<-`(temp_names_2)

                     return(temp_vec)

                   }) %>%
    `names<-`(temp_names_1)

  return(output)
}

browser_forms <- forms[forms %in% na.omit(unique(temp_labs$form))]

msmt_browser_cs <- lapply(browser_forms,
                          function(x){msmt_browserer(x, lang = "cs")}) %>%
  `names<-`(temp_flabs_1[browser_forms])

msmt_browser_en <- lapply(browser_forms,
                          function(x){msmt_browserer(x, lang = "en")}) %>%
  `names<-`(temp_flabs_2[browser_forms])



# Academic titles ---------------------------------------------------------

title_level <- list(
  NonA = c("diplspec.",
           "diplum.",
           "dis.",
           "disart."),
  Bc = c("bbs.",
         "bc.",
         "bca.",
         "lic."),
  Mgr = c("akmal.",
          "ing.",
          "ingarch.",
          "ingpaed.",
          "mag.",
          "mba.",
          "mg",
          "mga.",
          "mgr.",
          "mmus.",
          "msc.",
          "sm.",
          "thlic."
  ),
  Dr = c("judr.",
         "mudr.",
         "mvdr.",
         "paeddr.",
         "rndr.",
         "phdr.",
         "thdr."),
  PhD = c("artd.",
          "csc.",
          "dr.",
          "phd.",
          "doc."
  )
)

title_area <- list(
  Academic = c("csc.",
               "doc.",
               "dr.",
               "phd."),
  Art = c("akmal.",
          "artd.",
          "arch.",
          "bca.",
          "diplum.",
          "disart.",
          "mga.",
          "mgra.",
          "mmus."
  ),
  Business = c("bbs.",
               "mba."),
  Ecclesiastical = c("s.",
                     "thdr.",
                     "thlic."),
  Humanities = c("phdr.",
                 "phil."),
  Legal = c("judr."),
  Medical = c("mag.",
              "mudr.",
              "mvdr."),
  Military = c("plk."),
  Pedagogical = c("ingpaed.",
                  "paeddr."),
  Technical_Econ = c("diplspec.",
                     "dis.",
                     "ing.",
                     "ingarch.",
                     "msc.",
                     "rndr.",
                     "sm.")
)

# Save data ---------------------------------------------------------------

usethis::use_data(msmt_tables_cs,
                  msmt_tables_en,
                  msmt_browser_cs,
                  msmt_browser_en,
                  msmt_labels_cs,
                  msmt_labels_en,
                  msmt_forms_cs,
                  msmt_forms_en,
                  msmt_forms_availability,
                  msmt_inds,
                  dsia,
                  rej,
                  ruj,
                  res,
                  name_data,
                  title_level,
                  title_area,
                  internal = TRUE,
                  overwrite = TRUE)
