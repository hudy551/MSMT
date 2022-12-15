#' Append coordinates to school addresses

#' This function adds geo coordinates to data from the address book of all school and educational facilities in Czech Republic (Rejstříku škol a školských zařízení).
#' 
#' As no single method for finding coordinates is perfect, the function tries multiple sources in the following sequence:
#' 
#' 1. A database of State Administration of Land Surveying and Cadastre https://vdp.cuzk.cz/.  
#' 2. R package RCzechia, finding geocodes from addresses.
#' 3. R package tidygeocoder, finding geocodes from addresses.
#' 4. R package RCzechia, finding geocodes from centroids of polygons defined by a zip code region.
#' 
#' @param data_addresses Output of msmt_addresses
#' 
#' @return The original tibble, appended with variables Red_X, Red_Y, Misto_X, Misto_Y coordinates ('Red_' for red_izo, 'Misto' for p_izo).
#' The tibble also contains variables Red_source and Misto_source. These indicate the information source used to get the coordinates. 
#' 
#' @examples 
#' data_addresses <- msmt_addresses()
#' data_addresses_wcoord <- msmt_coordinates(data_addresses)

msmt_coordinates <- function(data_addresses){
  
  require(readr)
  require(tibble)
  require(tidyr)
  require(dplyr)
  require(RCzechia)
  require(sf)
  require(tidygeocoder)
  
  data_addresses_0 <- data_addresses
  data_addresses <- data_addresses %>%
    mutate(mark = 1:n()) %>%
    mutate(RedRUAINKod = ifelse(is.na(RedRUAINKod), paste0("MISSR", mark), RedRUAINKod),
           MistoRUAINKod = ifelse(is.na(MistoRUAINKod), paste0("MISSM", mark), MistoRUAINKod)) %>%
    select(-mark)
  
  ruj_url <- "https://vdp.cuzk.cz/vymenny_format/csv/20220131_OB_ADR_csv.zip"
  
  temp_dir <- tempdir()
  
  temp_loc <- paste0(temp_dir,"\\ruj.zip")
  
  download.file(destfile = temp_loc, 
                url = ruj_url,
                mode = "wb")
  
  temp_files <- unzip(temp_loc, 
                      exdir = temp_dir)
  
  ruj_codes <- c(data_addresses$RedRUAINKod,
                 data_addresses$MistoRUAINKod) %>%
    unique()
  
  total_1 <- length(temp_files)
  
  cat("\n\nReading RUIAN data [1\\4]\n\n")
  
  data_ruian <- lapply(seq_along(temp_files),
                       function(x){
                         temp_data <- read.csv(temp_files[x], sep = ";") %>%
                           as_tibble() %>%
                           filter(Kód.ADM %in% ruj_codes)
                         
                         progress_bar_1 <- txtProgressBar(min = 0, 
                                                          max = total_1, 
                                                          style = 3)
                         progress <- x
                         setTxtProgressBar(progress_bar_1, progress)
                         
                         return(temp_data)
                       }
  )
  
  lghts <- lapply(data_ruian, nrow) %>%
    unlist()
  
  data_ruj <- data_ruian[lghts!=0] %>%
    bind_rows()
  
  rcs <- ruj_codes[!ruj_codes%in%data_ruj$Kód.ADM]
  
  data_rcs_0_1 <- data_addresses %>%
    filter(RedRUAINKod %in% rcs) %>%
    select(RedRUAINKod, RedAdresa1, RedAdresa2, RedAdresa3)
  
  data_rcs_0_2 <- data_addresses %>%
    filter(MistoRUAINKod %in% rcs) %>%
    select(MistoRUAINKod, MistoAdresa1, MistoAdresa2, MistoAdresa3) %>%
    `colnames<-`(colnames(data_rcs_0_1))
  
  data_rcs <- bind_rows(data_rcs_0_1,
                        data_rcs_0_2) %>%
    distinct() %>%
    group_by(RedAdresa1, 
             RedAdresa2, 
             RedAdresa3) %>%
    mutate(temp_id = RedRUAINKod[1]) %>%
    ungroup()
  
  addresses <- data_rcs %>%
    select(matches("RedAdresa")) %>%
    apply(MARGIN = 1, function(x){paste0(na.omit(x), collapse = " ") %>%
        gsub(" ([0-9]{3}) ([0-9]{2}) ", " \\1\\2 ", .) %>%
        gsub("  ", " ", .)}) %>%
    tibble(address = ., temp_id = data_rcs$temp_id) %>%
    distinct()
  
  total_2 <- length(addresses$address)
  
  cat("\n\nFinding geocodes with RCzechia [2\\4]\n\n")
  
  geocodes <- lapply(seq_along(addresses$address),
                     function(x){
                       suppressMessages(output <- RCzechia::geocode(addresses$address[x]))
                       
                       progress_bar_2 <- txtProgressBar(min = 0, 
                                                        max = total_2, 
                                                        style = 3)
                       progress <- x
                       setTxtProgressBar(progress_bar_2, progress)
                       return(output)
                     })
  
  data_RCzechia_0 <- geocodes %>% 
    lapply(function(x){x[1,]}) %>% 
    bind_rows()
  
  data_RCzechia <- data_RCzechia_0 %>%
    as_tibble() %>%
    mutate(temp_id = addresses$temp_id) %>%
    filter(!is.na(result)) %>%
    unnest_wider(geometry, names_sep = c("long", "lat")) %>%
    rename(X = geometrylong1, 
           Y = geometrylat2) %>%
    select(temp_id, X, Y) %>%
    mutate(source = "2_RCZgeocode")
  
  addresses_2 <- addresses %>%
    filter(is.na(data_RCzechia_0$result))

  cat("\n\nFinding geocodes with tidygeocoder [3\\4]\n\n")

  data_tidygeocoder <- addresses_2 %>%
    mutate(country = "Czech Republic") %>%
    tidygeocoder::geocode(address = address) %>%
    rename(X = long, 
           Y = lat) %>%
    select(temp_id, X, Y) %>%
    mutate(source = "3_TIDYgeocode")
  
  cat("\n\nMatching zip codes [4\\4]\n\n")

  temp_zip <- RCzechia::zip_codes()
  
  data_zip <- addresses_2 %>%
    filter(is.na(data_tidygeocoder$X)) %>%
    mutate(PSC = gsub(".*([0-9]{5}).*", "\\1", address) %>%
             ifelse(grepl("^[0-9]{5}$", .), ., NA)) %>%
    na.omit() %>%
    left_join(temp_zip, "PSC") %>%
    mutate(geometry = sf::st_centroid(geometry)) %>%
    unnest_wider(geometry, names_sep = c("long", "lat")) %>%
    rename(X = geometrylong1, 
           Y = geometrylat2) %>%
    select(temp_id, X, Y) %>%
    mutate(source = "4_RCZzipcode")
  
  addresses_completed <- bind_rows(data_RCzechia, 
                                   data_tidygeocoder,
                                   data_zip) %>%
    na.omit() %>%
    distinct() %>%
    group_by(temp_id) %>%
    mutate(mark = 1:n()) %>%
    filter(mark == 1) %>%
    select(-mark) %>%
    ungroup() %>%
    right_join(data_rcs,
               "temp_id") %>%
    select(RedRUAINKod, X, Y, source)
  
  data_crs <- sf::st_crs(data_RCzechia_0)
  
  data_coords <- sf::st_as_sf(data_ruj %>%
                                select(Kód.ADM, 
                                       Souřadnice.Y, 
                                       Souřadnice.X) %>%
                                na.omit(), 
                              coords = c("Souřadnice.X", "Souřadnice.Y"), 
                              agr = "constant", 
                              crs = 5513) %>%
    st_transform(crs = data_crs) %>%
    as_tibble() %>%
    distinct() %>%
    unnest_wider(geometry, 
                 names_sep = c("long", "lat")) %>%
    `names<-`(c("RedRUAINKod", "X", "Y")) %>%
    mutate(RedRUAINKod = as.character(RedRUAINKod)) %>%
    mutate(source = "1_cuzk") %>%
    bind_rows(addresses_completed) %>%
    distinct()
  
  output <- data_addresses %>%
    left_join(data_coords %>%
                rename(Red_X = X,
                       Red_Y = Y,
                       Red_source = source),
              by = c("RedRUAINKod" = "RedRUAINKod")) %>%
    left_join(data_coords %>%
                rename(Misto_X = X,
                       Misto_Y = Y,
                       Misto_source = source),
              by = c("MistoRUAINKod" = "RedRUAINKod")) %>%
    mutate(RedRUAINKod = data_addresses_0$RedRUAINKod,
           MistoRUAINKod = data_addresses_0$MistoRUAINKod,
           Red_source = ifelse(is.na(Red_source), "5_failed", Red_source),
           Misto_source = ifelse(is.na(Red_source), "5_failed", Red_source))
  
  return(output)
}
