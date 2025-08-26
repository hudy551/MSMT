#' Append coordinates to school addresses

#' This function adds geo coordinates to data from the address book of all school and educational facilities in Czech Republic (Rejstříku škol a školských zařízení).
#'
#' As no single method for finding coordinates is perfect, the function tries multiple sources in the following sequence:
#' \enumerate{
#' \item A database of Czech State Administration of Land Surveying and Cadastre https://vdp.cuzk.cz/.
#' \item R package RCzechia, finding geocodes from addresses.
#' \item R package tidygeocoder, finding geocodes from addresses.
#' \item R package RCzechia, finding geocodes from centroids of polygons defined by a zip code area.
#' }
#'
#' @param data_addresses Output of \code{msmt_addresses}, or a data frame, containing variables with RUIAN codes and/or addresses
#' @param RUIAN_vars A character vector, containing the names of one or more variables with RUIAN codes of locations. Defaults to the names in \code{msmt_addresses} output
#' @param address_vars A named list consisting of character vector, each named as one of the RUIAN code variable. The character vectors consist of names of variable(s) containing addresses or their parts
#' @param id_var A string indicating the name of the variable containing the identifier of school, defaults to "red_izo"
#' @param complete Search for additional regional identifiers? Performed by finding intersections with municipality polygons.
#' @param append Should the output be appended to the input data? Default is TRUE.
#' @param scrape Should scraping the web of CUZK be included in the search for missing coordinates?
#' @param res Add locations of providers from the Business register (RES)?
#' @param timeout Increases timeout for dowloads with options(timeout = timeout) in order to allow the download of the RUIAN database. Defaults to 360, to skip, set value to NULL.
#'
#' @return Set of variables, with names starting with the RUIAN code variable names and appended with \code{"_geometry", "_source"}. Each row corresponds the the rows of the original dataset.
#' Variables ending with "_geometry" indicate coordinates (crs = 5513). Variables ending with "_source" indicate the database from which the coordinates were obtained. If \code{complete = TRUE},
#' more vatiables with regional identifiers are also included, akin to the \code{RCzechia} package objects.
#'
#' @examples
#' data_addresses <- msmt_addresses(NUTS3 = "CZ010")
#' data_addresses_wcoord <- msmt_coordinates(data_addresses)
#'
#' @importFrom dplyr as_tibble select all_of bind_rows mutate mutate_all rename everything filter distinct group_by ungroup left_join n right_join arrange
#' @importFrom utils txtProgressBar setTxtProgressBar unzip read.csv
#' @importFrom RCzechia geocode zip_codes obce_polygony casti
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#' @importFrom sf st_centroid st_crs st_crs<- st_as_sf st_transform st_as_sf st_intersects
#' @importFrom stats na.omit
#'
#' @export

msmt_coordinates <- function(data_addresses,
                             RUIAN_vars = c("RedRUAINKod", "MistoRUAINKod"),
                             address_vars = list(RedRUAINKod = c("RedAdresa1",
                                                                   "RedAdresa2",
                                                                   "RedAdresa3"),
                                                MistoRUAINKod = c("MistoAdresa1",
                                                                  "MistoAdresa2",
                                                                  "MistoAdresa3")),
                             id_var = "red_izo",
                             scrape = FALSE,
                             append = TRUE,
                             complete = TRUE,
                             res = FALSE,
                             timeout = 360){

  if(!is.data.frame(data_addresses)){
    stop("Data addresses must be a data frame")
  }

  all_vars <- c(RUIAN_vars, unlist(address_vars))

  if(!all(all_vars %in% colnames(data_addresses))){
    stop(paste0("Variables missing in 'data_addresses: ",
                paste0("'", all_vars[!all_vars %in% colnames(data_addresses)], "'", collapse = ", "))
    )
  }

# Prepare addresses -------------------------------------------------

  data_addresses <- as_tibble(data_addresses) %>%
    mutate(ZrizICO = ifelse(nchar(ZrizICO) == 10, substr(ZrizICO,1,8), ZrizICO))

# Find variables with RUIAN codes and formulate unique substitutes for NA values

  var_marks <- RUIAN_vars %>%
    substr(., 1, 1) %>%
    paste0(., 1:length(.), "_")

  temp_data_addresses <- lapply(seq_along(RUIAN_vars),
                                function(x){
    temp_vec <- data_addresses[,RUIAN_vars[x],drop = TRUE]
    temp_ind <- 1:length(temp_vec)
    return_vec <- ifelse(is.na(temp_vec), paste0(var_marks[x], temp_ind), temp_vec)
  }) %>%
    `names<-`(var_marks)

# Download RUIAN database -------------------------------------------------

  ruj_url <- MSMT:::ruj

  temp_dir <- tempdir()

  temp_loc <- paste0(temp_dir,"\\ruj.zip")

  old_timeout <- getOption("timeout")
  options(timeout = timeout)

  download.file(destfile = temp_loc,
                url = ruj_url,
                quiet = TRUE,
                mode = "wb")

  options(timeout = old_timeout)

  temp_files <- unzip(temp_loc,
                      exdir = temp_dir)

  # All RUIAN codes

  ruj_codes <- unlist(temp_data_addresses) %>%
    unique()

  total_1 <- length(temp_files)

  cat("\n\nReading RUIAN data [1/5]\n\n")

  data_ruian <- lapply(seq_along(temp_files),
                       function(x){

                         writeLines(iconv(readLines(temp_files[x]), from = "windows-1250", to = "UTF-8"), file(temp_files[x], encoding = "UTF-8"))

                         temp_data <- read.csv(temp_files[x], sep = ";", encoding = "UTF-8")

#                         temp_data <- temp_data[temp_data[,"K\u00F3d.ADM"] %in% ruj_codes,] %>%
                         temp_data <- temp_data %>%
                           as_tibble()

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

# Data with coordinates for codes contained in the original data

  data_ruj <- data_ruian[lghts!=0] %>%
    bind_rows()

# Find addresses missing in the database ----------------------------------

  found_codes <- data_ruj[,c("K\u00F3d.ADM", "Sou\u0159adnice.X", "Sou\u0159adnice.Y")] %>%
    `colnames<-`(c("RUIAN", "X", "Y")) %>%
    filter(RUIAN %in% ruj_codes) %>%
    mutate_all(as.character) %>%
    mutate(source = "1_cuzk") %>%
    na.omit()

  missing_codes <- ruj_codes[!ruj_codes %in% found_codes$RUIAN]

# Find addresses with CUZK scraper ----------------------------------------

  missing_codes_scrape <- missing_codes[grepl("^[0-9]+", missing_codes)]

  cat("\n\nFinding coordinates by scraping https://vdp.cuzk.cz/vdp/ruian [2/5]\n\n")
  if(scrape){
    total_2 <- length(missing_codes_scrape)

    progress_bar_2 <- txtProgressBar(min = 0,
                                     max = total_2,
                                     style = 3)

    found_codes_scrape <- seq_along(missing_codes_scrape) %>%
      lapply(function(x){

        setTxtProgressBar(progress_bar_2, x)

        temp_RUIAN <- missing_codes_scrape[x]

        url_lines <- paste0(c("https://vdp.cuzk.cz/vdp/ruian/adresnimista/",
                              "https://vdp.cuzk.cz/vdp/ruian/stavebniobjekty/"),
                            temp_RUIAN)

        mark <- TRUE
        y <- 1

        while (mark) {
          get_lines <- tryCatch({readLines(url_lines[y],warn = FALSE)},
                                error = function(e){return("MISS")},
                                warning = function(e){return("MISS")})

          Sys.sleep(.5)

          len_mark <- length(get_lines)
          mark <- (len_mark == 1) & y < 2
          y <- y + 1
        }

        if(len_mark > 1){
          if(sum(grepl("Y:.+X:", get_lines))>0){
            temp_line <- get_lines[grepl("Y:.+X:", get_lines)]

            temp_line_x <- gsub(".+X:([ 0-9,]+).+", "\\1", temp_line) %>%
              gsub(",", ".", .) %>%
              gsub(" ", "", .)

            temp_line_y <- gsub(".+Y:([ 0-9,]+).+", "\\1", temp_line) %>%
              gsub(",", ".", .) %>%
              gsub(" ", "", .)

            result <- tibble(RUIAN = temp_RUIAN,
                             X = temp_line_x,
                             Y = temp_line_y)
          }else{
            result <- tibble(RUIAN = temp_RUIAN,
                             X = NA,
                             Y = NA)
          }
        }else{

          result <- tibble(RUIAN = temp_RUIAN,
                           X = NA,
                           Y = NA)
        }
        return(result)
      }) %>%
      bind_rows() %>%
      na.omit()

    found_codes <- found_codes %>%
      bind_rows(found_codes_scrape %>%
                  mutate(source = "2_cuzk_scrape")) %>%
      distinct()
  }else{
    cat("\n\nSkipping\n\n")
  }

  found_codes <- found_codes %>%
    sf::st_as_sf(coords = c("X", "Y"),
                 agr = "constant",
                 crs = 5513) %>%
    sf::st_transform(crs = "EPSG:4326") %>%
    as_tibble() %>%
    distinct()

  missing_codes <- ruj_codes[!ruj_codes %in% found_codes$RUIAN]

# Geocode with RCZechia ---------------------------------------------------


  if(!is.null(address_vars)){

  #Prepare address strings

    temp_addresses <- lapply(seq_along(address_vars),
                             function(x){
                               temp_vars <- c(RUIAN_vars[[x]], address_vars[[x]])

                               temp_data <- data_addresses %>%
                                 select(all_of(temp_vars)) %>%
                                 `colnames<-`(c("RUIAN", paste0("a", seq_along(address_vars[[x]]))))

                               output_data <- temp_data %>%
                                 select(-RUIAN) %>%
                                 mutate_all(function(z){
                                   gsub("( |^)([0-9]{3}) ([0-9]{2})( |$)", " \\2\\3 ", z) %>%
                                     gsub(" $|^ ", "", .)
                                 }) %>%
                                 apply(MARGIN = 1,
                                       function(y){paste0(y, collapse = " ")}) %>%
                                 gsub(" NA$| NA |^NA ", "", .) %>%
                                 gsub(" $|^ ", "", .) %>%
                                 gsub("  ", " ", .) %>%
                                 as_tibble_col() %>%
                                 mutate(RUIAN = temp_data_addresses[[x]])
                             }) %>%
      bind_rows() %>%
      distinct() %>%
      filter(RUIAN %in% missing_codes)

    cat("\n\nFinding geocodes with RCzechia [3/5]\n\n")

    total_3 <- length(temp_addresses$value)

    progress_bar_3 <- txtProgressBar(min = 0,
                                     max = total_3,
                                     style = 3)

    found_RCzechia <- lapply(seq_along(temp_addresses$value),
                       function(x){
                         suppressMessages(output <- RCzechia::geocode(temp_addresses$value[x]))

                         setTxtProgressBar(progress_bar_3, x)
                         return(output[1,])
                       }) %>%
      bind_rows() %>%
      mutate(RUIAN = temp_addresses$RUIAN) %>%
      na.omit() %>%
      as_tibble() %>%
      select(RUIAN, geometry) %>%
      mutate(source = "3_RCzechia")

    found_codes <- found_codes %>%
      bind_rows(found_RCzechia) %>%
      distinct()

    cat("\n\nFinding geocodes with tidygeocoder [4/5]\n\n")

    found_tidygeocoder <- temp_addresses %>%
      filter(!RUIAN %in% found_codes$RUIAN) %>%
      mutate(value = paste0(value, " Czech Republic")) %>%
      tidygeocoder::geocode(address = "value") %>%
      select(RUIAN, long, lat) %>%
      `names<-`(c("RUIAN", "X", "Y")) %>%
      mutate(source = "4_tidygeocoder") %>%
      na.omit() %>%
      sf::st_as_sf(coords = c("X", "Y"),
                   crs = "EPSG:4326")

    found_codes <- found_codes %>%
      bind_rows(found_tidygeocoder) %>%
      distinct()

    cat("\n\nMatching zip codes [5/5]\n\n")

    found_zip <- temp_addresses %>%
      filter(!RUIAN %in% found_codes$RUIAN) %>%
      filter(grepl("[0-9]{5}", value)) %>%
      mutate(value = gsub(".*([0-9]{5}.*)", "\\1", value),
             value = gsub("[^ ]+\\.|[^ ]+,", "", value),
             value = gsub("[ ]+", " ", value),
             value = paste0(value, ", Czech Republic")) %>%
      tidygeocoder::geocode(address = "value") %>%
      select(RUIAN, long, lat) %>%
      `names<-`(c("RUIAN", "X", "Y")) %>%
      mutate(source = "5_tidygeocoderzip") %>%
      na.omit() %>%
      sf::st_as_sf(coords = c("X", "Y"),
                   crs = "EPSG:4326")

    found_codes <- found_codes %>%
      bind_rows(found_zip) %>%
      distinct()

  }else{
    }
    missing_codes <- temp_addresses %>%
      filter(!RUIAN %in% found_codes$RUIAN)
    if (nrow(missing_codes) > 0) {
    }
    missing_codes <- temp_addresses %>%
      filter(!RUIAN %in% found_codes$RUIAN)
    if (nrow(missing_codes) > 0) {
    }
    cat("\n\nNo address variables specified\n\n")
  }

  found_codes <- found_codes %>%
    group_by(RUIAN) %>%
    filter(1:n() == 1) %>%
    ungroup()

  temp_coordinates <- lapply(seq_along(RUIAN_vars),
                             FUN = function(x){
                               data_addresses %>%
                                 left_join(found_codes %>%
                                             `names<-`(c(RUIAN_vars[x],
                                                         paste0(RUIAN_vars[x], "_source"),
                                                         paste0(RUIAN_vars[x], "_geometry"))
                                                       ), by = RUIAN_vars[x]) %>%
                                 select(starts_with(RUIAN_vars[x]))}) %>%
    bind_cols()

  temp_coordinates[,grepl("source", names(temp_coordinates))] <- temp_coordinates[,grepl("source", names(temp_coordinates))] %>%
    mutate_all(function(x){ifelse(is.na(x), "99_failed", x)})

  if(complete){
    map_munip <- RCzechia::obce_polygony()
    map_citypart <- RCzechia::casti()

    map_all <- map_citypart %>%
      left_join(map_munip %>%
                  as_tibble() %>%
                  select(-geometry)) %>%
      bind_rows(map_munip %>%
                  filter(!KOD_OBEC %in% map_citypart$KOD_OBEC)) %>%
      mutate(KOD_ZUJ = ifelse(is.na(KOD), KOD_OBEC, KOD))

    map_points <- sf::st_as_sf(found_codes)
    sf::st_crs(map_points) <- sf::st_crs(map_all)

    temp_intersects <- sf::st_intersects(map_points, map_all)

    data_complete <- map_all %>%
      as_tibble() %>%
      select(-geometry) %>%
      .[unlist(temp_intersects),] %>%
      bind_cols(na.omit(found_codes) %>% select(RUIAN))

    temp_locations <- lapply(seq_along(RUIAN_vars),
                               FUN = function(x){
                                 data_addresses %>%
                                   left_join(data_complete %>%
                                               `names<-`(ifelse(names(.) == "RUIAN", RUIAN_vars[x], paste0(RUIAN_vars[x], "_", names(.)))
                                               ), by = RUIAN_vars[x]) %>%
                                   select(starts_with(RUIAN_vars[x]))}) %>%
      bind_cols()

    temp_coordinates <- temp_coordinates %>%
      bind_cols(temp_locations %>%
                  select(-matches(paste0("^", RUIAN_vars, "$", collapse = "|"))))

  }

  if(res){

    cat("\n\nDownloading the Business register (RES)\n\n")

    old_timeout <- getOption("timeout")
    options(timeout = timeout)

    data_RES <- read.csv(MSMT:::res)

    options(timeout = old_timeout)

    data_RES_zriz <- data_RES %>%
      mutate(ICO = substr(1e8+ICO, 2,9)) %>%
      select(ZrizICO = ICO, ZrizKOD_ZUJ = ICZUJ, ZrizKOD_RUIAN = KODADM) %>%
      filter(ZrizICO %in% data_addresses$ZrizICO) %>%
      na.omit() %>%
      mutate_all(as.character) %>%
      left_join(data_ruj %>%
                  select(all_of(c("K\u00F3d.ADM",
                                  "K\u00F3d.obce",
                                  "Sou\u0159adnice.X",
                                  "Sou\u0159adnice.Y"))) %>%
                  `names<-`(c("ZrizKOD_RUIAN", "ZrizKOD_OBEC","Zriz_X","Zriz_Y")) %>%
                  mutate(ZrizKOD_RUIAN = as.character(ZrizKOD_RUIAN)),
                "ZrizKOD_RUIAN")

    data_RES_schools <- data_RES %>%
      mutate(ICO = substr(1e8+ICO, 2,9)) %>%
      select(ico = ICO, RES_KOD_ZUJ = ICZUJ, RES_KOD_RUIAN = KODADM) %>%
      filter(ico %in% data_addresses$ico) %>%
      na.omit() %>%
      mutate_all(as.character) %>%
      left_join(data_ruj %>%
                  select(all_of(c("K\u00F3d.ADM",
                                  "K\u00F3d.obce",
                                  "Sou\u0159adnice.X",
                                  "Sou\u0159adnice.Y"))) %>%
                  `names<-`(c("RES_KOD_RUIAN", "RES_KOD_OBEC","RES_X","RES_Y")) %>%
                  mutate(RES_KOD_RUIAN = as.character(RES_KOD_RUIAN)),
                "RES_KOD_RUIAN")

    data_addresses <- data_addresses %>%
      left_join(data_RES_zriz) %>%
      left_join(data_RES_schools)
  }

  if(append){
    output <- data_addresses %>%
      select(-all_of(RUIAN_vars)) %>%
      bind_cols(temp_coordinates)
  }else{
    output <- data_addresses[,id_var] %>%
      bind_cols(temp_coordinates)
  }

  return(output)
}
