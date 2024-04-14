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
#' @param complete Search for additional regional identifiers? Performed by finding intersections with municipality polygons.
#' @param append Should the output be appended to the input data? Default is TRUE.
#'
#' @return Set of variables, with names starting with the RUIAN code variable names and appended with \code{"_X", "_Y", "_source"}. Each row corresponds the the rows of the original dataset.
#' Variables ending with "_X" and "_Y" indicate coordinates (crs = 5513). Variables ending with "_source" indicate the database from which the coordinates were obtained.
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
                             append = TRUE,
                             complete = TRUE){

  if(!is.data.frame(data_addresses)){
    stop("Data addresses must be a data frame")
  }

  all_vars <- c(RUIAN_vars, unlist(address_vars))

  if(!all(all_vars %in% colnames(data_addresses))){
    stop(paste0("Variables missing in 'data_addresses: ",
                paste0("'", all_vars[!all_vars %in% colnames(data_addresses)], "'", collapse = ", "))
    )
  }

  data_addresses <- as_tibble(data_addresses)

  var_marks <- RUIAN_vars %>%
    substr(., 1, 1) %>%
    paste0(., 1:length(.), "_")

  temp_data_addresses <- lapply(seq_along(RUIAN_vars),
                                function(x){
    temp_vec <- data_addresses[,RUIAN_vars[x],drop = TRUE]
    temp_ind <- 1:length(temp_vec)
    return_vec <- ifelse(is.na(temp_vec), paste0(var_marks[x], temp_ind), temp_vec)
  })

  ruj_url <- MSMT:::ruj

  temp_dir <- tempdir()

  temp_loc <- paste0(temp_dir,"\\ruj.zip")

  options(timeout = 360)

  download.file(destfile = temp_loc,
                url = ruj_url,
                quiet = TRUE,
                mode = "wb")

  options(timeout = 60)

  temp_files <- unzip(temp_loc,
                      exdir = temp_dir)

  ruj_codes <- unlist(temp_data_addresses) %>%
    unique()

  total_1 <- length(temp_files)

  cat("\n\nReading RUIAN data [1/4]\n\n")

  data_ruian <- lapply(seq_along(temp_files),
                       function(x){

                         writeLines(iconv(readLines(temp_files[x]), from = "windows-1250", to = "UTF-8"), file(temp_files[x], encoding = "UTF-8"))

                         temp_data <- read.csv(temp_files[x], sep = ";", encoding = "UTF-8")

                         temp_data <- temp_data[temp_data[,"K\u00F3d.ADM"] %in% ruj_codes,] %>%
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

  data_ruj <- data_ruian[lghts!=0] %>%
    bind_rows()

  if(!is.null(address_vars)){
    missing_ruj <- lapply(temp_data_addresses, function(x){!x %in% unlist(data_ruj[,"K\u00F3d.ADM"])})

    temp_addresses <- lapply(seq_along(missing_ruj),
                             function(x){
                               data_addresses[missing_ruj[[x]], address_vars[[x]]] %>%
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
                                 mutate(ruj = temp_data_addresses[[x]][missing_ruj[[x]]])
                             }) %>%
      bind_rows() %>%
      distinct() %>%
      group_by(.data$value) %>%
      mutate(temp_id = .data$ruj[1]) %>%
      ungroup()

    addresses <- temp_addresses %>%
      select(-all_of("ruj")) %>%
      distinct()

    total_2 <- length(addresses$value)

    cat("\n\nFinding geocodes with RCzechia [2/4]\n\n")

    geocodes <- lapply(seq_along(addresses$value),
                       function(x){
                         suppressMessages(output <- RCzechia::geocode(addresses$value[x]))

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

    data_RCzechia_1 <- data_RCzechia_0 %>%
      as_tibble() %>%
      mutate(temp_id = addresses$temp_id) %>%
      filter(!is.na(.data$result))

    data_RCzechia <- data_RCzechia_1$geometry %>%
      unlist() %>%
      matrix(ncol = 2, byrow = TRUE) %>%
      `colnames<-`(c("X", "Y")) %>%
      as_tibble() %>%
      bind_cols(data_RCzechia_1 %>%
                  select(-geometry)) %>%
      select(all_of(c("temp_id", "X", "Y"))) %>%
      mutate(source = "2_RCZgeocode")

    addresses_2 <- addresses %>%
      filter(is.na(data_RCzechia_0$result))

    cat("\n\nFinding geocodes with tidygeocoder [3/4]\n\n")

    data_tidygeocoder <- addresses_2 %>%
      mutate(country = "Czech Republic") %>%
      tidygeocoder::geocode(address = "value") %>%
      rename(X = .data$long,
             Y = .data$lat) %>%
      select(all_of(c("temp_id", "X", "Y"))) %>%
      mutate(source = "3_TIDYgeocode")

    cat("\n\nMatching zip codes [4/4]\n\n")

    temp_zip <- RCzechia::zip_codes()

    data_zip_0 <- addresses_2 %>%
      filter(is.na(data_tidygeocoder$X)) %>%
      mutate(PSC = gsub(".*([0-9]{5}).*", "\\1", .data$value) %>%
               ifelse(grepl("^[0-9]{5}$", .), ., NA)) %>%
      na.omit() %>%
      left_join(temp_zip, "PSC") %>%
      mutate(geometry = sf::st_centroid(.data$geometry))

    data_zip <- data_zip_0$geometry %>%
      unlist() %>%
      matrix(ncol = 2, byrow = TRUE) %>%
      `colnames<-`(c("X", "Y")) %>%
      as_tibble() %>%
      mutate(temp_id = data_zip_0$temp_id,
             source = "4_RCZzipcode")

    addresses_completed <- bind_rows(data_RCzechia,
                                     data_tidygeocoder,
                                     data_zip) %>%
      na.omit() %>%
      distinct() %>%
      group_by(.data$temp_id) %>%
      mutate(mark = 1:n()) %>%
      filter(.data$mark == 1) %>%
      select(-all_of("mark")) %>%
      ungroup() %>%
      right_join(temp_addresses,
                 "temp_id") %>%
      select(all_of(c("ruj", "X", "Y", "source")))

  }

  data_crs <- sf::st_crs(data_RCzechia_0)

  data_coords_0 <- sf::st_as_sf(data_ruj %>%
                                select(all_of(c("K\u00F3d.ADM",
                                                "Sou\u0159adnice.Y",
                                                "Sou\u0159adnice.X"))) %>%
                                na.omit(),
                              coords = c("Sou\u0159adnice.X", "Sou\u0159adnice.Y"),
                              agr = "constant",
                              crs = 5513) %>%
    sf::st_transform(crs = data_crs) %>%
    as_tibble() %>%
    distinct()

  data_coords <- data_coords_0$geometry %>%
    unlist() %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    `colnames<-`(c("X", "Y")) %>%
    as_tibble() %>%
    mutate(ruj = as.character(data_coords_0[,1,drop = TRUE])) %>%
    mutate(source = "1_cuzk")

  if(!is.null(address_vars)){
    data_coords <- data_coords %>%
      bind_rows(addresses_completed) %>%
      arrange(.data$source) %>%
      group_by(.data$ruj, .data$source) %>%
      mutate(source = .data$source[1]) %>%
      ungroup()
  }

  data_coords <- distinct(data_coords)

  temp_x <- data_coords$X %>%
    `names<-`(data_coords$ruj)
  temp_y <- data_coords$Y %>%
    `names<-`(data_coords$ruj)
  temp_source <- data_coords$source %>%
    `names<-`(data_coords$ruj)

  temp_coordinates <- lapply(seq_along(temp_data_addresses),
                             FUN = function(x){
                               tibble(X = temp_x[temp_data_addresses[[x]]],
                                      Y = temp_y[temp_data_addresses[[x]]],
                                      source = temp_source[temp_data_addresses[[x]]]
                               ) %>%
                                 mutate(source = ifelse(is.na(.data$source), "5_failed", .data$source)) %>%
                                 `names<-`(paste0(RUIAN_vars[x], "_", c("X", "Y", "source")))
                             }) %>%
    bind_cols()

  if(complete){
    map_munip <- RCzechia::obce_polygony()
    map_citypart <- RCzechia::casti()

    map_all <- map_citypart %>%
      left_join(map_munip %>%
                  as_tibble() %>%
                  select(-geometry)) %>%
      bind_rows(map_munip %>%
                  filter(!KOD_OBEC %in% map_citypart$KOD_OBEC))
    map_points <- sf::st_as_sf(na.omit(data_coords),coords = c("X", "Y"))
    sf::st_crs(map_points) <- sf::st_crs(map_all)

    temp_intersects <- sf::st_intersects(map_points, map_all)

    data_complete <- map_all %>%
      as_tibble() %>%
      select(-geometry) %>%
      .[unlist(temp_intersects),] %>%
      bind_cols(na.omit(data_coords) %>% select(ruj))

    vec_ind <- c(1:nrow(data_complete)) %>%
      `names<-`(data_complete$ruj)

    temp_complete <- lapply(seq_along(temp_data_addresses),
                            FUN = function(x){
                              data_complete[vec_ind[temp_data_addresses[[x]]],] %>%
                                `names<-`(paste0(RUIAN_vars[x], "_", names(.)))
                            }) %>%
      bind_cols()

    temp_coordinates <- bind_cols(temp_coordinates,
                                  temp_complete)
  }


  if(append){
    output <- data_addresses %>%
      bind_cols(temp_coordinates)
  }else{
    output <- data_addresses[,RUIAN_vars] %>%
      bind_cols(temp_coordinates)
  }

  return(output)
}
