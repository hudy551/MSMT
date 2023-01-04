#' Compute distances between locations

#' This function takes as input coordinates and identificators and returns a distance matrix that can be used in further functions.
#'
#' @param id A character vector containing ID values of all used locations
#' @param X A numeric vector of the same length, indicating X coordinates
#' @param Y A numeric vector of the same length, indicating Y coordinates
#' @param id_row Optional - a subset of id to constitute the rows of distance matrix
#' @param id_col Optional - a subset of id to constitute the columns of distance matrix

#' @return A unit matrix containing pairwise distances between locations in meters, with id as row and column names.
#' The units can be removed by using units::drop_units()
#'
#' @examples
#' data_addresses <- msmt_addresses(NUTS3 = "CZ010")
#' data_addresses_wcoord <- msmt_coordinates(data_addresses)
#' id <- data_addresses_wcoord$red_izo
#' X <- data_addresses_wcoord$RedRUAINKod_X
#' Y <- data_addresses_wcoord$RedRUAINKod_Y
#' dmat <- msmt_distances(id = id, X = X, Y = Y)
#'
#' @importFrom tibble tibble
#' @importFrom dplyr distinct filter
#' @importFrom sf st_as_sf
#' @importFrom stats na.omit
#'
#' @export

msmt_distances <- function(id,
                           X,
                           Y,
                           id_row = NULL,
                           id_col = NULL){

  if(is.null(id_row)){
    id_row <- id
  }
  if(is.null(id_col)){
    id_col <- id
  }

  temp_data <- tibble(id = id,
                      X = X,
                      Y = Y) %>%
    na.omit() %>%
    distinct()

  coords <- temp_data %>%
    sf::st_as_sf(crs = "EPSG:4326", coords = c("X", "Y"))

  cat("\n\nComputing distance matrix (this might take a while)\n\n")

  dmat <- sf::st_distance(x = coords %>%
                            filter(.data$id %in% id_row),
                          y = coords %>%
                            filter(.data$id %in% id_col)) %>%
    `rownames<-`(coords$id) %>%
    `colnames<-`(coords$id)

  return(dmat)
}
