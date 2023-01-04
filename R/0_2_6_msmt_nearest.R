#' Select nearest locations from a distance matrix

#' This function takes as input a distance matrix and returns list of either the nearest n locations, or locations within a given distance.
#'
#' @param dmat A matrix containing distances, with location identifiers included as column and and row names
#' @param dist A single number, indicating maximum distance, in which locations are to be included
#' @param count A single integer, indicating the number of k nearest locations to be included
#' @param format A single integer, indicating format of the output. Can be either a "list" or a "tibble".

#' @return A list, containing either one of both outputs, depending on whether both dist and count parameters were entered.
#' For each type of proximity a list is returned, where each element is named with the id of target location and contains a vector with ids of selected locations, arranged from nearest yto farthest
#'
#' @examples
#' data_addresses <- msmt_addresses(NUTS3 = "CZ010")
#' data_addresses_wcoord <- msmt_coordinates(data_addresses)
#' id <- data_addresses_wcoord$red_izo
#' X <- data_addresses_wcoord$RedRUAINKod_X
#' Y <- data_addresses_wcoord$RedRUAINKod_Y
#' dmat <- msmt_distances(id = id, X = X, Y = Y)
#' nearest_5 <- msmt_nearest(dmat, count = 5)
#'
#' @importFrom units drop_units
#' @importFrom dplyr bind_rows mutate select
#'
#' @export

msmt_nearest <- function(dmat,
                         dist = NULL,
                         count = NULL,
                         format = "list"){

  if(!is.matrix(dmat)){
    stop("'dmat' must be a matrix")
  }

  if(is.null(colnames(dmat))|is.null(rownames(dmat))){
    stop("Rows and columns of 'dmat' must be named")
  }

  outputs <- list()

  dmat <- units::drop_units(units::as_units(dmat))

  if(!is.null(dist)){
    outputs$dist <- lapply(1:nrow(dmat),
                           function(x){
                             temp_name <- rownames(dmat)[x]
                             temp_names_0 <- colnames(dmat)
                             temp_names <- temp_names_0[temp_names_0 != temp_name]
                             temp_vals <- dmat[x, temp_names_0 != temp_name]
                             temp_names <- temp_names[order(temp_vals)]
                             temp_vals <- temp_vals[order(temp_vals)]

                             temp_names[temp_vals <= dist]
                           }) %>%
      `names<-`(rownames(dmat))

    if(format == "tibble"){
      outputs$dist <- outputs$dist %>%
        lapply(function(x){
          if(length(x)>0){
            output <- `names<-`(x, paste0("Schl_", 1:length(x)))
          }else{
            output <- c(Schl_1 = NA)
          }
        }) %>%
        bind_rows() %>%
        mutate(ids = names(outputs$dist)) %>%
        select(all_of("ids"), everything())
    }

  }

  if(!is.null(count)){
    outputs$count <- lapply(1:nrow(dmat),
                           function(x){
                             temp_name <- rownames(dmat)[x]
                             temp_names_0 <- colnames(dmat)
                             temp_names <- temp_names_0[temp_names_0 != temp_name]
                             temp_vals <- dmat[x, temp_names_0 != temp_name]
                             temp_names[order(temp_vals)][1:count]
                           }) %>%
      `names<-`(rownames(dmat))

    if(format == "tibble"){
      outputs$count <- outputs$count %>%
        lapply(function(x){
          if(length(x)>0){
            output <- `names<-`(x, paste0("object_", 1:length(x)))
          }else{
            output <- c(object_1 = NA)
          }
        }) %>%
        bind_rows() %>%
        mutate(ids = names(outputs$count)) %>%
        select(all_of("ids"), everything())
    }
  }
  return(outputs)
}
