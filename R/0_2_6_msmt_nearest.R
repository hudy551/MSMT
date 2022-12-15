#' Select nearest locations from a distance matrix

#' This function takes as input a distance matrix and returns list of either the nearest n locations, or locations within a given distance.
#'  
#' @param dmat A matrix containing distances, with location identificators as col and row names
#' @param dist A single number, indicating maximum distance, in which locations are to be included
#' @param count A single integer, indicating the number of k nearest locations to be included

#' @return A list, containing either one of both outputs, depending on whether both dist and count parameters were entered.
#' For each type of proximity a list is returned, where each element is named with the id of target location and contains a vector with ids of selected locations, arranged from nearest yto farthest
#'  
#' @examples 
#' data_addresses <- msmt_addresses()
#' data_addresses_wcoord <- msmt_coordinates(data_addresses)
#' id <- data_addresses_wcoord$red_izo
#' X <- data_addresses_wcoord$Red_X
#' Y <- data_addresses_wcoord$Red_Y
#' dmat <- msmt_distances(id = id, X = X, Y = Y)

msmt_nearest <- function(dmat,
                         dist = NULL,
                         count = NULL){
  
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
  }

  return(outputs)
}
aa <- msmt_nearest(dmat, dist = 1000, count = 3)

aa$dist[1:3]
