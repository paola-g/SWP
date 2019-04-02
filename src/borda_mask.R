#' borda_extracting_mask
#' 
#' Generate masks to apply to graphs.
#' After borda voting and SWP have been applied, the results are aggregated by
#' computing the union of all the masks.
#' 
#' Author: Emanuele Pesce
library(igraph)

#' Generates a mask by computing the union of two graph masks
#' Takes in input two masks in order to create a new one which composed by the 
#' edges of both input masks.
#' @param gC graph of controls
#' @param gP graph of Patients
#' @return toReturn a matrix[m, 2], where m is the number of the edges after the
#'         union. So each row is an edge and columns represent the edges' vertices     
#' @examples
#' r = unionMask(residual_C, residual_P)
unionMask <- function(gC, gP){
  # get list of edges of controls and patients
  edgesC <- get.edgelist(gC)
  edgesP <- get.edgelist(gP)
  
  indexToRmv <- c()
  for (i in 1:dim(edgesC)[1]){
    v1 = edgesC[i,1]
    v2 = edgesC[i,2]
    if (v1 == v2){
      indexToRmv <- c(indexToRmv, i)
    }
  }
  if(!is.null(indexToRmv)){
    edgesC <- edgesC[-indexToRmv,]
  }
  
  indexToRmv <- c()
  for (i in 1:dim(edgesP)[1]){
    v1 = edgesP[i,1]
    v2 = edgesP[i,2]
    if (v1 == v2){
      indexToRmv <- c(indexToRmv, i)
    }
  }
  if(!is.null(indexToRmv)){
    edgesP <- edgesP[-indexToRmv,]
  }
  
  u_edges <- rbind(edgesC, edgesP)
  u_edges <- unique(u_edges)
  
  return(u_edges)
}