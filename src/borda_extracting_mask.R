#' borda_extracting_mask
#' 
#' Generate mask to apply to graphs.
#' After we have taken the result of borda voting, and after we have done the
#' cutting with S/W ties, we have to aggregate the result, doing the union the 
#' masks.
#' 
#' Author: Emanuele Pesce
library(igraph)
# source("./../strong_weak_ties_cutting/sw_cutting.R", chdir = T)

#' Generate the mask of the graph doing the union of two graph masks
#' Take in input two mask in order do create a new mask which composed by the 
#' edges of both input masks.
#' @param gC graph of controls
#' @param gP graph of Patients
#' @return toReturn a matrix[m, 2], where m is the number of the edges after the
#'         union. So each row is an edge and columns stands for vertices of the 
#'         edges     
#' @examples
#' r = unionMask(residual_C, residual_P)
unionMask <- function(gC, gP){
  # get list of edges of controls and patients
  edgesC <- get.edgelist(gC)
  edgesP <- get.edgelist(gP)
  
  # cleaning: remove from each list edges with same nodes (i.e.: [V1, V1])
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



# pathBordaControls <- "./../../data/other/borda/borda_matrix_controls.txt"
# pathBordaSLA2 <- "./../../data/other/borda/borda_matrix_SLA2.txt"
# pathBordaSLA3 <- "./../../data/other/borda/borda_matrix_SLA3.txt"
# 
# pathMask_borda_ws_cutting <- "./../../data/other/borda/borda_mask_ws_cutting.csv"
# pathMask_borda_ws_cutting_num <- "./../../data/other/borda/borda_mask_ws_cutting_num.csv"
# pathMask_borda_ws_cutting_info <- "./../../data/other/borda/borda_mask_ws_cutting_info.csv"
# 
# path_results_cutting <- "./../../data/other/borda/borda_sw_cut_objects.RData"
# load(path_results_cutting)
# residual_Controls <- r_Controls$residualGraph
# mask1 <- unionMask(residual_Controls, residual_Controls)

#example
# if(interactive()){
#   # import graphs
#   g_C <- i_adjacencyFromFile("./../../data/toyData/extract/bordaMatrixControls.txt")
#   g_P <- i_adjacencyFromFile("./../../data/toyData/extract/bordaMatrixPatients.txt")
#   
#   # compute residuals
#   RC <- minFlowPruning(g_C, threshold = 0.05, flow = 0)
#   RP <- minFlowPruning(g_P, threshold = 0.05, flow = 0)
#   residual_C <- RC$residualGraph
#   residual_P <- RP$residualGraph
#   
#   r = unionMask(residual_C, residual_P)
#   
#   print("Edges in Controls")
#   print(RC$n_residualEdges)
#   print("Edges in Patients")
#   print(RP$n_residualEdges)
#   print("Edges in Union")
#   print(dim(r)[1])
#   print("Union - Controls")
#   print(dim(r)[1] - RC$n_residualEdges)
#   print("Union - Patients")
#   print(dim(r)[1] - RP$n_residualEdges)
#   
#   out <- list("e_controls" = RC$n_residualEdges, "e_patients" = RP$n_residualEdges, 
#               "e_union" =dim(r)[1], "e_union_m_controls" =  dim(r)[1] - RC$n_residualEdges,
#               "e_union_m_patients" = dim(r)[1] - RP$n_residualEdges)
#   write.csv(out, file = "info_maskUnion.csv")
#   write.table(r,file="edgesMask.csv",sep="\t", col.names = F, row.names = F)
# }