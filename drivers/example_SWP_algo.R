#' Example script to show how to run SWP
#' 
#' Author: Emanuele Pesce
source("../src/SWP.R", chdir = T)

if(interactive()){
  ptm <- proc.time()
  
  # input here: acquiring a graph 
  # g must be in igraph format 
  # parse txt matrix and import through in igraph format
  g <- i_adjacencyFromFile("../data/toy/example_data_borda_aggregation_matrix.txt") 
  
  R <- sw_cutting(g, threshold = 0.05, flow = 0)
  
  print("Number of edges before pruning")
  print(length(E(g)))
  
  print("Number of edges after pruning:")
  print(R$n_residualEdges)
  
  print("Number of pruned edges:")
  print(R$n_cuttedEdges)
  
  print("Number of strong edges:") 
  print(R$n_strong)

  time  <- proc.time() - ptm
  print(time)
}