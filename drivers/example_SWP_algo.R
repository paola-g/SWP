# example for running strong weak ties cutting algorithm 

source("../src/sw_cutting.R", chdir = T)

if(interactive()){
  ptm <- proc.time()
  
  # input here: acquiring a graph 
  # the import thing is that g should be in igraph format 
  # (Basically here i_adjacencyFromFile parse txt matrix and then calls graph.adjacency())
  g <- i_adjacencyFromFile("../data/toy/example_data_borda_aggregation_matrix.txt") 
  
  R <- sw_cutting(g, threshold = 0.05, flow = 0)
  
  print("Number of edges before cutting")
  print(length(E(g)))
  
  print("Number of edges after cutting:")
  print(R$n_residualEdges)
  
  print("Number of cutted edges:")
  print(R$n_cuttedEdges)
  
  print("Number of utils edges:") 
  print(R$n_strong)

  time  <- proc.time() - ptm
  print(time)
}