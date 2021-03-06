#' graph_utils
#' 
#' Utility functions used to work with graphs
#' 
#' Author: Emanuele Pesce
library(igraph)

#' Get the adjacency matrix from a file
#' @param filename name of the file containing the graph
#' @return m adjacency matrix of graph in filename
#' @examples
#' A  <-   graph <- getMatrixFromFile("./file.txt")
getMatrixFromFile <- function(filename, sep = " "){
  dat <- read.csv(filename, header = FALSE, sep = sep)
  m <- as.matrix(dat);
  
  return(m)
}

#' Get the edges specified in a list
#' @param graph graph
#' @param listNames list of the names of the edges to retrieve. Edges are
#' specified in the following format: row_col. row and col are both integers.
#' @return m list of edges specified in listNames
#' @examples
#' edges <- getEdgesAsVector(graph, c("1_2",3_4") for retriving edges [1,2] and 
#' [3,4]
getEdgesAsVector <- function(graph, listNames){
  listEdges <- list()
  for(i in 1:length(listNames)){
    s <- listNames[i]
    ss <- strsplit(s, "_")
    row <- strtoi(ss[[1]][1])
    col <- strtoi(ss[[1]][2])
    listEdges[i] <- graph[row, col]
  }
  
  return(listEdges)
}

#' Get the adjancency matrix of a file
#' @param filename name of the file containing the graph (csv with separator = " ")
#' @return g igraph adjacency matrix
#' @examples
#' g <- i_adjacencyFromFile("graph.csv")
i_adjacencyFromFile <- function(filename){
  dat <- read.csv(filename, header = FALSE, sep = " ");
  m <- as.matrix(dat);
  g <- graph.adjacency(m, mode = "directed", weighted = TRUE);
  
  return(g)
}

#' Removes an edge
#' @param v1 vertex
#' @param v2 vertex
#' @return g igraph without edge v1 -> v2
#' @examples
#' g1 <- removeEdge(g, 90, 89)
removeEdge <- function(graph, v1, v2){
  sv1 <- toString(v1)
  sv2 <- toString(v2)
  sv1 <- paste("V", sv1, sep = "")
  sv2 <- paste("V", sv2, sep = "")
  graph[sv1, sv2] <- FALSE
  
  return(graph)
}