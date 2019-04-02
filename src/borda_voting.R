#' borda_voting
#' 
#' functions for pruning and aggregating edges of graphs
#' It means that given a set of graphs it will be done a voting in order to
#' get a ranking of the edges
#'  
#' Author: Emanuele Pesce
library(TopKLists)
source("../src/graph_utils.R")

#' Aggregates matrices
#' Get all matrices in 'path' and merge them all in a new matrix in which each 
#' row is a subject (file) and each column is a value
#' Required to perform borda voting afterwards
#'  
#' @param path, path of matrices to aggregate
#' @return A list of vectors
#' @examples
#' M <-  m <- genMatrix("./../../data/toyData/controls/")
genMatrix <- function(path = "./../../data/structural/naive/Controls/") {
  print(path)
  fileNames  <- list.files(path, pattern = "*.txt")
  nRoi = 90
  
  v_names <- list()
  for (i in 1:nRoi){
    si <- toString(i)
    for (j in 1:nRoi){
      sj <- toString(j)
      v_names <- c(v_names, paste(si, sj, sep = "_"))
    }
  }
  
  M <- list()
  for (i in 1:length(fileNames)){
    fname <- fileNames[i]
    dat <- read.csv(paste(path, fname, sep = ""), header = FALSE, sep = " ")
    m <- as.matrix(dat);
    v <- as.vector(t(m));
    names(v) <- v_names
    v <- sort(v, decreasing = TRUE)
    M[[i]] <- names(v)
  }
  return(M)
}


writeBordaMatrix <- function(filename, bordaTop, bordaScores, n=90){
  vec <- bordaScores
  names(vec) <- bordaTop
  M <- matrix(nrow = 90, ncol = 90)
  for(i in 1:n){
    for(j in 1:n){
      idx <- paste(toString(i), toString(j), sep = "_")
      M[i,j] <- vec[idx]
    }
  }
  write.table(M, file = filename, row.names = FALSE, col.names=FALSE )
}