#' Driver script to replicate experiments described in 
#' https://www.worldscientific.com/doi/10.1142/S0129065719500072 
#' 
#' First, borda voting is applied to input graphs, then Strong Weak Pruning is
#' used to detect relevant edges and finally the results are aggregated to 
#' generate masks ready to be applied to the inputs graphs.
#' 
#'  Output objects are saved in specified folders (see parameters).
#' 
#' Author: Emanuele Pesce

rm(list = ls())

# import libraries
source("../src/borda_voting.R", chdir = T)
source("../src/borda_mask.R", chdir = T)
source("../src/SWP.R", chdir = T)
source("../src/graph_utils.R", chdir = T)


#' -----------------------------------------------------------------------------
#' Function definitions
#'

borda_voting <- function(pathInp, pathOut) {
  #' Computes the Borda voting on inputs graphs and stores the borda output
  #' in the specified paths.
  #' 
  #' @param pathInp, list of directory paths; each directory should contain a 
  #'                 set of graphs stored as txt files.
  #' @param pathOut, list of paths where borda outputs will be stored to; 
  #'                 the list must have the same length of pathInp.

  # preparing input to Borda
  mControls = genMatrix(pathInp)
  
  # compute Borda voting
  outBorda = Borda(mControls,space = mControls)
  
  # saving
  writeBordaMatrix(filename = pathOut, outBorda$TopK$mean, outBorda$Scores$mean)
}

#' -----------------------------------------------------------------------------
#' Parameters
#'

# input group directories
borda_inps <- list("Controls/",
              "SLA2/",
              "SLA3/")

# borda group outputs 
borda_outs <- list("borda_matrix_controls.txt",
              "borda_matrix_SLA2.txt",
              "borda_matrix_SLA3.txt")

# threshold related output path
path_tr_out <- "../data/other/borda/threshold_0dot04/"

# output paths
path_borda_mask_swp <- paste(path_tr_out, "borda_mask_ws_cutting.csv", sep = "")
path_borda_mask_swp_num <- paste(path_tr_out, "borda_mask_ws_cutting_num.csv", sep = "")
path_borda_mask_swp_info <- paste(path_tr_out, "borda_mask_ws_cutting_info.csv", sep = "")

borda_swp_obj <- paste(path_tr_out, "borda_sw_cut_objects.RData", sep = "")

# SWP threshold
swp_tr = 0.04

# verbosity level
verbose = 2

#' -----------------------------------------------------------------------------
#' Execution
#'

ptm <- proc.time()

# -------------------------------
if(verbose > 0){
  print("Computing Borda voting")
}

if(interactive()){
  for (i in 1:length(borda_inps)){
    pathInp <- paste("../data/structural/naive/", borda_inps[[i]], sep = "", collapse = NULL)
    pathOut <- paste("../data/other/borda/", borda_outs[[i]], sep = "", collapse = NULL)
    borda_voting(pathInp, pathOut)
  }
}

# -------------------------------
if(verbose > 0){
  print("Create Borda SWP masks")
}

# read borda graphs
borda_graphs <- list()
for (i in 1:length(borda_inps)){
  pathInp <- paste("../data/other/borda/", borda_outs[[i]], sep = "", collapse = NULL)
  borda_graphs[[i]] <- i_adjacencyFromFile(pathInp)
}

# compute SWP on borda graphs and get residual graphs as well
swp_graphs <- list()
for (i in 1:length(borda_inps)){
  swp_graphs[[i]] <- sw_cutting(borda_graphs[[i]], threshold = swp_tr, flow = 0)
}

# save borda swp graph objects 
# variables names are kept as following for compatibility purposes
r_Controls <- swp_graphs[[1]]
r_SLA2 <- swp_graphs[[2]]
r_SLA3 <- swp_graphs[[3]]
save(r_Controls, r_SLA2, r_SLA3, file = borda_swp_obj)

# compute the union mask:
# - compute group individual masks first
# - merge all group masks
group_masks <- list()
for (i in 2:length(borda_inps)){
  group_masks[[i-1]] <- unionMask(swp_graphs[[1]]$residualGraph, swp_graphs[[i]]$residualGraph)
  if (i>2){
    union_mask <- rbind(group_masks[[i-2]],group_masks[[i-1]])
    union_mask <- unique(union_mask)
  }
}

if(verbose > 1){
  print("Checking union results")
  
  print("Edges in Control group")
  print(swp_graphs[[1]]$n_residualEdges)
  print("Edges in SLA2 group")
  print(swp_graphs[[2]]$n_residualEdges)
  print("Edges in SLA3 group")
  print(swp_graphs[[3]]$n_residualEdges)
  print("Edges in Union")
  print(dim(union_mask)[1])
  print("Union - Controls")
  print(r1 <- dim(union_mask)[1] - swp_graphs[[1]]$n_residualEdges)
  print("Union - SLA2")
  print(r2 <- dim(union_mask)[1] - swp_graphs[[2]]$n_residualEdges)
  print("Union - SLA3")
  print(r3 <- dim(union_mask)[1] - swp_graphs[[3]]$n_residualEdges)
}


# -------------------------------
if(verbose > 0){
  print("Saving computed objects")
}

# saving objects in compatibility mode for further analysis
out <- list("e_controls" = swp_graphs[[1]]$n_residualEdges, 
            "e_SLA2" = swp_graphs[[2]]$n_residualEdges, 
            "e_SLA3" = swp_graphs[[3]]$n_residualEdges, 
            "e_union" = dim(union_mask)[1], 
            "e_union_m_controls" = r1,
            "e_union_m_SLA2" = r2, 
            "e_union_m_SLA3" = r3 )
write.csv(out, file = path_borda_mask_swp_info)
write.table(union_mask,file= path_borda_mask_swp, sep="\t", col.names = F, row.names = F)

# create save a numeric mask (without "V" in cells)
new_mask <- matrix(nrow = dim(union_mask)[1], ncol = 2)
for(i in 1:dim(union_mask)[1]){
  tmp <- gsub("V","", union_mask[i, 1])
  new_mask[i,1] <- as.integer(tmp)
  tmp <- gsub("V","", union_mask[i, 2])
  new_mask[i,2] <- as.integer(tmp)
}
write.table(new_mask,file= path_borda_mask_swp_num, sep="\t", col.names = F, row.names = F)

save.image(file = paste(path_tr_out, "driver_create_borda_mask.RData", sep =""))

time  <- proc.time() - ptm
print(time)