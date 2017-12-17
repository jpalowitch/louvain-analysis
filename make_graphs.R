Args <- commandArgs(trailingOnly = TRUE)
Type <- Args[1]
nsims <- as.integer(Args[2])
cat(Type)
library(igraph)
source("igraph_recast.R")
data_dir <- "data"
pars <- 2^(0:7) * 100
louvain.convert.program <- "gen-louvain/convert"

# ------------------------------------------------------------------------------

if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}

for (i in seq_along(pars)) {
  
  for (j in 1:nsims) {
  
    cat("doing par", i, "sim", j, "\n")
    data_dir_t <- file.path(data_dir, Type)
    if (!dir.exists(data_dir_t)) {
      dir.create(data_dir_t)
    }
    
    # Making graph
    n <- pars[i]
    if (Type == "linear") {
      p <- 1 / 2
    } 
    if (Type == "log-sqrt") {
      p <- 3 * sqrt(log(n) / n)
    } 
    if (Type == "sqrt") {
      p <- 3 * sqrt(1 / n)
    } 
    if (Type == "log") {
      p <- 3 * log(n) / n
    } 
    if (Type == "constant") {
      p <- 3 / n
    }
    G <- erdos.renyi.game(n, p)
    edge.list <- get.edgelist(G)
    
    # Making sure all nodes have an edge
    if (sum(degree(G) == 0) > 0) {
      node1 <- which(degree(G) == 0)
      node2 <- sample(1:n, length(node1), replace=TRUE)
      edge.list <- rbind(edge.list, cbind(node1, node2))
      G <- graph.edgelist(edge.list, directed=FALSE)
      edge.list <- get.edgelist(G)
    }
    
    # Reformatting graph
    ig.recast <- igraph_recast(edge.list)
    edge.list <- ig.recast$edgelist
    
    # Saving graph
    fn <- file.path(data_dir_t, paste0(n, "_", j, ".dat"))
    write.table(edge.list, quote=FALSE, row.names=FALSE, col.names=FALSE, file=fn)
    
    # Converting graph
    bin.fn <- file.path(data_dir_t, paste0(n, "_", j, ".bin"))
    system(paste(louvain.convert.program, "-i", fn, "-o", bin.fn))
    
  }
  
}