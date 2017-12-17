Args <- commandArgs(trailingOnly = TRUE)
Type <- Args[1]
nsims <- as.integer(Args[2])
#Type <- "sqrt"
#nsims <- 10
library(igraph)
source("process_c_results.R")
data_dir <- "data"
louvain.run.program <- "gen-louvain/louvain"

# ------------------------------------------------------------------------------

pars <- 2^(0:7) * 100

times <- mods <- rep(list(matrix(0, nsims, length(pars))), 3)
names(times) <- names(mods) <- c("R", "Python", "C++")

for (i in seq_along(pars)) {
  
  for (j in 1:nsims) {
  
    cat("doing par", i, "\n")
    n <- pars[i]
    data_dir_t <- file.path(data_dir, Type)
    
    # Running R & saving results
    cat("--running R louvain\n")
    fn <- file.path(data_dir_t, paste0(n, "_", j, ".dat"))
    edge.list <- as.matrix(read.table(fn, header=FALSE))
    G <- graph.edgelist(edge.list, directed=FALSE)
    timer <- proc.time()[3]
    res <- cluster_louvain(G)
    timer <- proc.time()[3] - timer
    times[["R"]][j, i] <- timer
    mods[["R"]][j, i] <- tail(res$modularity, 1)
    
    # Running python
    cat("--running Python louvain\n")
    input.fn <- fn
    output.fn <- file.path(data_dir_t, paste0(n, "_py_results.dat"))
    timer <- proc.time()[3]
    system(paste("python python_louvain.py", input.fn, output.fn))
    timer <- proc.time()[3] - timer
    py.membership <- as.integer(readLines(output.fn)) + 1
    py.mod <- modularity(G, py.membership)
    times[["Python"]][j, i] <- timer
    mods[["Python"]][j, i] <- py.mod
    
    # Running C++
    cat("--running C++ louvain\n")
    input.fn <- fn
    output.fn <- file.path(data_dir_t, paste0(n, "_c_results.dat"))
    bin.fn <- file.path(data_dir_t, paste0(n, "_", j, ".bin"))
    timer <- proc.time()[3]
    system(paste(louvain.run.program, bin.fn, 
                 "-l -1 -v -q id_qual >", output.fn))
    timer <- proc.time()[3] - timer
    c.membership <- process_c_results(output.fn)
    c.mod <- modularity(G, c.membership)
    times[["C++"]][j, i] <- timer
    mods[["C++"]][j, i] <- c.mod
  }
}

save(times, mods, pars, 
     file = file.path(data_dir, paste0(Type, "_res.RData")))
