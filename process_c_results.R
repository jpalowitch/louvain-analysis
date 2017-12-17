process_c_results <- function (fn) {
  
  # Getting results file and splitting by partition
  comm_table <- read.table(fn, header=FALSE)
  zero_locs <- which(comm_table$V1 == 0)
  zero_locs <- c(zero_locs, nrow(comm_table) + 1)
  comm_dfs <- lapply(2:length(zero_locs), function (i) {
      comm_table[(zero_locs[i - 1] + 1):(zero_locs[i] - 1), ]})
  n <- nrow(comm_dfs[[1]])
  
  # Processing first partition
  K <- max(comm_dfs[[1]]$V2)
  partitions <- list(lapply(1:n, identity))

  for (j in seq_along(comm_dfs)) {
    K <- max(comm_dfs[[j]]$V2)
    partitions[[j + 1]] <- lapply(1:K, function (c) {
        unlist(partitions[[j]][comm_dfs[[j]]$V2 == c])
    })
  }
  
  # Formatting and returning
  membership <- integer(n)
  for (i in seq_along(partitions[[j + 1]])) {
    membership[partitions[[j + 1]][[i]]] <- i
  }
  return(membership)
}