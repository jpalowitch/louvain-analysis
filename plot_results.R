#Args <- commandArgs(trailingOnly = TRUE)
#nsims <- as.integer(Args[2])
nsims <- 10
library(igraph)
library(ggplot2)
library(reshape2)
data_dir <- "data"
louvain.run.program <- "~/Documents/code/gen-louvain/louvain"

# ------------------------------------------------------------------------------

# Getting types and setting up overall plot dfs
Types <- list.dirs(data_dir, full.names=FALSE)
Types <- setdiff(Types, "")
plotdf_mod_full <- plotdf_time_full <- NULL

for (Type in Types) {
  
  load(file.path(data_dir, paste0(Type, "_res.RData")))
  
  # Plot data format function
  format_and_process <- function (df, metric.name) {
    colnames(df) <- pars
    means <- colMeans(df)
    ses <- apply(df, 2, sd)
    ses <- ses * 2 * qt(.975, df=nrow(df) - 1) / sqrt(nrow(df))
    datadf <- data.frame(Metric=metric.name, n=names(means),
                         Mean=means, Upper=means + ses, Lower=means - ses)
    rownames(datadf) <- NULL
    return(datadf)
  }
  
  # Formatting overall plot data
  plotdf <- do.call(rbind, c(lapply(times, format_and_process, "Time"),
                             lapply(mods, format_and_process, "Modularity")))
  plotdf$Method <- sapply(rownames(plotdf), 
                          function (x) strsplit(x, ".", fixed=TRUE)[[1]][1])
  plotdf$n <- as.numeric(levels(plotdf$n)[as.integer(plotdf$n)])
  rownames(plotdf) <- NULL
  plotdf$Type <- Type
  
  # Splitting data by metric
  plotdf_time <- plotdf[plotdf$Metric == "Time", ]
  plotdf_mod <- plotdf[plotdf$Metric == "Modularity", ]
  
  # Converting to milliseconds
  plotdf_time[c("Mean", "Upper", "Lower")] <- 
      1000 * plotdf_time[c("Mean", "Upper", "Lower")]
  
  plotdf_time_full <- rbind(plotdf_time_full, plotdf_time)
  plotdf_mod_full <- rbind(plotdf_mod_full, plotdf_mod)
  
}

# Mapping negative lower time bounds to 1 for log scaling
plotdf_time_full$Lower[plotdf_time_full$Lower < 0] <- 1

p_t <- ggplot(plotdf_time_full, 
              aes(x=n, y=Mean, ymin=Lower, ymax=Upper, colour=Method)) +
  geom_point() + geom_line() + geom_errorbar(width=0.10) + scale_y_log10() + 
  labs(y="Milliseconds (log10)", x="Node Count") + facet_wrap(~Type) + 
  ggtitle("Runtime vs. Network Node Count") + scale_x_continuous(trans="log10")

p_m <- ggplot(plotdf_mod_full, 
              aes(x=n, y=Mean, ymin=Lower, ymax=Upper, colour=Method)) +
  geom_point() + geom_line() + geom_errorbar(width=0.10) + facet_wrap(~Type) +
  labs(y="Modularity", x="Node Count") + scale_x_continuous(trans="log10") + 
  ggtitle("Modularity vs. Network Node Count")

ggsave(file.path(data_dir, "time.png"), p_t)
ggsave(file.path(data_dir, "mod.png"), p_m)
