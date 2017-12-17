import numpy
import louvain
import igraph as ig
import sys
g = ig.Graph.Read_Ncol(sys.argv[1], directed = False)
n = g.vcount()
part = louvain.find_partition(g, method="Modularity")
membership = part.membership
numpy.savetxt(fname = sys.argv[2], X = membership)
