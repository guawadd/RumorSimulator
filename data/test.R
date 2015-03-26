library("igraph")

loadGraph <- function( file )
{
  lines <- readLines(file)
  # currently not using the first line indicating number of vertices
  lines <- lines[-1]
  
  edgelist <- matrix(, ncol=2, nrow=as.numeric(lines[1]))
  # drop first line - number of edges
  lines <- lines[-1]
  for(i in 1:length(lines))
    edgelist[i,] <- as.numeric(unlist(strsplit(lines[i]," ")))
  
  graph <- graph.edgelist(edgelist, directed=FALSE)
  V(graph)$id <- 1:vcount(graph)
  graph$layout <- layout.fruchterman.reingold(graph)
  
  
  cluster <- clusters(graph)
  print(cluster$membership)
  #return (graph)
}
loadGraph("powergrid.txt")