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
  cluster <- clusters(graph)
  graph$connected <- TRUE
  if(cluster$no > 1)
  {
    graph <- induced.subgraph(graph, which(cluster$membership == which(cluster$csize==max(cluster$csize))[1]))
    graph$connected <- FALSE
  }
  
  V(graph)$id <- 1:vcount(graph)
  graph$layout <- layout.fruchterman.reingold(graph)
  return (graph)
}

loadPlotData <- function( g, src, size )
{
  inf <- vector(mode="numeric", length=size)
  inf[1] <- src
  
  boundary <- neighbors(g, src)
  
  if( size > 1 )
  {
    for( i in 2:size )
    {
      if(length(boundary) > 1)
        victim <- sample(boundary, 1)
      else
        victim <- boundary
      boundary <- boundary[boundary!=victim]
      inf[i] <- victim
      neib <- neighbors(g, victim)
      uNeib <- neib[!neib %in% inf]
      boundary <- append(boundary, uNeib)
    }
  }
  
  rtn <- list()
  rtn$g <- g
  rtn$inf <- inf
  
  return (rtn)
}

genSource <- function(graph)
{
  eccentricity <- eccentricity(graph)
  eccentricityC <- V(graph)$id[which(eccentricity==min(eccentricity))]
  neib <- unique(unlist(neighborhood(graph, 3, eccentricityC)))
  return(sample(neib,1))
}

assignGroup <- function(group, center, name)
{
  if(length(center)==0)
    return( group )
  
  rtn <- group
  for(i in 1:length(group$center))
  {
    if( group$center[i] %in% center )
    {
      rtn$legend[i] <- paste(rtn$legend[i], "+", name)
      return( rtn )
    }
  }
  rtn$center <- append(rtn$center, center[1])
  rtn$legend <- append(rtn$legend, name)
  return(rtn)
}

rumor.centrality <- function(G,GN)
{
  if( vcount(GN)==1 )
    return(1)
  
  score <- vector(mode="numeric", length=vcount(GN))
  NFAC <- factorial(vcount(GN))
  for(i in 1:vcount(GN))
  {
    v <- V(GN)[i]
    bfs <- graph.bfs(GN, v, father=TRUE)
    bfsTree <- graph.edgelist(cbind(bfs$order,bfs$father[bfs$order])[-1,], directed=FALSE)
    up_rtn <- rc_up(bfsTree, -1, v)
    rc <- NFAC/up_rtn[2]
    prob <- bfsProb(G, V(GN)$id[bfs$order])
    
    score[i] <- rc*prob
  }
  
  return(score)
}

rc_up <- function(Tree, parent, child)
{
  t_child <- as.numeric(1)
  p_child <- as.numeric(1)
  
  neib <- neighbors(Tree, child)
  if( length(neib)>0 )
  {
    for(i in 1:length(neib))
    {
      if( neib[i] != parent )
      {
        rc_rtn <- rc_up(Tree, child, neib[i])
        t_child <- t_child + rc_rtn[1]
        p_child <- p_child*rc_rtn[2]
      }
    }
  }
  p_child <- p_child*t_child
  
  return(c(t_child, p_child))
}

bfsProb <- function(G, order)
{
  prob <- as.double(1)
  boundary <- neighbors(G, order[1])
  for(i in 2:length(order))
  {
    count <- table(boundary)
    prob <- prob*count[names(count)==order[i]]/length(boundary)
    boundary <- append(boundary, neighbors(G, order[i]))
    boundary <- boundary[!boundary %in% order[1:i]]
  }
  return(prob)
}