#' A forceNetwork of an igraph object
#' 
#' This function returns a javascript/html object of d3 force network
#' 
#' @param graph An instance of class \code{igraph}
#' @param weight The graph edge attribute used for weighting the graph.
#' @param group The vertex attribute used for coloring the graph.
#'
#' @import networkD3
#' @import igraph
#' @import dplyr
forceNetwork.igraph <- function(g, weight, group){
  
  # must be an instance of class igraph
  if(class(g) != "igraph"){
    stop("`graph` must be an instance of class `igraph`")
  } 
  
  # nodes must be numbered 1:n, as per 
  # http://stackoverflow.com/a/30364358/843419
  V(g)$id <- V(g)$name
  V(g)$name <- 1:length(igraph::V(g))
  
  # get a table of the link attributes
  links <- dplyr::data_frame(
    from = get.edgelist(g)[, 1],
    to   = get.edgelist(g)[, 2],
    weight = rpois(length(from), 2)
  )
  
  links <- filter(links, weight > 0)  # drop zero weight edges
  
  # get a table of the node attributes
  nodes <- dplyr::data_frame(
    name = V(g)$name,
    group = sample(1:3, size = length(V(g)), replace = TRUE)
  )
  
  # make the plot
  d <- networkD3::forceNetwork(
    Links = links,
    Source = "from", Target = "to", Value = "attr",
    Nodes = nodes, 
    NodeID = "name", Group = "group" 
  ) 
  
  return(d)
}