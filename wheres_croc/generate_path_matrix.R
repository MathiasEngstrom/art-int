generate_path_matrix <- function(edges) {
  
  path_matrix <- matrix(0, 40, 40)
  
  for(i in 1:40) {
    
    
    for(j in 1:40) {
      
      if(i != j ) {
        
        path <- best_first(i, j, edges)
        path_matrix[i,j] <- path[1]
        
        }
      } 
    }
  
  return(path_matrix)
  
}


# best_first implementation
best_first <- function(start_node, goal, edges) {
  expanded = list()
  frontier = list()
  
  start_node <- list(node = start_node, cost = 0, path = list())
  
  #Initial node, node position = start_node, cost 0,
  # h = h_start, path is empty
  expanded <- start_node
  
  while(expanded$node != goal) {

    neighbors_1 <- edges[which(edges[, 1] == expanded$node), 2]
    
    neighbors_2 <- edges[which(edges[, 2] == expanded$node), 1]
    
    neighbors <- c(neighbors_1, neighbors_2)
    
    edge_cost = 1
    #### Add neighbors to frontier #####
    for(node in neighbors){
      frontier <- update_frontier(node, edge_cost, expanded, frontier, goal)
    }
    
    #### Expand the node with least score #### 
    cost <- sapply(frontier, function(item)item$cost)
    best_index <- which.min(cost)
    
    expanded <- frontier[[best_index]]
    frontier <- frontier[-best_index]
    #print(frontier$path)
    
    #print(length(frontier))
  } # End of while
  
  if(length(expanded$path) < 2){
    
    path <- expanded$node
    return(path[[1]])
    
  } else {
    
    path <- expanded$path
    return(path[[2]])
  }
  
 

}


update_frontier <- function(node, edge_cost, expanded_node, frontier, goal) {

  cost <- edge_cost + expanded_node$cost
  
  path <- append(expanded_node$path, expanded_node$node)

  index <- NULL
  
  if(length(frontier) > 0) {
    for(i in 1:length(frontier)) {
      curr_node <- frontier[[i]]$node
      if(curr_node == node) {
        index <- i
      }
    }
  }
  
  # Node already exists in frontier
  
  if(!is.null(index)) {
    
    old_cost <- frontier[[index]]$cost
    
    
    # New path is better
    if(cost < old_cost){
      frontier[[index]]$path <- path
      frontier[[index]]$cost <- cost
      
      return(frontier)
      
    } else {
      # Return frontier unchanged
      return(frontier)
      
    }
  }
  # Node does not exist in frontier
  neighbor <- list(node = node, cost = cost, path = path)
  frontier <- append(frontier, list(neighbor))
  return(frontier)
  
}