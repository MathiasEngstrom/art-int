makeMoves <- function (moveInfo, readings, positions, edges, probs) {

  # These are only necessary to perform once
  if(moveInfo$mem$status == 0) {
    # Matrix with best moves(shortest path) given any two nodes
    path_matrix <- generate_path_matrix(edges)
    moveInfo$mem$path_matrix <- path_matrix
  
    
    # Transition matrix
    transition_matrix <- matrix(0, 40, 40)
    for(i in 1:40) {
      
      # Neighbors from edges can occur in column 1 or 2
      neighbors_1 <- edges[which(edges[, 1] == i), 2]
      neighbors_2 <- edges[which(edges[, 2] == i), 1]
      
      # Include the node itself, since croc can stay put
      neighbors <- c(i, neighbors_1, neighbors_2)
      
      # The transition probabilities are uniformly distributed,
      # with probability 1/(possible nodes to move to or stay)
      transition_matrix[i, neighbors] <- 1/length(neighbors)   
    }
    moveInfo$mem$transition_matrix <- transition_matrix
    
    
  }
  
  if(moveInfo$mem$status == 1 || moveInfo$mem$status == 0){
    # Initial state s0, all locations except where we and tourists
    # are are equally likly
    s0 <- rep(1, 40)
    s0[positions] <- 0
    n_zero_prob <- length(s0[positions])
    s0 <- s0/(40 - n_zero_prob)
    moveInfo$mem$state <- s0
    
    moveInfo$mem$status <- 2
  }
  
  s0 <- moveInfo$mem$state
  backpacker_1 <- positions[1]
  backpacker_2 <- positions[2]
  ranger <- positions[3]
  
  # Backpacker 1 has died, croc is here with prob 1
  if(backpacker_1 < 0 && !is.na(backpacker_1)) {
    s1 <- numeric(40)
    s1[-backpacker_1] <- 1
  
  # Backpacker 2 has died, croc is here with prob 1  
  } else if(backpacker_2 < 0 && !is.na(backpacker_2)) {
    s1 <- numeric(40)
    s1[-backpacker_2] <- 1
  } else {
    
    # Croc cant be on the same nodes as the backpackers or the ranger
    if(!is.nan(backpacker_1)) {
    
      s0[backpacker_1] <- 0
    } 
    
    if(!is.nan(backpacker_2)) {
      
      s0[backpacker_2] <- 0
    }
    
    s0[ranger] <- 0
    
    # Calculate a new state s1
    transition_matrix <- moveInfo$mem$transition_matrix
    
    probabilities <- calc_probabilitities(readings, probs)
    
    s1 <- s0 %*% transition_matrix %*% diag(40,40)*probabilities

  }
  

  highest_prob <- which.max(s1)

  move <- moveInfo$mem$path_matrix[ranger, highest_prob]

  
  # Update moveInfo and return
  moveInfo$moves <- c(move, 0)
  moveInfo$mem$state <- s1
  
  return(moveInfo)
}



#----------------- Help functions --------------------------------------------#

##### Calculate probability ################

calc_probabilitities <- function(readings, probs) {
  salinity_mean <- probs$salinity[,1]
  salinity_std <- probs$salinity[,2]
  
  nitrogen_mean <- probs$nitrogen[,1]
  nitrogen_std <- probs$nitrogen[,2]
  
  phosphate_mean <- probs$phosphate[,1]
  phosphate_std <- probs$phosphate[,2]
  
  salinity_reading <- readings[1]
  phosphate_reading <- readings[2]
  nitrogen_reading <- readings[3]
  
  
  salinity_prob <- dnorm(salinity_reading, salinity_mean, salinity_std)
  nitrogen_prob <- dnorm(nitrogen_reading, nitrogen_mean, nitrogen_std)
  phosphate_prob <- dnorm(phosphate_reading, phosphate_mean, phosphate_std)
  
  probabilities <- salinity_prob*nitrogen_prob*phosphate_prob
  
  probabilities_normalised <- probabilities
  
  return(probabilities_normalised)
  }

###### Generate the path matrix ############

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


####### best_first implementation ############

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

########### update the frontier for best first #################

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

