setwd("C:/Users/mathi/Documents/uni_courses/AI_2019/code/delivery_man")

myFunction2 <- function (roads, car, packages) { 
  
  # Initialise variables
  nextMove = 0
  toGo = 0
  #offset = 0
  
  car_pos <- c(car$x, car$y)
  
  # We are not carrying a package
  if (car$load == 0) {
    
    # Packages with a zero in column 5 are not delivered
    not_delivered <- which(packages[, 5] == 0)
    
    # Position of packages is located on columns 1 to 2
    package_positions <- packages[not_delivered, 1:2, drop=F]
    
    n_not_delivered = length(not_delivered)
    
    distances = numeric(n_not_delivered)
    
    for(i in 1:n_not_delivered) {
      
      distances[i] = manhattan_distance(car_pos, package_positions[i,])
      
    }
    
    index_smallest_distance = which.min(distances)
    
    # Goal is the package closest(manhattan distance)
    toGo = not_delivered[index_smallest_distance]
    goal <- packages[toGo, 1:2]
    
    
    # We are carrying a package
  } else {
    
    # Goal is the package delivery destination
    toGo <- car$load
    #offset <- 2
    goal <- packages[toGo, 3:4]
  }
  
  if(goal[1] == car_pos[1] && goal[2] == car_pos[2]) {
    
    nextMove <- 5
    
  } else {
    
    # Path we choose is determined by A*
    nextMove <- a_star(car_pos, goal, roads)
    
  }
  
  car$nextMove <- nextMove
  car$mem <- list()
  
  return(car)
}

# Helper functions

# Calculates manhattan distance between two positions specified by coord_1 and coord_2
manhattan_distance <- function(coord_1, coord_2) {
  distance <- dist(rbind(coord_1, coord_2), 'manhattan')
  return(distance[1])
}


# A* implementation
a_star <- function(start_pos, goal, roads) {
  expanded = list()
  frontier = list()
  
  # Get roads (edge costs)
  h_roads <- roads$hroads
  v_roads <- roads$vroads
  
  # Initial frontier is immediately expanded
  h_start = manhattan_distance(start_pos, goal)
  start_node <- list(pos = start_pos, cost = 0, h = h_start, score = h_start, path = list())
  
  #Initial node, node position = start_node, cost 0,
  # h = h_start, path is empty
  expanded <- start_node
  
  while(expanded$pos[1] != goal[1] || expanded$pos[2] != goal[2]) {
    
    expanded_x <- expanded$pos[1]
    expanded_y <- expanded$pos[2]
    
    #### Add left neighbor to frontier #####
    pos <- c(expanded_x - 1, expanded_y)
    x <- pos[1]
    y <- pos[2]
    if( x > 0) {
      edge_cost <- h_roads[x, y]
      frontier <- update_frontier(pos, edge_cost, expanded, frontier, goal)
    }
    #### Add right neighbor to frontier #####
    
    pos <- c(expanded_x + 1, expanded_y)
    x <- pos[1]
    y <- pos[2]
    if( x-1 < 10) {
      edge_cost <- h_roads[x - 1, y]
      frontier <- update_frontier(pos, edge_cost, expanded, frontier, goal)
    }
    
    #### Add down neighbor to frontier #####
    pos <- c(expanded_x, expanded_y - 1)
    x <- pos[1]
    y <- pos[2]
    if( y > 0) {
      edge_cost <- v_roads[x, y]
      frontier <- update_frontier(pos, edge_cost, expanded, frontier, goal)
    }
    #### Add up neighbor to frontier #####
    pos <- c(expanded_x, expanded_y + 1)
    x <- pos[1]
    y <- pos[2]
    if( y-1 < 10) {
      edge_cost <- v_roads[x, y - 1]
      frontier <- update_frontier(pos, edge_cost, expanded, frontier, goal)
    }
    
    #### Expand the node with least score #### 
    scores <- sapply(frontier, function(item)item$score)
    best_index <- which.min(scores)
    
    # Arbitralily break ties
    if(length(best_index) > 1) {
      min_h <- 2000
      for(i in best_index){
        
        min_h <- min(frontier[[i]]$h, min_h)
      }
      best_index <- best_index[which(best_index == min_h)]
      
    }
    
    if(length(best_index) > 1) {
      best_index <- best_index[1]
    }
    
    
    expanded <- frontier[[best_index]]
    frontier <- frontier[-best_index]
    
  } # End of while
  
  if(length(expanded$path) < 2){
    
    move_to <- expanded$pos
    
  } else {
    
    move_to <- expanded$path[[2]]
  }
  
  move_to_x <- move_to[1]
  move_to_y <- move_to[2]
  
  start_x <- start_pos[1]
  start_y <- start_pos[2]
  
  
  next_move <- NULL
  
  # Move to the left
  if(move_to_x < start_x) {
    next_move <- 4
  }
  
  # Move to the right
  if(move_to_x > start_x) {
    next_move <- 6
  }
  
  # Move down
  if(move_to_y < start_y) {
    next_move <- 2
  }
  
  # Move up
  if(move_to_y > start_y) {
    next_move <- 8
  }
  
  return(next_move)
}


update_frontier <- function(node_pos, edge_cost, expanded_node, frontier, goal) {
  node_x <- node_pos[1]
  node_y <- node_pos[2]
  
  h <- manhattan_distance(node_pos, goal)
  
  cost <- edge_cost + expanded_node$cost
  
  score <- cost + h
  
  if(length(expanded_node$path) < 2){
    path <- append(expanded_node$path, list(expanded_node$pos))
  } else {
    path <- expanded_node$path
  }
  
  
  index <- NULL
  
  if(length(frontier) > 0) {
    for(i in 1:length(frontier)) {
      curr_pos <- frontier[[i]]$pos
      if(curr_pos[1] == node_x && curr_pos[2] == node_y) {
        index <- i
      }
    }
  }
  
  # Node already exists in frontier

  if(!is.null(index)) {
    
    old_score <- frontier[[index]]$score
    
    
    # New path is better
    if(score < old_score){
      frontier[[index]]$score <- score
      frontier[[index]]$path <- path
      frontier[[index]]$cost <- cost
      frontier[[index]]$h <- h
      
      return(frontier)
      
    } else {
      # Return frontier unchanged
      return(frontier)
      
    }
  }
  # Node does not exist in frontier
  neighbor <- list(pos = node_pos, cost = cost, h = h, score = score, path = path)
  frontier <- append(frontier, list(neighbor))
  return(frontier)
  
}
