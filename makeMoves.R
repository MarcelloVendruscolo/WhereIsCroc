#List all the packages available
library()

#Install the WheresCroc package
install.packages("/Users/marcellovendruscolo/Documents/rstudio-workspace/WhereIsCroc/WheresCroc_1.2.2.tar.gz", repos = NULL, type = "source")

#Load DeliveryMan package for this session
library(WheresCroc)

#List all the packages currently loaded
search()

#Help function for calculating the observable probabilities (salinity, phosphate and nitrogen) of each waterhole given the observations from Croc's current location
current_waterholesProbabilites <- function(croc_locationReadings, waterholes_distributions) {
  
  observation_matrix <- matrix(data = 0, nrow = 40, ncol = 40)
  
  sum_salinity <- 0
  sum_phosphate <- 0
  sum_nitrogen <- 0
  
  for (i in 1:40) {
    sum_salinity <- sum_salinity + dnorm(croc_locationReadings[1], waterholes_distributions$salinity[i,1],waterholes_distributions$salinity[i,2])
    sum_phosphate <- sum_phosphate + dnorm(croc_locationReadings[2], waterholes_distributions$phosphate[i,1],waterholes_distributions$phosphate[i,2])
    sum_nitrogen <- sum_nitrogen + dnorm(croc_locationReadings[3], waterholes_distributions$nitrogen[i,1],waterholes_distributions$nitrogen[i,2])
  }

  salinity_normalisedProbabilities <- seq(0, 0, length.out = 40)
  phosphate_normalisedProbabilities <- seq(0, 0, length.out = 40)
  nitrogen_normalisedProbabilities <- seq(0, 0, length.out = 40)
  
  for (i in 1:40) {
    salinity_normalisedProbabilities[i] <- (dnorm(croc_locationReadings[1], waterholes_distributions$salinity[i,1], waterholes_distributions$salinity[i,2]) / sum_salinity)
    phosphate_normalisedProbabilities[i] <- (dnorm(croc_locationReadings[2], waterholes_distributions$phosphate[i,1], waterholes_distributions$phosphate[i,2]) / sum_phosphate)
    nitrogen_normalisedProbabilities[i] <- (dnorm(croc_locationReadings[3], waterholes_distributions$nitrogen[i,1], waterholes_distributions$nitrogen[i,2]) / sum_nitrogen)
  }
  
  for (diagonal in 1:40) {
    observation_matrix[diagonal,diagonal] <- (salinity_normalisedProbabilities[diagonal] * phosphate_normalisedProbabilities[diagonal] * nitrogen_normalisedProbabilities[diagonal])
  }
  
  return(observation_matrix)
}

#Help function to calculate the transition matrix considering a uniform distribution
compute_transitionMatrix <- function(edges_matrix) {
  
  transitionProbabilities_Matrix <- matrix(0, nrow = 40, ncol = 40)
  counter_neighbours <- seq(1, 1, length.out = 40)

  for (i in 1:dim(edges_matrix)[1]) {
    counter_neighbours[edges_matrix[i,1]] <- counter_neighbours[edges_matrix[i,1]] + 1
    counter_neighbours[edges_matrix[i,2]] <- counter_neighbours[edges_matrix[i,2]] + 1
  }
  
  for (i in 1:dim(edges_matrix)[1]) {
    transitionProbabilities_Matrix[edges_matrix[i,1],edges_matrix[i,1]] <- (1 / counter_neighbours[edges_matrix[i,1]])
    transitionProbabilities_Matrix[edges_matrix[i,2],edges_matrix[i,2]] <- (1 / counter_neighbours[edges_matrix[i,2]])
    transitionProbabilities_Matrix[edges_matrix[i,1],edges_matrix[i,2]] <- (1 / counter_neighbours[edges_matrix[i,1]])
    transitionProbabilities_Matrix[edges_matrix[i,2],edges_matrix[i,1]] <- (1 / counter_neighbours[edges_matrix[i,2]])
  }

  return(transitionProbabilities_Matrix)
}

#Help function to find the neighbours (adjacent nodes) of a given node
compute_neighbours <- function(waterhole, edges_matrix) {
  neighbours <- c()
  for (i in 1:dim(edges_matrix)[1]) {
    if (edges_matrix[i,1] == waterhole) {
      neighbours[length(neighbours) + 1] <- edges_matrix[i,2]
    }
    if (edges_matrix[i,2] == waterhole) {
      neighbours[length(neighbours) + 1] <- edges_matrix[i,1]
    }
  }
  return(neighbours)
}

calculate_shortestPath <- function(origin_node, destination_node, edges_matrix) {
  
  #Variable to store the distance between destination and origin nodes. All edges are considered to have same weight
  shortest_distance <- 0
  
  #Elementary node data structure
  node <- list('number' = 'undefined', 'parent' = 'undefined', 'distance' = 'undefined')
  #Graph data structure as list to represent the map
  graph <- list()
  #Queue data structure as list to optimize the BFS algorithm
  queue <- list()
  
  #Fulfill the graph data structure with 40 nodes
  for (index in 1:40) {
    node$number <- index
    graph[[index]] <- node
  }
  
  #Set the current ranger location as the start point and en-queue it.
  graph[[origin_node]]$parent <- origin_node
  graph[[origin_node]]$distance <- 0
  queue[[1]] <- graph[[origin_node]]
  
  while (length(queue) != 0) {
    
    #Create a temporary current_node holding the first element of the queue and de-queue it.
    current_node <- queue[[1]]
    queue <- queue[-1]
    
    #If the destination is reached, iterate back through the path to find out the best move from current ranger position
    if (current_node$number == destination_node) {
      shortest_distance <- current_node$distance
      #If the destination waterhole is far away (distance > 2), move as fast as possible to the destination area
      if (shortest_distance > 2) {
        while (current_node$distance != 2) {
          current_node <- graph[[current_node$parent]]
        }
        next_waterhole_moves <- c(graph[[current_node$parent]]$number, current_node$number)
      } 
      #If the destination waterhole is close (distance <= 2), move but also search the waterholes
      else {
        while (current_node$parent != origin_node) {
          current_node <- graph[[current_node$parent]]
        }
        next_waterhole_moves <- c(current_node$number, 0)
      }
      return(next_waterhole_moves)
    }
    #If the destination is not reached yet, explore the node and en-queue its adjacent nodes.
    else {
      children <- compute_neighbours(current_node$number, edges_matrix)
      for (index in 1:length(children)) {
        if (graph[[children[index]]]$distance == 'undefined') {
          graph[[children[index]]]$distance <- current_node$distance + 1
          graph[[children[index]]]$parent <- current_node$number
          queue[[length(queue) + 1]] <- graph[[children[index]]]
        }
      }
    }
  }
}

#move_information$moves is a vector (length 2) of numbers corresponding to the next waterhole destination and the search action
#move_information$mem is a memory list to record information for next rounds of the game
#croc_locationReadings is a vector (length 3) providing the salinity, phosphate and nitrogen readings from Croc's location
#humans_location is a vector is a vector (length 3) providing: tourist1, tourist2  and ranger locations, respectively
#A negative number indicates the death's location of tourist. NA indicates tourist was killed in previous turns
#edges_matrix is a two column matrix indicating the existence of path between two waterholes
#waterholes_distributions$salinity is a matrix giving the salinity's mean and sd at each waterhole
#waterholes_distributions$phosphate is a matrix giving the phosphate's mean and sd at each waterhole
#waterholes_distributions$nitrogen is a matrix giving the nitrogen's mean and sd at each waterhole
makeMoves <- function(move_information, croc_locationReadings, humans_location, edges_matrix, waterholes_distributions) {
  
  #Matrix data structure to store probability information about each of the waterholes
  waterholes <- matrix(0, nrow = 1, ncol = 40, byrow = T)
  
  #Calculate observable waterholes' probability based on the readings of each round.
  observableProbabilities_matrix <- current_waterholesProbabilites(croc_locationReadings, waterholes_distributions)
  most_probable_waterhole <- 1
  
  #At S0, the probability of each waterhole is the product of its salinity, phosphate and nitrogen probabilities given Croc's location readings
  if (move_information$mem$status == 0 | move_information$mem$status == 1) {
    
    #Compute transition matrix when a new game starts and store in memory
    transition_matrix <- compute_transitionMatrix(edges_matrix)
    move_information$mem$transition_matrix <- transition_matrix
    
    #Create a first matrix where all waterholes have the same probability, as no there is previous knowledge about the system
    first_matrix <- matrix((1/40), nrow = 1, ncol = 40, byrow = TRUE)
    
    #Calculate the new probability of each waterhole by forwarding probabilities from the first_matrix according to HMM
    waterholes <- first_matrix %*% move_information$mem$transition_matrix %*% observableProbabilities_matrix
  }
  
  #After the first round, the most probable waterhole is calculated by forwarding probabilities and considering the new observable reading probabilities
  if (move_information$mem$status == -1) {
    waterholes <- move_information$mem$waterholesProbabilities %*% move_information$mem$transition_matrix %*% observableProbabilities_matrix
  }
  
  #Update waterholes probabilities based on the information inferred by the location or death of the two tourists
  for (tourist in 1:2) {
    #If a tourist has died in previous rounds, there is nothing to do about it anymore
    if (is.na(humans_location[tourist]) == TRUE) {
    }
    #If a tourist remains alive after a round, Croc is definitely not at the tourist current waterhole
    else if (humans_location[tourist] > 0) {
      waterholes[1,humans_location[tourist]] <- 0
    }
    #If a tourist dies during a round, Croc is definitely at the tourist's death location
    else if (humans_location[tourist] < 0) {
      for (index in 1:40) {
        waterholes[1,index] <- 0
      }
      waterholes[1,(humans_location[tourist]*(-1))] <- 1
    }
  }
  
  #Based on the probabilities, find the most probable waterhole in which Croc is currently at
  for (index in 1:40) {
    if (waterholes[1,index] > waterholes[1,most_probable_waterhole]) {
      most_probable_waterhole <- index
    }
  }
  
  #cat("Most probable waterhole: ", most_probable_waterhole, "\n")
  
  #Computes the next 2 moves based on current location, destination waterhole and edges connecting waterholes
  move_information$moves <- calculate_shortestPath(humans_location[3], most_probable_waterhole, edges_matrix)
  
  #If one of the moves is to search, the probability of such waterhole can be updated to zero
  #Croc will certainly not be there is the game proceeds to the next round
  if (move_information$moves[2] == 0) {
    waterholes[1,move_information$moves[1]] <- 0
  }

  #Store information in memory for access during the upcoming round
  move_information$mem$waterholesProbabilities <- waterholes
  
  #Update the game status to a number different of 0 and 1 after the first round
  if (move_information$mem$status == 0 | move_information$mem$status == 1) {
    move_information$mem$status <- -1
  }

  return(move_information)
}

#Run one game
runWheresCroc(makeMoves, doPlot = T, showCroc = T, pause = 1, verbose = T, returnMem = F, mem = NA)

#Run test function checking whether the implementation surpass the par performance
testWC(makeMoves, verbose = 1, returnVec = FALSE, n = 500, seed = 21, timeLimit = 300)
