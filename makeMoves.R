#List all the packages available
library()

#List all the packages currently loaded
search()

#Load DeliveryMan package for this session
library(WheresCroc)

#Function for calculating probabilities for salinity, phosphate and nitrogen of all waterholes given the readings from Croc's current location
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

compute_neighbours <- function(waterhole, edges_matrix) {
  neighbours <- seq(0, 0, lenght.out = 0)
  for (i in 1:dim(edges_matrix)[1]) {
    if (edges_matrix[i,1] == waterhole) {
      neighbours[length(neighbours) + 1] <- edges_matrix[i,2]
    }
    if (edges_matrix[i,2] == waterhole) {
      neighbours[length(neighbours) + 1] <- edges_matrix[i,1]
    }
  }

  return(neighbours[c(2:length(neighbours))])
}

calculate_shortestPath <- function(origin_node, destination_node, edges_matrix) {
  
  node <- list('number' = 0, 'parent' = 0, 'visited' = F)
  graph <- list()
  queue <- c()
  
  for (index in 1:40) {
    node$number <- index
    graph[[index]] <- node
  }
  
  current_node <- graph[[origin_node]]
  
  if (current_node == destination_node) {
    return(current_node$number);
  }
  else {
    
    children <- compute_neighbours(current_node$number, edges_matrix)
    for (index in 1:length(children)) {
      queue[length(queue) + 1] <- children[index]
    }
    while (length(queue) != 0) {
      
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
  
  #Vector data structure to store probability information about each of the waterholes 
  waterholes <- matrix(0, nrow = 1, ncol = 40, byrow = T)
  transition_matrix <- compute_transitionMatrix(edges_matrix)
  most_probable_waterhole <- 1
  
  observationProbabilities_matrix <- current_waterholesProbabilites(croc_locationReadings, waterholes_distributions)
  
  #At S0, the probability of each waterhole is the product of its salinity, phosphate and nitrogen probabilities given Croc's location readings
  if (move_information$mem$status == 0) {
    for (index in 1:40) {
      waterholes[1,index] <- observationProbabilities_matrix[index,index]
    }
  }
  
  #At S1, the most probable waterhole will a neighbour of the most probable waterhole at S0 or itself, depending on the transition matrix
  if (move_information$mem$status != 0) {
    
    waterholes <- move_information$mem$probabilitiesVector * transition_matrix * observationProbabilities_matrix
    
    #Updated the game status to a number different of 0 and 1
    move_information$mem$status <- -1
  }
  
  #Update waterholes probabilities based on the information inferred by the location or death of the two tourists
  for (tourist in 1:2) {
    #If a tourist remains alive after a round, Croc is definitely not at the tourist current waterhole
    if (humans_location[tourist] > 0) {
      waterholes[1,humans_location[tourist]] <- 0
    }
    #If a tourist dies during a round, Croc is definitely at the tourist's death location
    if (humans_location[tourist] < 0) {
      for (index in 1:40) {
        waterholes[1,index] <- 0
      }
      waterholes[1,humans_location[tourist]] <- 1
    }
  }
  
  move_information$mem$probabilitiesVector <- waterholes
  
  
  #Based on the probabilities, find the most probable waterhole in which Croc is currently at
  for (index in 1:40) {
    if (waterholes[1,index] > waterholes[1,most_probable_waterhole]) {
      most_probable_waterhole <- index
    }
  }
  
  cat("Most probable waterhole: ", most_probable_waterhole)
  
  next_waterhole <- calculate_shortestPath(humans_location[3], most_probable_waterhole, edges_matrix)
  move_information$moves[next_waterhole, 0]
  
  return(move_information)
}

runWheresCroc(makeMoves, doPlot = T, showCroc = T, pause = 1, verbose = T, returnMem = F, mem = NA)
