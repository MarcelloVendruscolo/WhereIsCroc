#List all the packages available
library()

#List all the packages currently loaded
search()

#Load DeliveryMan package for this session
library(WheresCroc)

#Function for calculating normalised probabilities for salinity, phosphate and nitrogen of all waterholes given the readings from Croc's current location
current_waterholesProbabilites <- function(croc_locationReadings, waterholes_distributions) {
  
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
  
  normalisedProbabilities <- list("salinity_normalised" = salinity_normalisedProbabilities, "phosphate_normalised" = phosphate_normalisedProbabilities, "nitrogen_normalised" = nitrogen_normalisedProbabilities)
  return(normalisedProbabilities)
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
  waterholes <- seq(0, 0, length.out = 40)
  
  normalisedProbabilities <- current_waterholesProbabilites(croc_locationReadings, waterholes_distributions)
  
  #At S0, the probability of each waterhole is the product of its salinity, phosphate and nitrogen probabilities given Croc's location readings
  if (move_information$mem$status == 0) {
    for (i in 1:40) {
      waterholes[i] <- (normalisedProbabilities$salinity_normalised[i] * normalisedProbabilities$phosphate_normalised[i] * normalisedProbabilities$nitrogen_normalised[i])
    }
  }
  
  #Update waterholes probabilities based on the information inferred by the location or death of the two tourists
  for (tourist in 1:2) {
    #If a tourist remains alive after a round, Croc is definitely not at the tourist current waterhole
    if (humans_location[tourist] > 0) {
      waterholes[humans_location[tourist]] <- 0
    }
    #If a tourist dies during a round, Croc is definitely at the tourist's death location
    if (humans_location[tourist] < 0) {
      for (i in 1:40) {
        waterholes[i] <- 0
      }
      waterholes[humans_location[tourist]] <- 1
    }
  }
  
  return(move_information)
}

runWheresCroc(makeMoves, doPlot = T, showCroc = T, pause = 1, verbose = T, returnMem = F, mem = NA)
