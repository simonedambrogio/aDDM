get_trial_likelihood <- function(value_up_boundary, value_down_boundary, d, theta, sigma, timeStep = 10, 
                                 approxStateStep = 0.1, barrier = 1, choice, FixItem, FixTime) {
  
  correctedFixTime <- FixTime %/% timeStep
  # [2]
  #TRASFORMA I TEMPI DI FISSAZIONE DA MS IN MS/10 E SOMMA TUTTO. TENERE PRESENTE CHE QUESTO LAVORO È PER 
  # UN SINGOLO TRIAL. IN SOSTANZA RESTITUISCE LA LARGHEZZA DELLA TABELLA.
  if ( sum(correctedFixTime) < 1 ) stop('fix_time più piccolo del timeStep')
  numTimeSteps <- sum( correctedFixTime)
  
  # [3]
  # CREA TANTI 1 E -1 QUANTI SONO I TimeSteps (lunghezza tabella) 
  barrierUp <- rep(1, numTimeSteps)
  barrierDown <- rep(-1, numTimeSteps)
  
  # [4]
  # CALCOLO DI stateStep, OSSIA DELL'AMPIEZZA DEI QUADRATINI VERTICALI
  halfNumStateBins <- barrier /approxStateStep
  stateStep <- barrier / (halfNumStateBins + 0.5)
  
  # [5]
  # Crea un'array 1-D di 21 elementi, rappresentatnti l'altezza della tabella
  states <- seq(barrierDown[1] + (stateStep / 2), 
                barrierUp[1] - (stateStep / 2),
                stateStep)
  
  # [6]
  # Crea una matrice di 0, eccetto che in corrispondenza dell'11 riga della prima colonna, dove ci piazza un 1.
  prStates <- matrix(0, length(states), numTimeSteps)
  prStates[which(states == 0), 1] <- 1
  
  # [7] 
  # crea 161 0 per probUpCrossing e 161 0 per probDownCrossing, E RAPPRESENTANO LE PROBABILITÀ
  probUpCrossing <- rep(0, numTimeSteps)
  probDownCrossing <- rep(0, numTimeSteps)
  
  
  
  # [8]
  changeMatrix <- sapply(seq_along(states), function(i) states[i] - states )
  
  changeUp <- sapply(seq_along(states), function(i) barrierUp - states[i] )
  
  changeDown <- sapply(seq_along(states), function(i) barrierDown - states[i] )
  
  # [9]
  media <- sapply(seq_along(FixItem), function(i){
    if (FixItem[i] == 1){#1 = sguardo verso up_boundary
      mean <- d * (value_up_boundary - (theta * value_down_boundary))
    } else if (FixItem[i] == -1){ #-1 = sguardo verso down_boundary
      mean <- d * (theta*value_up_boundary - value_down_boundary)
    } else if (FixItem[i]==3){
      mean <- 0
    } else {
      stop('The FixItem variable must contain 1 or -1 values!')
    }
  })
  
  tim <- cumsum( correctedFixTime)
  i <- 1
  
for (time in 2:sum(correctedFixTime)) {
    
    if ( time == (tim[i]+1) ) {
      i <- i+1
      mean <- media[i] } else if (time == 2){
        mean <- media[1]
      }
    
    # Update the probability of the states that remain inside the barriers.
    prStatesNew <-  stateStep * apply(dnorm(changeMatrix, mean, sigma) * prStates[,time -1],
                                      2, sum)
    
    # Queta funzione non ho ben capito cosa faccia
    prStatesNew[(states >= barrierUp[time]) |
                  (states <= barrierDown[time])] = 0
    
    # Calculate the probabilities of crossing the up barrier and the down barrier.
    tempUpCross <- sum(
      prStates[, time - 1] * # Pi
        (1-pnorm(changeUp[time, ], mean, sigma)))
    
    tempDownCross <- sum(
      prStates[, time - 1] *
        (pnorm(changeDown[time, ], mean, sigma)))
    
    # Renormalize to cope with numerical approximations.
    sumIn <- sum(prStates[, time-1])
    sumCurrent <- sum(prStatesNew) + tempUpCross + tempDownCross
    if (sumCurrent != 0){
      prStatesNew <- (prStatesNew * sumIn) / sumCurrent
      tempUpCross <- (tempUpCross * sumIn) / sumCurrent
      tempDownCross <- (tempDownCross * sumIn) / sumCurrent
    } else {
      prStatesNew <- (prStatesNew * 0)
      tempUpCross <- (tempUpCross * sumIn)
      tempDownCross <- (tempDownCross * sumIn)
      print('Error: loglikelihood = 0!!!')
    }
    
    # Update the probabilities of each state and the probabilities
    # of crossing each barrier at this timestep.
    prStates[, time] <- prStatesNew
    probUpCrossing[time] <- tempUpCross
    probDownCrossing[time] <- tempDownCross
  }
  
  # [10]
  # Compute the likelihood contribution of this trial based on the final choice.
  likelihood <- 0
  if (choice == -1){ 
    if (tail(probDownCrossing, n = 1) > 0){
      likelihood <- tail(probDownCrossing, n = 1)}
  } else if (choice == 1){
    if ( tail(probUpCrossing, n = 1) > 0){
      likelihood <- tail(probUpCrossing, n = 1) }}
  
  return(likelihood)
  
}

#system.time(
#get_trial_likelihood(value_up_boundary = 6, value_down_boundary = 5, d = 0.0041, theta = 0.36, sigma = 0.063, 
#                     choice = 1, FixItem =  c(-1, 1, -1, 1), FixTime = c(194, 574, 364,30))
#)


