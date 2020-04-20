get_trial_likelihood_C <- function(value_up_boundary, value_down_boundary, d, theta, sigma, timeStep = 10, 
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
    } else if (FixItem[i] == 3){
      mean <- 0
    }else{
      stop('The FixItem variable must contain 3, 1 or -1 values!')
    }
  })
  
  tim <- cumsum( correctedFixTime)
  
  lik <- likelihood::likelihood(media=media, correctedFixTime=correctedFixTime, tim=tim, sum_correctedFixTime=sum(correctedFixTime),
                                stateStep=stateStep, changeMatrix=changeMatrix, prStates=prStates, sigma=sigma, 
                                changeUp=changeUp, changeDown=changeDown)
  
  if( is.nan(lik[1]) ) lik[1] <- 1e-10 
  if( is.nan(lik[2]) ) lik[2] <- 1e-10 
  
  # [10]
  # Compute the likelihood contribution of this trial based on the final choice.
  likeli <- 0
  if (choice == -1){ 
    if (lik[2] > 0){
      likeli <- lik[2]}
  } else if (choice == 1){
    if ( tail(lik[1], n = 1) > 0){
      likeli <- lik[1] }}
  
  return(likeli)
  
}

#system.time(
#get_trial_likelihood(value_up_boundary = 6, value_down_boundary = 5, d = 0.0041, theta = 0.36, sigma = 0.063, 
#                     choice = 1, FixItem =  c(-1, 1, -1, 1), FixTime = c(194, 574, 364,30))
#)
