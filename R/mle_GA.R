#Maximum Likelihood Estimation via Genetic Algorithm
mle_GA <- function(data, popSize, generations, num_cores, lower = c(0.0001, 0.01, 0), upper = c(0.01, 0.1, 1)){
  
  library(GA)
  cl <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)
  # Il dataset deve contenere due colonne value_up_boundary e value_down_boundary!!!
  maximum_likelihood_genetic_algorithm_C <- function(data, d, sigma, theta){

    library(foreach)
    library(doParallel)
    library(Rcpp)
    
    #get_trial_likelihood_C
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
        } else if( FixItem[i] == 0 ) {
          mean <- d * (value_up_boundary - value_down_boundary)
        } else {
          stop('The FixItem variable must contain 3, 0, 1 or -1 values!')
        }
      })
      
      tim <- cumsum( correctedFixTime)
      
      lik <- likelihood::likelihood(media=media, correctedFixTime=correctedFixTime, tim=tim, sum_correctedFixTime=sum(correctedFixTime),
                                    stateStep=stateStep, changeMatrix=changeMatrix, prStates=prStates, sigma=sigma, 
                                    changeUp=changeUp, changeDown=changeDown)
      if( is.nan(lik[1]) ) lik[1] <- 0 
      if( is.nan(lik[2]) ) lik[2] <- 0
      
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
    
    likelihood <-  unlist( foreach(trial_i=unique(data$trial)) %dopar% get_trial_likelihood_C(value_up_boundary = unique(data[data$trial == trial_i, 'value_up_boundary' ]), 
                                                                                              value_down_boundary = unique(data[data$trial == trial_i, 'value_down_boundary' ]),
                                                                                              d = d, theta = theta, sigma = sigma, 
                                                                                              choice = unique(data[data$trial == trial_i, 'choice' ]),
                                                                                              FixItem = data[data$trial == trial_i, 'fix_item'],
                                                                                              FixTime = data[data$trial == trial_i, 'fix_time']))
    
    #Calcolo del NegativeLogLokelihood
    nll <- -sum(log(likelihood[likelihood != 0]))
    
    #Save each model
    fitness <- data.frame( d=d, sigma=sigma, theta=theta, NLL = nll )
    results <<- rbind(results, fitness)
    
    print(paste0('Calcolo NLL modello: d = ', round(d, 5),' sigma = ', round(sigma, 3), ' theta = ', round(theta, 2),' --- NLL = ', round(nll, 2) ))
    return(nll)
  }

  results <- data.frame()

  GA <- ga(type = "real-valued", 
           fitness = function(x) - maximum_likelihood_genetic_algorithm_C(data = data, x[1], x[2], x[3]), 
           lower = lower, upper = upper, popSize = popSize, maxiter = generations, 
           names = c('d', 'sigma', 'theta'))
  
  return(list(GA = GA, Results = results))
}


#library(tidyverse)
#data <- read.csv("aDDM_data.csv") %>% filter(subject==4)
#system.time(
#mle_GA(data=data, num_cores = 8, popSize = 50, generations = 1)
#)
