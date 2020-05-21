####################################### GSA_mle #######################################
#Grid-Search Algorithm Function
GSA_aDDM <- function (data, d_set, sigma_set, theta_set, z_set, timeStep = 10, barrier = 1, numCores) {
  
  mle <- function(data, d, sigma, theta, z, timeStep, barrier) {
    library(foreach)
    library(doParallel)
    library(Rcpp)
    get_trial_likelihood_C <- function(value_up_boundary, value_down_boundary, 
                                       d, theta, sigma, z, timeStep, 
                                       approxStateStep = 0.1, barrier, choice, FixItem, FixTime) {
      correctedFixTime <- FixTime%/%timeStep
      if (sum(correctedFixTime) < 1) 
        stop("fix_time più piccolo del timeStep")
      numTimeSteps <- sum(correctedFixTime)
      barrierUp <- rep(1, numTimeSteps)
      barrierDown <- rep(-1, numTimeSteps)
      halfNumStateBins <- barrier/approxStateStep
      stateStep <- barrier/(halfNumStateBins + 0.5)
      states <- seq(barrierDown[1] + (stateStep/2), barrierUp[1] - 
                      (stateStep/2), stateStep)
      prStates <- matrix(0, length(states), numTimeSteps)
      
      prStates[(which(states == 0)+z*10), 1] <- 1
      
      probUpCrossing <- rep(0, numTimeSteps)
      probDownCrossing <- rep(0, numTimeSteps)
      changeMatrix <- sapply(seq_along(states), function(i) states[i] - 
                               states)
      changeUp <- sapply(seq_along(states), function(i) barrierUp - 
                           states[i])
      changeDown <- sapply(seq_along(states), function(i) barrierDown - 
                             states[i])
      media <- sapply(seq_along(FixItem), function(i) {
        if (FixItem[i] == 1) {
          mean <- d * (value_up_boundary - (theta * value_down_boundary))
        }
        else if (FixItem[i] == -1) {
          mean <- d * (theta * value_up_boundary - value_down_boundary)
        }
        else if (FixItem[i] == 3) {
          mean <- 0
        }
        else if (FixItem[i] == 0) {
          mean <- d * (value_up_boundary - value_down_boundary)
        }
        else {
          stop("The FixItem variable must contain 3, 0, 1 or -1 values!")
        }
        
      })
      tim <- cumsum(correctedFixTime)
      lik <- likelihood::likelihood_C(media = media, correctedFixTime = correctedFixTime, 
                                      tim = tim, sum_correctedFixTime = sum(correctedFixTime), 
                                      stateStep = stateStep, changeMatrix = changeMatrix, 
                                      prStates = prStates, sigma = sigma, changeUp = changeUp, 
                                      changeDown = changeDown)
      
      if( is.nan(lik[1]) ) lik[1] <- 1e-10 
      if( is.nan(lik[2]) ) lik[2] <- 1e-10 
      
      if (is.nan(lik[1])) 
        lik[1] <- 0
      if (is.nan(lik[2])) 
        lik[2] <- 0
      likeli <- 0
      if (choice == -1) {
        if (lik[2] > 0) {
          likeli <- lik[2]
        }
      }
      else if (choice == 1) {
        if (tail(lik[1], n = 1) > 0) {
          likeli <- lik[1]
        }
      }
      return(likeli)
    }
    likelihood <- unlist(foreach(trial_i = unique(data$trial)) %dopar% 
                           get_trial_likelihood_C(value_up_boundary = unique(data[data$trial == trial_i, "value_up_boundary"]), 
                                                  value_down_boundary = unique(data[data$trial == trial_i, "value_down_boundary"]), 
                                                  d = d, theta = theta, sigma = sigma, z = z, choice = unique(data[data$trial == trial_i, "choice"]), 
                                                  FixItem = data[data$trial == trial_i, "fix_item"], FixTime = data[data$trial == trial_i, "fix_time"], 
                                                  timeStep = timeStep, barrier = barrier))
    nll <- -sum(log(likelihood[likelihood != 0]))
    print(paste0("Calcolo NLL modello: d = ", d, "  sigma = ", sigma, "  theta = ", theta, "  z = ", z, "       --- NLL = ", round(nll, 2) ))
    return(nll)
  }
  
  doParallel::registerDoParallel(numCores)
  improvement <- 1
  iteration <- 1
  
  while (improvement >= 0.01) {
    print(paste("Performing step", iteration))
    par_set <- expand.grid(d = d_set, sigma = sigma_set, 
                           theta = theta_set, z = z_set)
    grid_results <- sapply(1:nrow(par_set), function(row_i) {
      mle(data = data, d = par_set[row_i, "d"], sigma = par_set[row_i, "sigma"], theta = par_set[row_i, "theta"], z = par_set[row_i, "z"], timeStep = timeStep, barrier = barrier)
    })
    
    Best_par_set <- par_set[which(grid_results == min(grid_results)), ]
    
    Best_NLL_t1 <- min(grid_results)
    if (iteration == 1) 
      improvement <- 1
    else improvement <- Best_NLL_t0/Best_NLL_t1 - 1
    iteration <- iteration + 1
    Best_NLL_t0 <- Best_NLL_t1
    
    delta_d <- if( length(unique(d_set)) != 1) diff(d_set) %>% min()
    delta_sigma <- if( length(unique(sigma_set)) != 1) diff(sigma_set) %>% min()
    delta_theta <- if( length(unique(theta_set)) != 1) diff(theta_set) %>% min()
    delta_z <- if( length(unique(z_set)) != 1) diff(z_set) %>% min()
    
    #new d_set
    if( length(unique(d_set)) != 1){
      d_set <- c(Best_par_set$d - (delta_d/2), Best_par_set$d, Best_par_set$d + (delta_d/2))}
    
    #new sigma_set
    if( length(unique(sigma_set)) != 1){
      sigma_set <- c(Best_par_set$sigma - (delta_sigma/2), Best_par_set$sigma, Best_par_set$sigma + (delta_sigma/2))}
    
    #new theta_set
    if( length(unique(theta_set)) != 1){
      theta_set <- c(Best_par_set$theta - (delta_theta/2), Best_par_set$theta, Best_par_set$theta + (delta_theta/2))}
    
    #new z_set
    if( length(unique(z_set)) != 1){
      z_set <- c(Best_par_set$z - (delta_z/2), Best_par_set$z, Best_par_set$z + (delta_z/2))}
  }
  
  return(cbind(Best_par_set, NLL=min(grid_results)))
}