####################################### GSA_l2a #######################################
GSA_l4aDDM <- function(data, d_set, sigma_set, theta_gain_set, theta_loss_set, timeStep = 10, barrier = 1, numCores){
  
  #load package
  library(foreach)
  library(doParallel)
  library(Rcpp)
  
  #mle_ala function
  mle_ala <- function (data, d, sigma, theta_gain, theta_loss, timeStep = 10, barrier = 1){
    
    get_trial_likelihood_C <- function(value_up_boundary, value_down_boundary, 
                                       d, theta_gain, theta_loss, sigma, timeStep, approxStateStep = 0.1, 
                                       barrier, choice, FixItem, FixTime) {
      
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
      prStates[which(states == 0), 1] <- 1
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
          mean <- d * (value_up_boundary - (theta_loss * value_down_boundary))
        }
        else if (FixItem[i] == -1) {
          mean <- d * ( (theta_gain * value_up_boundary) - value_down_boundary)
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
      
      #Questo comando è già implementato nella funzione su C++
      # if( is.nan(lik[1]) ) lik[1] <- 1e-10 
      # if( is.nan(lik[2]) ) lik[2] <- 1e-10 
      
      # if (is.nan(lik[1])) 
      #   lik[1] <- 0
      # if (is.nan(lik[2])) 
      #   lik[2] <- 0
      likeli <- 0
      if (choice == -1) {
        likeli <- lik[2]
      }
      else if (choice == 1) {
        likeli <- lik[1]
      }
      return(likeli)
    }
    
    likelihood <- unlist(foreach(trial_i = unique(data$trial)) %dopar% get_trial_likelihood_C(value_up_boundary = unique(data[data$trial == trial_i, "value_up_boundary"]), 
                                                                                              value_down_boundary = unique(data[data$trial == trial_i, "value_down_boundary"]), 
                                                                                              d = d, sigma = sigma, theta_gain = theta_gain, theta_loss = theta_loss,
                                                                                              choice = unique(data[data$trial == trial_i, "choice"]), 
                                                                                              FixItem = data[data$trial == trial_i, "fix_item"], 
                                                                                              FixTime = data[data$trial == trial_i, "fix_time"], 
                                                                                              timeStep = timeStep, barrier = barrier))
    
    nll <- -sum(log(likelihood[likelihood != 0]))
    print(paste0("Calcolo NLL modello: d = ", d, " sigma = ", sigma, " theta_gain = ", theta_gain, " theta_loss = ", theta_loss, " --- NLL = ", round(nll) ))
    return(nll)
  }
  
  doParallel::registerDoParallel(numCores)
  improvement <- 1
  iteration <- 1
  
  while (improvement >= 0.01){
    
    print(paste("Performing step", iteration))
    
    #Set o parameters to test
    par_set <- expand.grid(d=d_set, sigma=sigma_set, theta_gain=theta_gain_set, theta_loss=theta_loss_set)
    #Compute the NLL for each set of parameters
    grid_results <- sapply( 1:nrow(par_set), function(row_i){
      mle_ala(data=data, d=par_set[row_i, 'd'], sigma=par_set[row_i, 'sigma'], 
              theta_gain=par_set[row_i, 'theta_gain'], theta_loss = par_set[row_i, 'theta_loss'], 
              timeStep = timeStep, barrier = barrier)
    })
    #Best set of parameters
    Best_par_set <- par_set[ which(grid_results==min(grid_results)), ]
    
    Best_NLL_t1 <- min(grid_results)
    
    if(iteration==1) improvement <- 1 else  improvement <- Best_NLL_t0/Best_NLL_t1-1 
    
    iteration <- iteration+1
    Best_NLL_t0 <- min(grid_results)
    
    #Set delta
    delta_d <- if( length(unique(d_set)) != 1) diff(d_set) %>% min() 
    delta_sigma <- if( length(unique(sigma_set)) != 1) diff(sigma_set) %>% min() 
    delta_theta_gain <- if( length(unique(theta_gain_set)) != 1) diff(theta_gain_set) %>% min()
    delta_theta_loss <- if( length(unique(theta_loss_set)) != 1) diff(theta_loss_set) %>% min()
    
    #Set new set of parameters
    
    #new d_set
    if( length(unique(d_set)) != 1){
      d_set <- c(Best_par_set$d - (delta_d/2), Best_par_set$d, Best_par_set$d + (delta_d/2))}
    
    #new sigma_set
    if( length(unique(sigma_set)) != 1){
      sigma_set <- c(Best_par_set$sigma - (delta_sigma/2), Best_par_set$sigma, Best_par_set$sigma + (delta_sigma/2))}
    
    #new theta_gain_set
    if( length(unique(theta_gain_set)) != 1){
      theta_gain_set <- c(Best_par_set$theta_gain - (delta_theta_gain/2), Best_par_set$theta_gain, Best_par_set$theta_gain + (delta_theta_gain/2))}
    
    #new theta_loss_set
    if( length(unique(theta_loss_set)) != 1){
      theta_loss_set <- c(Best_par_set$theta_loss - (delta_theta_loss/2), Best_par_set$theta_loss, Best_par_set$theta_loss + (delta_theta_loss/2))}
    
  }
  return( cbind(Best_par_set, NLL = min(grid_results)) )
}


 
# library(tidyverse)
# 
# data_aDDM <- read.csv('/home/simone/Dropbox/Ongoing Projects/Analysis_Platt_Lab/Feng - Aging and Loss Aversion/Data/aDDM_data.csv')
# 
# fix_type <- aDDM::fixations_type( fixations = data_aDDM )
# 
# #Simulate 5 trials for each combination of 1:20
# 
# sim_data <- rl4aDDM(n = 5, sigma = 0.0233, V = 0, d = 0.00065, theta_gain = 0.5, theta_loss = 0.8,
#                     value_up_boundary = seq(1,20,4), value_down_boundary = seq(1,20,4),
#                     first_fixation_time_up = fix_type$first_fixation_time_up,
#                     first_fixation_time_down = fix_type$first_fixation_time_down,
#                     fixation_time_up = fix_type$fixation_time_up,
#                     fixation_time_down = fix_type$fixation_time_down,
#                     first_fix_type = fix_type$first_fix_type,
#                     trans_time = fix_type$trans_time[fix_type$trans_time<100],
#                     non_dec_time = fix_type$non_dec_time,
#                     num_up_boundary = 1, transition_time = FALSE) %>%
#   mutate(subject=0, gain_loss = apply(.[, c('value_down_boundary', 'value_up_boundary')], 1, diff))
# 
# GSA_l4aDDM(data = sim_data, 
#            d_set = c(0.0001, 0.0005, 0.001), 
#            sigma_set = c(0.01, 0.05, 0.1), 
#            theta_gain_set = c(0.1, 0.5, 0.9),
#            theta_loss_set = c(0.1, 0.5, 0.9), 
#            timeStep = 1, numCores = 8)
