####################################### GSA_mle_la #######################################
GSA_laDDM <- function(data, d_set, sigma_set, lambda_set, timeStep = 10, barrier = 1){
  
  doParallel::registerDoParallel(8)
  improvement <- 1
  iteration <- 1
  
  while (improvement > 0.01){
    
    print(paste("Performing step", iteration))
    
    #Set o parameters to test
    par_set <- expand.grid(d=d_set, sigma=sigma_set, lambda=lambda_set)
    #Compute the NLL for each set of parameters
    grid_results <- sapply( 1:nrow(par_set), function(row_i){
      aDDM::mle_la(data=data, d=par_set[row_i, 'd'], sigma=par_set[row_i, 'sigma'], lambda=par_set[row_i, 'lambda'],
                   timeStep = timeStep, barrier = barrier)
    })
    #Best set of parameters
    Best_par_set <- par_set[ which(grid_results==min(grid_results)), ]
    
    Best_NLL_t1 <- min(grid_results)
    
    if(iteration==1) improvement <- 1 else  improvement <- Best_NLL_t0/Best_NLL_t1-1 
    
    iteration <- iteration+1
    Best_NLL_t0 <- min(grid_results)
    
    #Set delta
    delta_d <- diff(d_set) %>% min()
    delta_sigma <- diff(sigma_set) %>% min()
    delta_lambda <- diff(lambda_set) %>% min()
    
    #Set new set of parameters
    d_set <- c(Best_par_set$d - (delta_d/2), Best_par_set$d, Best_par_set$d + (delta_d/2))
    sigma_set <- c(Best_par_set$sigma - (delta_sigma/2), Best_par_set$sigma, Best_par_set$sigma + (delta_sigma/2))
    lambda_set <- c(Best_par_set$lambda - (delta_lambda/2), Best_par_set$lambda, Best_par_set$lambda + (delta_lambda/2))
  }
  
  return(Best_par_set)
  
}