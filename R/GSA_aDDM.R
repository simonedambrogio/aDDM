####################################### GSA_mle #######################################
#Gried-Search Algorithm Function
GSA_aDDM <- function(data, d_set, sigma_set, theta_set, timeStep = 10, barrier = 1){
  
  doParallel::registerDoParallel(8)
  improvement <- 1
  iteration <- 1
  
  while (improvement > 0.01){
    
    print(paste("Performing step", iteration))
    
    #Set o parameters to test
    par_set <- expand.grid(d=d_set, sigma=sigma_set, theta=theta_set)
    #Compute the NLL for each set of parameters
    grid_results <- sapply( 1:nrow(par_set), function(row_i){
      aDDM::mle(data=data, d=par_set[row_i, 'd'], sigma=par_set[row_i, 'sigma'], theta=par_set[row_i, 'theta'],
                timeStep=timeStep, barrier = barrier)
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
    delta_theta <- diff(theta_set) %>% min()
    
    #Set new set of parameters
    d_set <- c(Best_par_set$d - (delta_d/2), Best_par_set$d, Best_par_set$d + (delta_d/2))
    sigma_set <- c(Best_par_set$sigma - (delta_sigma/2), Best_par_set$sigma, Best_par_set$sigma + (delta_sigma/2))
    theta_set <- c(Best_par_set$theta - (delta_theta/2), Best_par_set$theta, Best_par_set$theta + (delta_theta/2))
  }
  
  return(Best_par_set)
  
}

#Load data
#data <- read.csv('Data/aDDM_data.csv')
#fix_type <- fixations_type( fixations = data )

#Vengono Prodotti 400 trial
#fix_sim <- raDDM(n = 5, sigma = 0.08, theta = 0.7, V = 0, d = 0.00065, 
#                 value_up_boundary = seq(1,20,2), value_down_boundary = seq(1,20,2), 
#                 first_fixation_time_up = fix_type$first_fixation_time_up%/%10, 
#                 first_fixation_time_down = fix_type$first_fixation_time_down%/%10, 
#                 fixation_time_up = fix_type$fixation_time_up%/%10, 
#                 fixation_time_down = fix_type$fixation_time_down%/%10, 
#                 first_fix_type = fix_type$first_fix_type, 
#                 trans_time = fix_type$trans_time[fix_type$trans_time<100]%/%10,
#                 non_dec_time = fix_type$non_dec_time%/%10,
#                 num_up_boundary = 1) %>%
#  mutate(subject = 0, rt = rt*10, fix_time = fix_time*10)

#GSA_mle(data = fix_sim, d_set = c(0.001, 0.005, 0.01), sigma_set = c(0.01, 0.05, 0.1), theta_set = c(0.1, 0.5, 0.9))