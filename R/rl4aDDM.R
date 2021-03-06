# update 22/01/2020
#In questa funzione, la variabile first_fix_type dev'essere 1 o -1.
rl4aDDM <- function(n, sigma, theta_gain, theta_loss, d, fixation_time_up, fixation_time_down, 
                    trans_time, first_fixation_time_up, first_fixation_time_down,
                    non_dec_time, first_fix_type, num_up_boundary, V = 0, value_up_boundary, value_down_boundary, 
                    transition_time = TRUE) {
  
  library(parallel)
  library(dplyr)
  library(pbmcapply)
  library(pbapply)
  
  # Combinazioni valori soggettivi e prezzi
  comb.up_down <- function( value_up_boundary, value_down_boundary) {
    
    up_down <- expand.grid(value_up_boundary, value_down_boundary)
    up_down <- up_down %>% 
      mutate(Var3 = abs(up_down$Var1 - up_down$Var2)) %>% 
      filter( Var3 <= 20)
    
    return(up_down)
    
  }
  up_down <- comb.up_down( value_up_boundary = value_up_boundary, value_down_boundary = value_down_boundary)
  
  n_up_down <- nrow(up_down)
  N <- n * n_up_down
  
  # n Simulazioni per ogni coppia di valore e prezzo il cui valore relativo netto sia minore di 20
  # 24/08/2019
  # Con questo aggiornamento, è possibile scegliere se simulare dei dati sia tenendo conto dei tempi di transizione,
  # sia non tenendone conto. Di default non tiene conto dei tempi di transizione
  raDDM_one <- function( n, up, down, sigma, theta_gain, theta_loss, lambda, d, V = 0, first_fixation_time_up, first_fixation_time_down, 
                         fixation_time_up, fixation_time_down, trans_time, non_dec_time, first_fix_type,
                         num_up_boundary, transition_time = transition_time) {
    
    raDDM1 <- function(n) {
      
      n <- 1:n
      
      raddm <- function(trial){
        
        if(transition_time){
          
          att <- 1
          iterations <- 0
          first_fix <- sample(first_fix_type, 1)
          
          if(first_fix==1){
            att_up <- 2
            att_down <- 4
          } else {
            att_up <- 4
            att_down <- 2
          }
          
          while (V >= -1 && V <= 1) {
            ######################## First gaze ########################
            if(att==1){
              
              v <- 0
              fixation_time <- sample(non_dec_time, 1)
              i_3 <- 1
              
              while(V <= 1 && V >= -1 && i_3 < fixation_time){
                V <- V + v + rnorm(1, 0, sigma)
                i_3 <- i_3+1}
              
              if(V >= 1 | V <= -1 | i_3 == fixation_time){ 
                att <- att+1
                iterations <- iterations+fixation_time
                
                fix_time <- fixation_time
                fix_item <- 3
                
              }} else if (att%%2==0 & att==att_up) {#If the gaze is to the item
                ######################## Second gaze ######################## 
                
                v <- d*(up - theta_loss*down )
                if(att_up==2){ time_up <- sample(first_fixation_time_up, 1) 
                } else {
                  time_up <- sample(fixation_time_up, 1)
                }
                
                i_up <- 1
                
                while(V <= 1 && V >= -1 && i_up < time_up){
                  V <- V + v + rnorm(1, 0, sigma)
                  i_up <- i_up+1}
                
                if(V >= 1 | V <= -1 | i_up == time_up){ 
                  att <- att+1
                  att_up <- att_up+4
                  iterations <- iterations+time_up
                  
                  fix_time <- c(fix_time, time_up)
                  fix_item <- c(fix_item, 1)
                  
                }
                
              } else if(att%%2==0 & att==att_down) {
                
                v <- d* ( theta_gain*up - down )
                
                if(att_down==2){ time_down <- sample(first_fixation_time_down, 1) 
                } else {
                  time_down <- sample(fixation_time_down, 1)
                }
                
                i_down <- 1
                
                while(V <= 1 && V >= -1 && i_down < time_down){
                  V <- V + v + rnorm(1, 0, sigma)
                  i_down <- i_down+1}
                
                if(V >= 1 | V <= -1 | i_down == time_down){ 
                  att <- att+1
                  att_down <- att_down+4
                  iterations <- iterations+time_down
                  
                  fix_time <- c(fix_time, time_down)
                  fix_item <- c(fix_item, -1)
                  
                }
                
              } else if (att%%2==1) {
                ######################## Third and all the other Odd gaze ########################
                v <- d* ( up - down )
                fixation_time <- sample(trans_time, 1)
                i_0 <- 1
                
                while(V <= 1 && V >= -1 && i_0 < fixation_time){
                  V <- V + v + rnorm(1, 0, sigma)
                  i_0 <- i_0+1}
                
                if(V >= 1 | V <= -1 | i_0 == fixation_time){ 
                  att <- att+1
                  iterations <- iterations+fixation_time
                  
                  fix_time <- c(fix_time, fixation_time)
                  fix_item <- c(fix_item, 0)
                  
                }}
          }
          
        } else {
          
          iterations <- 0
          first_fix <- sample(first_fix_type, 1)
          fix_item <- c()
          fix_time <- c()
          if(first_fix==1){
            att <- 1  #up
          } else {
            att <- 0  #down
          }
          
          while (V >= -1 && V <= 1) {
            ######################## First gaze ########################
            if(att%%2==1){ 
              
              v <- d*(up - (theta_loss *down) )
              
              if(att==1){ time_up <- sample(first_fixation_time_up, 1) 
              } else {
                time_up <- sample(fixation_time_up, 1)
              }
              
              i_up <- 1
              
              while(V <= 1 && V >= -1 && i_up < time_up){
                V <- V + v + rnorm(1, 0, sigma)
                i_up <- i_up+1}
              
              if(V >= 1 | V <= -1 | i_up == time_up){ 
                att <- att+1
                iterations <- iterations+time_up
                
                fix_time <- c(fix_time, time_up)
                fix_item <- c(fix_item, 1)
                
              }} else if (att%%2==0) {#If the gaze is to the item
                ######################## Second gaze ######################## 
                
                v <- d* ( (theta_gain*up) - down) 
                
                if(att==0){ time_down <- sample(first_fixation_time_down, 1) 
                } else {
                  time_down <- sample(fixation_time_down, 1)
                }
                
                i_down <- 1
                
                while(V <= 1 && V >= -1 && i_down < time_down){
                  V <- V + v + rnorm(1, 0, sigma)
                  i_down <- i_down+1}
                
                if(V >= 1 | V <= -1 | i_down == time_down){ 
                  att <- att+1
                  iterations <- iterations+time_down
                  
                  fix_time <- c(fix_time, time_down)
                  fix_item <- c(fix_item, -1)
                  
                }
                
              }}
          
        } 
        if ( V >= 1 ) {
          choice <-  1
        } else if (V <= -1){ 
          choice <- -1
        } else { choice = V}
        
        return(cbind(rt = iterations, choice, fix_item, fix_time))
        
      }
      
      results <- mclapply(n, raddm, mc.cores = detectCores() )
      
      results <- lapply(n, function(i){
        cbind(results[[i]], trial = rep(i, nrow(results[[i]])))
      })
      
      results <- results %>%
        do.call(rbind.data.frame, .)
      
      return(results)
    }
    
    results <- raDDM1(n = n)
    return(results)
  }
  
  create.rt <- function(n,  sigma, d, theta_gain, theta_loss, V = 0, 
                        first_fix_time, non_first_fix_time, trans_time, non_dec_time ) {
    
    rt <- pblapply(1:n_up_down, function(i){
      
      rt <- raDDM_one( n = n, up = up_down[i, 1], down = up_down[i, 2], sigma = sigma, theta_gain = theta_gain, theta_loss = theta_loss,
                       d = d, V = 0, 
                       first_fixation_time_up = first_fixation_time_up, first_fixation_time_down = first_fixation_time_down,
                       fixation_time_up = fixation_time_up, fixation_time_down = fixation_time_down, 
                       trans_time = trans_time, non_dec_time = non_dec_time, transition_time = transition_time,
                       first_fix_type = first_fix_type, num_up_boundary = num_up_boundary)
      
      #print(paste0('Value: ', up_down[i, 1],'  |  Price: ', up_down[i, 2], '  ------  ', round(i / n_up_down * 100,1), '%'))
      return(rt)
    })
    
    rt <- lapply(1:n_up_down, function(i){
      rt[[i]]$trial <- rt[[i]]$trial + (n * (i-1))
      cbind(rt[[i]], value_up_boundary = up_down[i, 1], value_down_boundary = up_down[i, 2], trial = rt[[i]]$trial)
    }) %>% do.call(rbind.data.frame, .)
    
    rt <- rt[, -ncol(rt)]
    return(rt)
  }
  
  rt <- create.rt(n = n,  sigma = sigma, theta_gain = theta_gain, theta_loss = theta_loss, d = d, V = 0, 
                  first_fix_time = first_fix_time, non_first_fix_time = non_first_fix_time, 
                  trans_time = trans_time, non_dec_time = non_dec_time)
  
  return( rt )
  
}

#library(tidyverse)
#source('https://raw.githubusercontent.com/simonedambrogio/aDDM/master/fixations_type.R?token=ANVVDWW64XEGCZ3QQNFZAZ26HVZ7O')
#source("https://raw.githubusercontent.com/simonedambrogio/Useful_R_Functions/master/df_probability.R?token=ANVVDWUPA2WSWHSNQZPO2V26HV2DC")
#data_aDDM <- read.csv('/home/simone/Dropbox/Ongoing Projects/Analysis_Platt_Lab/Feng - Aging and Loss Aversion/Data/aDDM_data.csv')

#fix_type <- aDDM::fixations_type( fixations = data_aDDM )

#Simulate 5 trials for each combination of 1:20

# sim_data <- rl4aDDM(n = 5, sigma = 0.03, V = 0, d = 0.0006, theta_gain = 0.5, theta_loss = 0.8,
#                   value_up_boundary = seq(1,20,2), value_down_boundary = seq(1,20,2),
#                   first_fixation_time_up = fix_type$first_fixation_time_up,
#                   first_fixation_time_down = fix_type$first_fixation_time_down,
#                   fixation_time_up = fix_type$fixation_time_up,
#                   fixation_time_down = fix_type$fixation_time_down,
#                   first_fix_type = fix_type$first_fix_type,
#                   trans_time = fix_type$trans_time[fix_type$trans_time<100],
#                   non_dec_time = fix_type$non_dec_time,
#                   num_up_boundary = 1) %>%
#  mutate(subject=0, gain_loss = apply(.[, c('value_down_boundary', 'value_up_boundary')], 1, diff))

#df_probability(data = sim_data, optA_optB = "gain_loss",  step = 1) %>% 
#  ggplot(aes(optA_optB, probability) )+
#  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
#  geom_point( size = 3, shape=21, fill="white" ) +
#  labs( y = "P(Accept)", x = "Gain - Loss") +
#  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) +
#  scale_x_continuous(limits = c(-20, 20)) +
#  scale_color_brewer(palette = "Dark2") +
#  geom_vline(xintercept = 0, linetype = 3) +
#  geom_hline(yintercept = .5, linetype = 3)
