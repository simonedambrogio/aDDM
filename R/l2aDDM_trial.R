l2aDDM_trial <- function( up, down, sigma, theta, lambda, d, V = 0, fixations_type_list, 
                        transition_time = TRUE, plot_trial = TRUE, color = c("maroon4", "mediumaquamarine") ){
  
  first_fixation_time_up <- fixations_type_list$first_fixation_time_up[fixations_type_list$first_fixation_time_up>9]
  first_fixation_time_down <- fixations_type_list$first_fixation_time_down[fixations_type_list$first_fixation_time_down>9]
  fixation_time_up <- fixations_type_list$fixation_time_up[fixations_type_list$fixation_time_up>9&fixations_type_list$fixation_time_up<1000]
  fixation_time_down <- fixations_type_list$fixation_time_down[fixations_type_list$fixation_time_down>9&fixations_type_list$fixation_time_down<1000]
  trans_time <- fixations_type_list$trans_time[fixations_type_list$trans_time<200]
  non_dec_time <- fixations_type_list$non_dec_time[fixations_type_list$non_dec_time < 500]
  first_fix_type <- fixations_type_list$first_fix_type
  
  if(transition_time){
    
    att <- 1
    iterations <- 0
    first_fix <- sample(first_fix_type, 1)
    n_gaze_transitions <- 0
    
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
          iterations <- c(iterations, V)
          i_3 <- i_3+1}
        
        if(V >= 1 | V <= -1 | i_3 == fixation_time){ 
          att <- att+1
          
          n_gaze_transitions <- n_gaze_transitions+1
          fix_time <- i_3
          fix_item <- 3
          
        }} else if (att%%2==0 & att==att_up) {#If the gaze is to the item
          ######################## Second gaze ######################## 
          
          v <- d*(up - theta*(down*lambda))
          if(att_up==2){ time_up <- sample(first_fixation_time_up, 1) 
          } else {
            time_up <- sample(fixation_time_up, 1)
          }
          
          i_up <- 1
          
          while(V <= 1 && V >= -1 && i_up < time_up){
            V <- V + v + rnorm(1, 0, sigma)
            iterations <- c(iterations, V)
            i_up <- i_up+1}
          
          if(V >= 1 | V <= -1 | i_up == time_up){ 
            att <- att+1
            att_up <- att_up+4
            
            n_gaze_transitions <- n_gaze_transitions+1
            fix_time <- c(fix_time, i_up-1)
            fix_item <- c(fix_item, 1)
            
          }
          
        } else if(att%%2==0 & att==att_down) {
          
          v <- d*(up*theta - down*lambda)
          if(att_down==2){ time_down <- sample(first_fixation_time_down, 1) 
          } else {
            time_down <- sample(fixation_time_down, 1)
          }
          
          i_down <- 1
          
          while(V <= 1 && V >= -1 && i_down < time_down){
            V <- V + v + rnorm(1, 0, sigma)
            iterations <- c(iterations, V)
            i_down <- i_down+1}
          
          if(V >= 1 | V <= -1 | i_down == time_down){ 
            att <- att+1
            att_down <- att_down+4
            
            n_gaze_transitions <- n_gaze_transitions+1
            fix_time <- c(fix_time, i_down-1)
            fix_item <- c(fix_item, -1)
            
          }
          
        } else if (att%%2==1) {
          ######################## Third and all the other Odd gaze ########################
          v <- d* ( up - (lambda*down) )
          fixation_time <- sample(trans_time, 1)
          i_0 <- 1
          
          while(V <= 1 && V >= -1 && i_0 < fixation_time){
            V <- V + v + rnorm(1, 0, sigma)
            iterations <- c(iterations, V)
            i_0 <- i_0+1}
          
          if(V >= 1 | V <= -1 | i_0 == fixation_time){ 
            att <- att+1
            
            n_gaze_transitions <- n_gaze_transitions+1
            fix_time <- c(fix_time, i_0-1)
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
        
        v <- d*(up - theta*(down*lambda))
        
        if(att==1){ time_up <- sample(first_fixation_time_up, 1) 
        } else {
          time_up <- sample(fixation_time_up, 1)
        }
        
        i_up <- 1
        
        while(V <= 1 && V >= -1 && i_up < time_up){
          V <- V + v + rnorm(1, 0, sigma)
          iterations <- c(iterations, V)
          i_up <- i_up+1}
        
        if(V >= 1 | V <= -1 | i_up == time_up){ 
          att <- att+1
          
          n_gaze_transitions <- n_gaze_transitions+1
          fix_time <- c(fix_time, i_up-1)
          fix_item <- c(fix_item, 1)
          
        }} else if (att%%2==0) {#If the gaze is to the item
          ######################## Second gaze ######################## 
          
          v <- d*(up*theta - down*lambda)
          
          if(att==0){ time_down <- sample(first_fixation_time_down, 1) 
          } else {
            time_down <- sample(fixation_time_down, 1)
          }
          
          i_down <- 1
          
          while(V <= 1 && V >= -1 && i_down < time_down){
            V <- V + v + rnorm(1, 0, sigma)
            iterations <- c(iterations, V)
            i_down <- i_down+1}
          
          if(V >= 1 | V <= -1 | i_down == time_down){ 
            att <- att+1
            
            n_gaze_transitions <- n_gaze_transitions+1
            fix_time <- c(fix_time, i_down-1)
            fix_item <- c(fix_item, -1)
            
          }
          
        }}
    
  } 
  
  if ( V >= 1 ) {
    choice <-  1
  } else if (V <= -1){ 
    choice <- -1
  } else { choice = V}
  
  dat <- as.data.frame(suppressWarnings(cbind(RDV = iterations, choice, Time = 1:sum(fix_time), fix_item, fix_time, fix_time_cum = cumsum(fix_time), n_gaze_transitions)))
  
  if (plot_trial) {
    
    
    library(ggplot2)
    library(hrbrthemes)
    library(dplyr)
    library(ggthemes)
    
    
    #Funzione finale ----------------------------------------------------------------------
    creare_df_plot <- function(data) {
      
      n_gaze_transitions <- unique(data$n_gaze_transitions)
      
      
      if ( n_gaze_transitions == 1) {
        
        xmin = 0
        xmax = nrow(data)
        ymin = -0.99
        ymax = 0.99
        gaze_transition = "Gaze Transition"
        
        
        df.plot <-  data.frame ( xmin, xmax, ymin, ymax, gaze_transition )
        
      } else if ( n_gaze_transitions > 1 ) {
        
        xmin = c(0, unique(data$fix_time_cum)[-length(unique(data$fix_time_cum))] )
        xmax =  unique(data$fix_time_cum)
        ymin = -0.99
        ymax = 0.99
        gaze_transition = factor(data$fix_item[1:n_gaze_transitions])
        
        
        df.plot <-  data.frame ( xmin, xmax, ymin, ymax, gaze_transition )
        
      } 
      
      return(df.plot)
    }
    
    #Transform the level from -1 to 2 in order to have a clearer plot
    dat[dat$fix_item==-1, "fix_item"] <- 2
    
    df.plot <- creare_df_plot(data = dat)
    
    p <- ggplot(dat, aes(Time, RDV)) +
      scale_y_continuous(limits = c(-1, 1)) +
      scale_x_continuous(limits = c(0, nrow(dat))) +
      geom_hline(yintercept = c(1, -1), size = 1, color = "red", linetype = 2) +
      theme_tufte(ticks = FALSE) +
      geom_rect(data = df.plot,
                aes( xmin = xmin,
                     ymin = ymin,
                     xmax = xmax,
                     ymax = ymax,
                     fill = gaze_transition),
                alpha=0.5,inherit.aes=FALSE)+
      scale_fill_manual(values = c("3" = "white", "1" = color[2], "2" = color[1],"0" = "white"),
                        name = "Fixations", labels = c("3" = "","1"="Gain", "2"="Loss", "0"= "")) +
      geom_line(size = 0.8)  + labs(x = "Time (ms)")
    
    print(p)
    return(dat)
    
  } else {
    return( dat )
  }
  
}

# #Load Data
# library(tidyverse)
# data <- read.csv('/home/simone/Dropbox/Ongoing Projects/Analysis_Platt_Lab/Feng - Aging and Loss Aversion/Data/aDDM_data.csv') %>%
#   filter( !(subject %in% c(8, 9, 10, 15, 17, 23, 30)) ) %>%
#   mutate(trial = user::trials(.), subject = 0)
# 
# fix <- aDDM::fixations_type(data)
# 
# l2aDDM_trial(up=3, down = 3, sigma = 0.003, d = 0.0003, theta = 0.8, lambda = 1.3, fixations_type_list = fix)
