# update 22/01/2020
#In questa funzione, la variabile first_fix_type dev'essere 1 o -1.
raDDM <- function(n, sigma, theta, d, fixation_time_up, fixation_time_down, 
                  trans_time, first_fixation_time_up, first_fixation_time_down,
                  non_dec_time, first_fix_type, num_up_boundary, z = 0, value_up_boundary, value_down_boundary, 
                  transition_time = TRUE, device = "Linux") {
  
  library(tidyverse)
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
  raDDM_one <- function( n, up, down, sigma, theta, d, z, first_fixation_time_up, first_fixation_time_down, 
                         fixation_time_up, fixation_time_down, trans_time, non_dec_time, first_fix_type,
                         num_up_boundary, transition_time) {
        
        raDDM1 <- function(n, up, down, sigma, theta, d, z, first_fixation_time_up, first_fixation_time_down, 
                           fixation_time_up, fixation_time_down, trans_time, non_dec_time, first_fix_type,
                           num_up_boundary, transition_time) {
          
          n <- 1:n
          V <- z
          
          raddm <- function(trial){
            V <- z
            
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
                    
                    v <- d*(up - theta*down)
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
                    
                    v <- d*(up*theta - down)
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
                    v <- 0
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
                  
                  v <- d*(up - theta*down)
                  
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
                    
                    v <- d*(up*theta - down)
                    
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
          
          if(device=="Windows"){
            library(foreach)
            library(doParallel)
            #cl <- makeCluster(detectCores())
            registerDoParallel(cores = detectCores())
            results <- foreach(n) %dopar% raddm()
          } else if (device=="Linux") {
            library(parallel)
            results <- mclapply(n, raddm, mc.cores = detectCores() )
          }
          
          results <- lapply(n, function(i){
            cbind(results[[i]], trial = rep(i, nrow(results[[i]])))
          })
          
          results <- results %>%
            do.call(rbind.data.frame, .)
          
          return(results)
        }
        
        results <- raDDM1(n = n, up=up, down=down, sigma=sigma, theta=theta, d=d, z=z, 
                          first_fixation_time_up=first_fixation_time_up, 
                          first_fixation_time_down=first_fixation_time_down, 
                          fixation_time_up=fixation_time_up, fixation_time_down=fixation_time_down, 
                          trans_time=trans_time, non_dec_time=non_dec_time, first_fix_type=first_fix_type,
                          num_up_boundary=num_up_boundary, transition_time=transition_time)
        return(results)
  }
      
  create.rt <- function(n,  sigma, theta, d, z, first_fix_time, non_first_fix_time, trans_time, non_dec_time ) {
    
    rt <- pblapply(1:n_up_down, function(i){
      
      rt <- raDDM_one( n = n, up = up_down[i, 1], down = up_down[i, 2], sigma = sigma, theta = theta, d = d, z = z, 
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
  
  rt <- create.rt(n = n,  sigma = sigma, theta = theta, d = d, z = z, 
                  first_fix_time = first_fix_time, non_first_fix_time = non_first_fix_time, 
                  trans_time = trans_time, non_dec_time = non_dec_time)
  
  return( rt )
  
}

# library(tidyverse)
# library(user)
# data_aDDM <- read.csv('/home/simone/Dropbox/Ongoing Projects/Analysis_Platt_Lab/Feng - Aging and Loss Aversion/Data/aDDM_data.csv')
# fixations_type <- function(fixations, fix_item = c(-1, 1)){
#   
#   library(ggplot2)
#   library(dplyr)
#   
#   if ( is.character(fixations) ){
#     fixations <- read.csv(fixations)
#   } 
#   
#   if (  any(names(fixations) ==  'sbj') ){
#     fixations <- fixations %>%
#       na.omit(.)
#     names(fixations)[which(names(fixations) == 'sbj')] <- 'parcode'
#     
#   } else if (any(names(fixations) ==  'subject')){
#     fixations <- fixations %>%
#       rename(parcode = subject) %>%
#       na.omit(.)
#   } else {fixations <- fixations %>%
#     na.omit(.)
#   }
#   
#   # Tempi di fissazione empirici rivolti verso gli item, che siano i primi
#   first_fix_time <- na.omit(unlist( sapply(1: length(unique(fixations$parcode)), function(i){
#     
#     fix <- fixations[fixations$parcode == unique(fixations$parcode)[i], ]
#     sapply( unique(fix$trial), function(t){
#       fix[fix$trial == t & fix$fix_item != 3, "fix_time"][1]
#     })
#     
#   })))
#   
#   first_fix_time <- first_fix_time[first_fix_time<5000 & first_fix_time>10]
#   
#   #hist(first_fix_time)
#   
#   first_fix_type <- unlist(lapply( unique(fixations$parcode), function(sbj_i){
#     fix_i <- fixations[fixations$parcode == sbj_i, ]
#     sapply( unique(fix_i$trial), function(trial_i){
#       fix_i[fix_i$trial == trial_i & fix_i$fix_item != 3, 'fix_item'][1]
#     })})) %>% na.omit()
#   
#   #barplot(table(first_fix_type))
#   # Tempi di fissazione empirici rivolti verso gli item, che non siano nè i primi ne gli ultimi
#   non_first_fix_time <- unlist( sapply(unique(fixations$parcode), function(parcode_i){
#     fix <- fixations[fixations$parcode == parcode_i, ]
#     
#     sapply( unique(fix$trial), function(t){
#       all_fix <- fix[fix$trial == t & fix$fix_item != 3, "fix_time"]
#       (non_first_fix_time <- all_fix[- c(1, length(all_fix)) ])
#     }) }) )
#   
#   #hist(non_first_fix_time)
#   first_fix_time <- first_fix_time[first_fix_time<5000 & first_fix_time>10]
#   
#   #################### Fixation time up. Non First, non last ####################
#   fixation_time_up <- lapply(unique(fixations$parcode), function(par_i){
#     fix <- fixations[fixations$parcode == par_i, ]
#     
#     lapply(unique(fix$trial), function(trl_i){
#       
#       fix_tm_up <- fix[fix$trial==trl_i & fix$fix_item == 1, "fix_time"]
#       
#       if( last(fix[fix$trial==trl_i, "fix_item"]) == 1 & 
#           first(fix[fix$trial==trl_i, "fix_item"]) == 1){
#         
#         fix_tm_up[- c(1, length(fix_tm_up))]
#         
#       } else if ( first(fix[fix$trial==trl_i, "fix_item"]) == 1 ){
#         
#         fix_tm_up[- 1]
#         
#       } else if ( last(fix[fix$trial==trl_i, "fix_item"]) == 1 ){
#         
#         fix_tm_up[- length(fix_tm_up)]
#         
#       } else {
#         
#         fix[fix$trial==trl_i & fix$fix_item == 1, "fix_time"]
#         
#       }
#       
#     })
#     
#   }) %>% unlist()
#   
#   #################### First fixation time up. Non First, non last ####################
#   first_fixation_time_up <- lapply(unique(fixations$parcode), function(par_i){
#     fix <- fixations[fixations$parcode == par_i, ]
#     
#     sapply(unique(fix$trial), function(trl_i){
#       if (!is.na(first(fix[fix$trial==trl_i & fix$fix_item!=3, "fix_item"]))){
#         if( first(fix[fix$trial==trl_i & fix$fix_item!=3, "fix_item"]) == 1 ){ first(fix[fix$trial==trl_i & fix$fix_item == 1, "fix_time"]) } 
#       }
#     }) }) %>% unlist()
#   
#   #################### Fixation time down Non First, non last ####################
#   fixation_time_down <- lapply(unique(fixations$parcode), function(par_i){
#     fix <- fixations[fixations$parcode == par_i, ]
#     
#     lapply(unique(fix$trial), function(trl_i){
#       
#       fix_tm_down <- fix[fix$trial==trl_i & fix$fix_item == -1, "fix_time"]
#       
#       if( last(fix[fix$trial==trl_i, "fix_item"]) == -1 & 
#           first(fix[fix$trial==trl_i, "fix_item"]) == -1){
#         
#         fix_tm_down[- c(1, length(fix_tm_down))]
#         
#       } else if ( first(fix[fix$trial==trl_i, "fix_item"]) == -1 ){
#         
#         fix_tm_down[- 1]
#         
#       } else if ( last(fix[fix$trial==trl_i, "fix_item"]) == -1 ){
#         
#         fix_tm_down[- length(fix_tm_down)]
#         
#       } else {
#         
#         fix_tm_down
#         
#       }
#     })
#     
#   }) %>% unlist()
#   
#   #################### First fixation time up. Non First, non last ####################
#   first_fixation_time_down <- lapply(unique(fixations$parcode), function(par_i){
#     fix <- fixations[fixations$parcode == par_i, ]
#     
#     sapply(unique(fix$trial), function(trl_i){
#       if (!is.na(first(fix[fix$trial==trl_i & fix$fix_item!=3, "fix_item"]))){
#         if( first(fix[fix$trial==trl_i & fix$fix_item!=3, "fix_item"]) == -1 ){ first(fix[fix$trial==trl_i & fix$fix_item == -1, "fix_time"]) } 
#       }
#     }) }) %>% unlist()
#   
#   # Tempo non decisionale
#   non_dec_time <- sapply(unique(fixations$parcode), function(par_i){
#     fix <- fixations[fixations$parcode==par_i, ]
#     lapply(unique(fix$trial), function(t){
#       trans_time <- first(fix[fix$trial==t & fix$fix_item==3, "fix_time"])
#     }) %>% unlist()
#   } ) %>% unlist() %>% na.omit()
#   #hist(non_dec_time)
#   
#   # Tempo di transizione
#   trans_time <- sapply(unique(fixations$parcode), function(par_i){
#     fix <- fixations[fixations$parcode==par_i, ]
#     lapply(unique(fix$trial), function(t){
#       trans_time <- fix[fix$trial==t & fix$fix_item==3, "fix_time"]
#       trans_time[-1]
#     }) %>% unlist()
#   } ) %>% unlist()
#   
#   #hist(trans_time)
#   return( list(trans_time = round(trans_time), non_dec_time = round(non_dec_time), 
#                non_first_non_last_fix_time=round(non_first_fix_time), first_fix_time=round(first_fix_time),
#                first_fix_type = first_fix_type, fixation_time_up = fixation_time_up, fixation_time_down = fixation_time_down,
#                first_fixation_time_down = first_fixation_time_down, first_fixation_time_up = first_fixation_time_up) )
#   
# }
# fix_type <- fixations_type( fixations = data_aDDM )
# 
# raDDM(n = 1, sigma = 0.0233, theta = 0.7, z = 0, d = 0.000065, value_up_boundary = 1:20, value_down_boundary = 1:20,
#       first_fixation_time_up = fix_type$first_fixation_time_up,
#       first_fixation_time_down = fix_type$first_fixation_time_down,
#       fixation_time_up = fix_type$fixation_time_up,
#       fixation_time_down = fix_type$fixation_time_down,
#       first_fix_type = fix_type$first_fix_type,
#       trans_time = fix_type$trans_time[fix_type$trans_time<100],
#       non_dec_time = fix_type$non_dec_time,
#       num_up_boundary = 1, transition_time = F)
