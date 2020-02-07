
fixations_type <- function(fixations, fix_item = c(-1, 1)){

library(ggplot2)
library(dplyr)
  
  if ( is.character(fixations) ){
    fixations <- read.csv(fixations)
  } 

  if (  any(names(fixations) ==  'sbj') ){
  fixations <- fixations %>%
               na.omit(.)
  names(fixations)[which(names(fixations) == 'sbj')] <- 'parcode'
  
  } else if (any(names(fixations) ==  'subject')){
    fixations <- fixations %>%
      rename(parcode = subject) %>%
      na.omit(.)
  } else {fixations <- fixations %>%
                          na.omit(.)
    }
  
# Tempi di fissazione empirici rivolti verso gli item, che siano i primi
first_fix_time <- na.omit(unlist( sapply(1: length(unique(fixations$parcode)), function(i){
    
  fix <- fixations[fixations$parcode == unique(fixations$parcode)[i], ]
  sapply( unique(fix$trial), function(t){
    fix[fix$trial == t & fix$fix_item != 3, "fix_time"][1]
  })
  
})))

first_fix_time <- first_fix_time[first_fix_time<5000 & first_fix_time>10]

#hist(first_fix_time)

first_fix_type <- unlist(lapply( unique(fixations$parcode), function(sbj_i){
                      fix_i <- fixations[fixations$parcode == sbj_i, ]
                      sapply( unique(fix_i$trial), function(trial_i){
                        fix_i[fix_i$trial == trial_i & fix_i$fix_item != 3, 'fix_item'][1]
                      })})) %>% na.omit()
    
#barplot(table(first_fix_type))
# Tempi di fissazione empirici rivolti verso gli item, che non siano nè i primi ne gli ultimi
non_first_fix_time <- unlist( sapply(unique(fixations$parcode), function(parcode_i){
                          fix <- fixations[fixations$parcode == parcode_i, ]
                          
                          sapply( unique(fix$trial), function(t){
                            all_fix <- fix[fix$trial == t & fix$fix_item != 3, "fix_time"]
                            (non_first_fix_time <- all_fix[- c(1, length(all_fix)) ])
                            }) }) )

#hist(non_first_fix_time)
first_fix_time <- first_fix_time[first_fix_time<5000 & first_fix_time>10]

#################### Fixation time up. Non First, non last ####################
fixation_time_up <- lapply(unique(fixations$parcode), function(par_i){
        fix <- fixations[fixations$parcode == par_i, ]
        
        lapply(unique(fix$trial), function(trl_i){
          
          fix_tm_up <- fix[fix$trial==trl_i & fix$fix_item == 1, "fix_time"]
          
          if( last(fix[fix$trial==trl_i, "fix_item"]) == 1 & 
              first(fix[fix$trial==trl_i, "fix_item"]) == 1){
            
            fix_tm_up[- c(1, length(fix_tm_up))]
          
          } else if ( first(fix[fix$trial==trl_i, "fix_item"]) == 1 ){
            
            fix_tm_up[- 1]
            
          } else if ( last(fix[fix$trial==trl_i, "fix_item"]) == 1 ){
            
            fix_tm_up[- length(fix_tm_up)]
            
          } else {
            
            fix[fix$trial==trl_i & fix$fix_item == 1, "fix_time"]
            
            }
          
        })
        
      }) %>% unlist()

#################### First fixation time up. Non First, non last ####################
first_fixation_time_up <- lapply(unique(fixations$parcode), function(par_i){
  fix <- fixations[fixations$parcode == par_i, ]
  
  sapply(unique(fix$trial), function(trl_i){
    if (!is.na(first(fix[fix$trial==trl_i & fix$fix_item!=3, "fix_item"]))){
    if( first(fix[fix$trial==trl_i & fix$fix_item!=3, "fix_item"]) == 1 ){ first(fix[fix$trial==trl_i & fix$fix_item == 1, "fix_time"]) } 
    }
    }) }) %>% unlist()

#################### Fixation time down Non First, non last ####################
fixation_time_down <- lapply(unique(fixations$parcode), function(par_i){
  fix <- fixations[fixations$parcode == par_i, ]
  
  lapply(unique(fix$trial), function(trl_i){
    
    fix_tm_down <- fix[fix$trial==trl_i & fix$fix_item == -1, "fix_time"]
    
    if( last(fix[fix$trial==trl_i, "fix_item"]) == -1 & 
        first(fix[fix$trial==trl_i, "fix_item"]) == -1){
      
      fix_tm_down[- c(1, length(fix_tm_down))]
      
    } else if ( first(fix[fix$trial==trl_i, "fix_item"]) == -1 ){
      
      fix_tm_down[- 1]
      
    } else if ( last(fix[fix$trial==trl_i, "fix_item"]) == -1 ){
      
      fix_tm_down[- length(fix_tm_down)]
      
    } else {
      
      fix_tm_down
      
    }
  })
  
}) %>% unlist()

#################### First fixation time up. Non First, non last ####################
first_fixation_time_down <- lapply(unique(fixations$parcode), function(par_i){
  fix <- fixations[fixations$parcode == par_i, ]
  
  sapply(unique(fix$trial), function(trl_i){
    if (!is.na(first(fix[fix$trial==trl_i & fix$fix_item!=3, "fix_item"]))){
      if( first(fix[fix$trial==trl_i & fix$fix_item!=3, "fix_item"]) == -1 ){ first(fix[fix$trial==trl_i & fix$fix_item == -1, "fix_time"]) } 
    }
  }) }) %>% unlist()

# Tempo non decisionale
non_dec_time <- sapply(unique(fixations$parcode), function(par_i){
  fix <- fixations[fixations$parcode==par_i, ]
  lapply(unique(fix$trial), function(t){
    trans_time <- first(fix[fix$trial==t & fix$fix_item==3, "fix_time"])
  }) %>% unlist()
} ) %>% unlist() %>% na.omit()
#hist(non_dec_time)

# Tempo di transizione
trans_time <- sapply(unique(fixations$parcode), function(par_i){
  fix <- fixations[fixations$parcode==par_i, ]
  lapply(unique(fix$trial), function(t){
    trans_time <- fix[fix$trial==t & fix$fix_item==3, "fix_time"]
    trans_time[-1]
  }) %>% unlist()
} ) %>% unlist()

#hist(trans_time)
return( list(trans_time = round(trans_time), non_dec_time = round(non_dec_time), 
             non_first_non_last_fix_time=round(non_first_fix_time), first_fix_time=round(first_fix_time),
             first_fix_type = first_fix_type, fixation_time_up = fixation_time_up, fixation_time_down = fixation_time_down,
             first_fixation_time_down = first_fixation_time_down, first_fixation_time_up = first_fixation_time_up) )

}

#fixations_type( fixations = data_pooled )

# timestep 10
#first_fix_time <- round(first_fix_time / 10)
#non_first_fix_time <- round(non_first_fix_time / 10)
#non_dec_time <- round(non_dec_time / 10)
#trans_time <- round(trans_time / 10)
