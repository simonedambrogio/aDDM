# update 24/08/2019
# Con questo aggiornamento, è possibile scegliere se simulare un tria sia che tenga conto dei tempi di transizione,
# sia che non ne tenga conto. Di default tiene cont dei tempi di transizione
aDDM_trial <- function( up, down, sigma, theta, d, V = 0, first_fix_time, non_first_fix_time, 
                        trans_time, non_dec_time, transition_time = FALSE, 
                        plot_trial = FALSE, color = c("maroon4", "mediumaquamarine")) {
    
  library(dplyr)
  
  if (transition_time){
  trans_time_non_first_fix_time <- c(rbind(sample((trans_time), 500, replace = T), 
                                           sample((non_first_fix_time), 500, replace = T)))
  s <- c( sample(non_dec_time, 1), #3
          sample(trans_time, 1), #0
          sample(first_fix_time, 1), # 1 o 2 first
          trans_time_non_first_fix_time) # 0, 1o2, 0, 1o2, ...
  
  
  #attenzione
  att <- 0
  i <- 0
  nat <- 1
  nat3 <- 3
  n <- sample(0:1, 1)
  fir_fix <- n
  fix_item <- c()
  f_i <- 3
  
  s1 <- c(0, s)
  
  
  iterazioni <- c()
  
  while (V >= -1 && V <= 1) {
    
    if (s[nat] == (i - sum(att)) & s[nat3] == (i - sum(att)) & nat == nat3 ) {
      
      att <- c(att, s[nat])
      nat <- nat + 1
      
      nat3 <- nat3 + 2
      n <- n+1
    } else if ( s[nat] == (i - sum(att)) ) {
      
      att <- c(att, s[nat])
      nat <- nat + 1}
    
    if (nat == 1){
      v <- 0
    } else if (nat %% 2 == 0){
      v <- 0
      f_i <- 0
      
    } else if (nat %% 2 == 1 & nat != 1 & n %% 2 == 0){ #se n è pari (0), lo sguardo è verso up
      
      v <-  d*(up - theta*down)
      f_i <- 2
      
    } else if (nat %% 2 == 1 & nat != 1 & n %% 2 == 1){
      
      v <- d*(theta*up - down)
      f_i <- 1
      
    }
    
    if( s1[nat] == (i - sum( att[- length(att)])) ){
      fix_item <- c(fix_item, f_i)
    }
    
    i <- i + 1
    V <- V + v + rnorm(1, 0, sigma)
    iterazioni <- c(iterazioni, V)
    cum.att <- cumsum( att[1:nat])
    
  }} else
    {
    s <- c(sample(first_fix_time, 1), sample(non_first_fix_time, 1000, replace = TRUE))
    
    #attenzione
    att <- 0
    i <- 0
    nat <- 1
    n <- sample(0:1, 1)
    fir_fix <- n
    fix_item <- c()
    f_i <- 3
    
    s1 <- c(0, s)
    
    
    iterazioni <- c()
    
    while (V >= -1 && V <= 1) {
      
      if (s[nat] == (i - sum(att)) ) {
        
        att <- c(att, s[nat])
        nat <- nat + 1
        
        n <- n+1
        
      }
      
      if (n %% 2 == 0) {
        v <-  d*(up - theta*down)
        f_i <- 2
      } else if (n %% 2 == 1) {
        v <- d*(theta*up - down)
        f_i <- 1
        
      } 
      
      if( s1[nat] == (i - sum( att[- length(att)])) ){
        fix_item <- c(fix_item, f_i)
      }
      
      i <- i + 1
      V <- V + v + rnorm(1, 0, sigma)
      iterazioni <- c(iterazioni, V)
      cum.att <- cumsum( att[1:nat])
    }
  }
  
  att <- att[-1]
  att <- c(att, tail(i, 1) - tail( unique(cum.att), 1) )
  cum.att <- cum.att[-1]
  cum.att <- c(cum.att, tail(i, 1))
  dat <- as.data.frame( cbind(iterazioni, 1:i, att, cum.att, nat, fix_item) )
  names(dat) <- c("RDV", "Time", "fix_time", "fix_time_cum", "Sguardo", "fix_item")
  
  if (nrow(dat) < s[1]) {
    
    dat$Attenzione <- i
    dat$Att.Cum <- i
    dat$Attenzione[1] <- 0
  }
  
  
  if (plot_trial) {
    
    
    library(ggplot2)
    library(hrbrthemes)
    library(dplyr)
    library(ggthemes)
    
    
    #Funzione finale ----------------------------------------------------------------------
    creare_df_plot <- function(data) {
      
      n <- data[1,5]
      
      
      if ( n == 1) {
        
        xmin = 0
        xmax = nrow(data)
        ymin = -0.99
        ymax = 0.99
        sguardo = c("Sgurado")
        
        
        df.plot <-  data.frame ( xmin, xmax, ymin, ymax, sguardo )
        
      } else if ( n > 1 ) {
        
        xmin = c(0, unique(data$fix_time_cum)[-length(unique(data$fix_time_cum))] )
        xmax =  unique(data$fix_time_cum)
        ymin = -0.99
        ymax = 0.99
        sguardo = factor(data$fix_item[1:n])
        
        
        df.plot <-  data.frame ( xmin, xmax, ymin, ymax, sguardo )
        
      } 
      
      return(df.plot)
    }
    
    df.plot <- creare_df_plot(data = dat )
    
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
                     fill = sguardo),
                alpha=0.5,inherit.aes=FALSE)+
      scale_fill_manual(values = c("3" = "white", "1" = color[1], "2" = color[2],"0" = "white"),
                        name = "Fixations", labels = c("3" = "", "2"="Loss","1"="Gain","0"= "")) +
      geom_line(size = 0.8) 
    
    print(p)
    return(dat)
    
  } else {
    return( dat )
  }
}

#source('Functions/aDDM/fixations_type.R')
#data_aDDM <- read.csv('Data/Dati per stimare parametri con aDDM - Tavares 2017/Funzione R/data_aDDM.csv')
#data_aDDM_C0 <- data_aDDM %>% filter(group == 'C0')
#fix_type <- fixations_type( fixations = data_aDDM_C0 )

#Generate artificial trial + plot
#aDDM_trial(up = 25, down = 25, sigma = 0.004, theta = 0.2, d = 0.000065,
#           first_fix_time = fix_type$first_fix_time, 
#           non_first_fix_time = fix_type$non_first_non_last_fix_time,
#           trans_time = fix_type$trans_time, non_dec_time = fix_type$non_dec_time, 
#           plot_trial = T)

