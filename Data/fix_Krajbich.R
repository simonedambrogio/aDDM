#Questo dataset serve ad avere delle fissazione di lunghezze ragionevoli quando si vogliono simulare dati
library(dplyr)

data_aDDM <- foreign::read.dta('/home/simone/Documenti/Data Analysis/Datasets/Krajbich (2010)/original/data_nature2010.dta') %>% 
  select(subject, trial, event_duration, choice) %>% tbl_df()


first_fixation_time <- data_aDDM %>% group_by(subject, trial) %>% filter(row_number()==1) %>% ungroup() %>% .[, 'event_duration', drop=T]
first_fixation_time <- first_fixation_time[first_fixation_time>100]

fixation_time <- data_aDDM %>% group_by(subject, trial) %>% filter(row_number()!=1) %>% ungroup() %>% .[, 'event_duration', drop=T]
fixation_time <- fixation_time[fixation_time>100 & fixation_time<1500]

fix_Krajbich <- data.frame(first_fixation_time=first_fixation_time[1:3000], fixation_time=fixation_time[1:3000])
write.csv(fix_Krajbich, 'Data/fix_Krajbich.csv', row.names = FALSE)
