#######################################################
# Perceptual stability - ASD experiment preprocessing #
#######################################################

####### Read the .txt results from JATOS and perform the data preprocessing.

# (To reado the .txt results from JATOS it was necesary to add an "enter" before the curly brackets
# that open the component "sincericidio").

library(jsonlite)
library(dplyr)

# Read the .txt result
root <- rprojroot::is_rstudio_project
basename(getwd())

#read each line and convert

# data
content<-readLines(root$find_file("pilot_2/jatos_results_20230803152619.txt"))
res<-lapply(content,fromJSON)

# load the function to read the .txt results from JATOS and create a dataframe
source(root$find_file("Analysis/AuxiliaryFunctions/initial_df.R"))
df_list <- initial_df(res)

# df_NotExperimentData: for data of each subject.
# df_exp: save each trial of metacognition exp (already created in previous loop)
df_NotExperimentData <- df_list$a
df_exp <- df_list$b
AQ <- df_list$c

## check problems with videos loading, and canvas measures
videos_checking <- if(is.null(unique(df_exp$failed_video)[[1]])){print("videos: OK")}else{print("videos: problem")}
canvas_width_checking <- summary(df_exp$canvas_width)
canvas_height_checking <- summary(df_exp$canvas_height)

# first of all, I filter by columns of interest. Then I filter by rows of interest.
df_exp_filtered <- df_exp %>%
  # columns
  select(rt, stimulus, response, trial_type, trial_index) 

part_morph_condition <- c()
participant <- c()
part_nr <- 1

for(i in 1:nrow(df_exp_filtered)){
  # if trial index == 0, is a new participant
  if (df_exp_filtered$trial_index[i] == 0){
    if(df_exp_filtered$trial_index[i+56] == 56){ # if trial index i+56 is 56, is morph type 2
      part_morph_condition <- c(part_morph_condition,rep(2,57))
      participant <- c(participant,rep(part_nr,57))
      part_nr <- part_nr+1
    }else{
      part_morph_condition <- c(part_morph_condition,rep(1,47))# if trial index i+46 is 46, is morph type 1
      participant <- c(participant,rep(part_nr,47))
      part_nr <- part_nr+1
    }
  }
  
}
part_nr = part_nr-1

# add columns 
df_exp_filtered$part_morph_condition <- part_morph_condition
df_exp_filtered$participant <- participant

df <- data.frame()
# select the rows of interest
for (i in 1:part_nr) {
  d <- df_exp_filtered %>%
    filter(participant == i)
  if (d$part_morph_condition[1] == 1){
    d <- d %>%
      filter(trial_index %in% c(4,5,23,25,26,34,36,37,45))
    df <- rbind(df,d) 
  }else{
    d <- d %>%
      filter(trial_index %in% c(4,5,39,41,42,47,49,50,55))
    df <- rbind(df,d)
  }
}

df_exp_filtered <- df

for (i in 1:nrow(df_NotExperimentData)) {
  if(i ==1){
    d <- df_exp_filtered %>%
      filter(participant==i) %>%
      mutate(stimulus = case_when(row_number() == 2 ~ as.character(.$stimulus[1]),
                                  row_number() == 3 & part_morph_condition == 1 ~ "dot1_13_26",
                                  row_number() == 3 & part_morph_condition == 2 ~ "dot2_26_52",
                                  row_number() == 5 ~ as.character(.$stimulus[4]),
                                  row_number() == 6 & part_morph_condition == 1 ~ "dot2_26",
                                  row_number() == 6 & part_morph_condition == 2 ~ "dot1_13",
                                  row_number() == 8 ~ as.character(.$stimulus[7]),
                                  row_number() == 9 & part_morph_condition == 1 ~ "dot2_52",
                                  row_number() == 9 & part_morph_condition == 2 ~ "dot1_26"),
             response = case_when(row_number() == 2 ~ as.numeric(.$response[[2]]$Estimation),
                                  row_number() == 3 ~ as.numeric(.$response[[3]]$Estimation),
                                  row_number() == 5 ~ as.numeric(.$response[[5]]$Estimation),
                                  row_number() == 6 ~ as.numeric(.$response[[6]]$Estimation),
                                  row_number() == 8 ~ as.numeric(.$response[[8]]$Estimation),
                                  row_number() == 9 ~ as.numeric(.$response[[9]]$Estimation)))
  }else{
    d_mod <- df_exp_filtered %>%
      filter(participant==i) %>%
      mutate(stimulus = case_when(row_number() == 2 ~ as.character(.$stimulus[1]),
                                  row_number() == 3 & part_morph_condition == 1 ~ "dot1_13_26",
                                  row_number() == 3 & part_morph_condition == 2 ~ "dot2_26_52",
                                  row_number() == 5 ~ as.character(.$stimulus[4]),
                                  row_number() == 6 & part_morph_condition == 1 ~ "dot2_26",
                                  row_number() == 6 & part_morph_condition == 2 ~ "dot1_13",
                                  row_number() == 8 ~ as.character(.$stimulus[7]),
                                  row_number() == 9 & part_morph_condition == 1 ~ "dot2_52",
                                  row_number() == 9 & part_morph_condition == 2 ~ "dot1_26"),
             response = case_when(row_number() == 2 ~ as.numeric(.$response[[2]]$Estimation),
                                  row_number() == 3 ~ as.numeric(.$response[[3]]$Estimation),
                                  row_number() == 5 ~ as.numeric(.$response[[5]]$Estimation),
                                  row_number() == 6 ~ as.numeric(.$response[[6]]$Estimation),
                                  row_number() == 8 ~ as.numeric(.$response[[8]]$Estimation),
                                  row_number() == 9 ~ as.numeric(.$response[[9]]$Estimation)))
    d <- rbind(d,d_mod)
    
  }
}

# filter again
df_exp_filtered_mod <- d %>%
  # rows
  filter(stimulus %in% c("videos/fs1yo.mp4","dot1_13_26","videos/fs2y.mp4","dot2_26","videos/fs2o.mp4",
                         "dot2_52","videos/fs2yo.mp4","dot2_26_52","videos/fs1y.mp4","dot1_13",
                         "videos/fs1o.mp4","dot1_26")) %>%
  # columns
  select(-trial_type, -trial_index, -part_morph_condition)

####### adding participants and trials columns to df_exp_filtered_mod

# prepare trials column
trials <- rep(1:6, times = nrow(df_NotExperimentData))

# add columns to df_exp
df_exp_filtered_mod$trial <- trials

####### get the AQ quotient 

# number of AQ sublists for each subject 
n_AQ_sublists <- 2

# number of subject 
n_participants <- nrow(df_NotExperimentData)

# location of the sublist where the responses to the AQ of the first subject are
ubicacion_comp_AQ <- 2

# load the function to get the AQ quotient  
#source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ.R"))
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ_CORREGIDA.R"))
# get the AQ quotient
#puntaje_AQ_sujetos <- puntaje_AQ(cant_sujetos,
#                                 cant_componentes_por_sujetos,
#                                 ubicacion_comp_AQ,
#                                 AQ)
###### this is the function for AQ
puntaje_AQ_sujetos <- puntaje_AQ_corregido(n_participants,   ################################ me tira error
                                           n_AQ_sublists,
                                           ubicacion_comp_AQ,
                                           AQ)
### this is the function for the social subscale of AQ 
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ_social.R"))

puntaje_AQ_sujetos_social <- puntaje_AQ_social(n_participants,
                                               n_AQ_sublists,
                                           ubicacion_comp_AQ,
                                           AQ)

### this is the function for the attencion switch subscale of AQ
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ_atencion_switch.R"))

puntaje_AQ_sujetos_atencion_switch <- puntaje_AQ_atencion_switch(n_participants,
                                                                 n_AQ_sublists,
                                               ubicacion_comp_AQ,
                                               AQ)

### this is the function for the attencion to detail subscale of AQ
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ_atencion_detail.R"))

puntaje_AQ_sujetos_atencion_detail <- puntaje_AQ_atencion_detail(n_participants,
                                                                 n_AQ_sublists,
                                                                 ubicacion_comp_AQ,
                                                                 AQ)


### ### this is the function for the comunication subscale of AQ
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ_communication.R"))

puntaje_AQ_sujetos_communication <- puntaje_AQ_communication(n_participants,
                                                             n_AQ_sublists,
                                                                 ubicacion_comp_AQ,
                                                                 AQ)

### ### this is the function for the imagination subscale of AQ
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ_imagination.R"))

puntaje_AQ_sujetos_imagination <- puntaje_AQ_imagination(n_participants,
                                                         n_AQ_sublists,
                                                            ubicacion_comp_AQ,
                                                            AQ)

# add to df_NotExperimentData
df_NotExperimentData$AQ <- puntaje_AQ_sujetos 
df_NotExperimentData$AQ_social <- puntaje_AQ_sujetos_social
df_NotExperimentData$AQ_attentional_switches <- puntaje_AQ_sujetos_atencion_switch
df_NotExperimentData$AQ_attencion_detail <- puntaje_AQ_sujetos_atencion_detail
df_NotExperimentData$AQ_communication <- puntaje_AQ_sujetos_communication
df_NotExperimentData$AQ_imagination <- puntaje_AQ_sujetos_imagination


## yuval s format
library(tidyverse)

df_total_response <- df_exp_filtered_mod %>%
  pivot_wider(names_from = trial, names_prefix = "response_trial_", values_from = response) %>%
  mutate_all(., ~replace(., is.na(.), 5555)) %>%
  group_by(participants) %>%
  summarise(response_trial_1 = min(response_trial_1),
            response_trial_2 = min(response_trial_2),
            response_trial_3 = min(response_trial_3),
            response_trial_4 = min(response_trial_4),
            response_trial_5 = min(response_trial_5),
            response_trial_6 = min(response_trial_6))

df_total_rt <- df_exp_filtered_mod %>%
  pivot_wider(names_from = trial, names_prefix = "rt_trial_", values_from = rt) %>%
  mutate_all(., ~replace(., is.na(.), -100)) %>%
  group_by(participants) %>%
  summarise(rt_trial_1 = max(rt_trial_1),
            rt_trial_2 = max(rt_trial_2),
            rt_trial_3 = max(rt_trial_3),
            rt_trial_4 = max(rt_trial_4),
            rt_trial_5 = max(rt_trial_5),
            rt_trial_6 = max(rt_trial_6))

df_total_stim <- df_exp_filtered_mod %>%
  pivot_wider(names_from = trial, names_prefix = "stim_trial_", values_from = stimulus) %>%
  mutate_all(., ~replace(., is.na(.), -100)) %>%
  group_by(participants) %>%



for (i in 1:length(unique(df_exp_filtered_mod$participants))) {
  lala <- df_exp_filtered_mod %>%
    filter(participants == i)
  df_total <- cbind(df_total, lala)
}

######## I observe if all the paritcipant.id are in prolific data and the other
######## way around 
prolific_info<-read.csv("pilot/prolific_export_05062023_40_participants.csv") 
prolific_info$Participant.id %in%  df_NotExperimentData$prolific
df_NotExperimentData$prolific %in% prolific_info$Participant.id

### save the data frames
write.csv(df_exp_filtered_mod, "pilot_2/df_exp_filtered_mod.csv", row.names=FALSE)
write.csv(df_NotExperimentData, "pilot_2/df_NotExperimentData.csv", row.names=FALSE)

