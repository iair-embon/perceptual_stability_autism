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
content<-readLines(root$find_file("pilot_3/dot_morph_first/jatos_results_20230816142841.txt"))
res<-lapply(content,fromJSON)

# load the function to read the .txt results from JATOS and create a dataframe
source(root$find_file("Analysis/AuxiliaryFunctions/initial_df_pilot_guille.R"))
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
  select(rt, stimulus, response, trial_type, trial_index, position, type, participant) 


df <- data.frame()
# select the rows of interest
for (i in 1:length (unique(df_exp_filtered$participant))) {
  d <- df_exp_filtered %>%
    filter(participant == i)
  
  d_morph <- d %>%
    filter(type == "morph")

  d_morph <- d_morph %>%
    filter(trial_index == 36)
  
  df <- rbind(df,d_morph)
  
  d_many <- d %>%
    filter(type == "many")
  
  d_many <- d_many %>%
    filter(trial_index == 10)
  
  df <- rbind(df,d_many)
}

df_exp_filtered <- df
df_exp_filtered$response <- as.numeric(unlist( df_exp_filtered$response))

# filter again
df_exp_filtered_mod <- df_exp_filtered %>%
  select(-trial_type, -trial_index, -stimulus)

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


######## I observe if all the paritcipant.id are in prolific data and the other
######## way around 
# prolific_info<-read.csv("pilot/prolific_export_05062023_40_participants.csv") 
# prolific_info$Participant.id %in%  df_NotExperimentData$prolific
# df_NotExperimentData$prolific %in% prolific_info$Participant.id

### save the data frames
write.csv(df_exp_filtered_mod, "pilot_3/dot_morph_first/df_exp_filtered_mod.csv", row.names=FALSE)
write.csv(df_NotExperimentData, "pilot_3/dot_morph_first/df_NotExperimentData.csv", row.names=FALSE)

