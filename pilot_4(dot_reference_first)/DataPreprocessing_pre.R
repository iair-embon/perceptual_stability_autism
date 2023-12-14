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
content<-readLines(root$find_file("pilot_4(dot_reference_first)/jatos_results_20231214141104.txt"))
res<-lapply(content,fromJSON)

# load the function to read the .txt results from JATOS and create a dataframe
source(root$find_file("pilot_4(dot_reference_first)/initial_df_pilot.R"))
df_list <- initial_df(res)

# df_NotExperimentData: for data of each subject.
# df_exp: save each trial of metacognition exp (already created in previous loop)
df_NotExperimentData <- df_list$a
df_exp <- df_list$b
AQ <- df_list$c


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
write.csv(df_exp, "pilot_4(dot_reference_first)/df_exp.csv", row.names=FALSE)
write.csv(df_NotExperimentData, "pilot_4(dot_reference_first)/df_NotExperimentData.csv", row.names=FALSE)

