# Read the .txt result
root <- rprojroot::is_rstudio_project
basename(getwd())

# n participant
n_participants <- 1000

source(root$find_file("Analysis/AuxiliaryFunctions/sim_AQ_participants.R"))

# Simular respuestas para 5 sujetos con 50 preguntas cada uno
respuestas_simuladas <- sim_AQ_participants(n_s = n_participants)

# number of AQ sublists for each subject 
n_AQ_sublists <- 2

# location of the sublist where the responses to the AQ of the first subject are
ubicacion_comp_AQ <- 2

# load the function to get the AQ quotient  
#source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ.R"))
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ_CORREGIDA.R"))

###### this is the function for AQ
puntaje_AQ_sujetos <- puntaje_AQ_corregido(n_participants,
                                           n_AQ_sublists,
                                           ubicacion_comp_AQ,
                                           respuestas_simuladas)

### this is the function for the social subscale of AQ 
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ_social.R"))

puntaje_AQ_sujetos_social <- puntaje_AQ_social(n_participants,
                                               n_AQ_sublists,
                                               ubicacion_comp_AQ,
                                               respuestas_simuladas)

### this is the function for the attencion switch subscale of AQ
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ_atencion_switch.R"))

puntaje_AQ_sujetos_atencion_switch <- puntaje_AQ_atencion_switch(n_participants,
                                                                 n_AQ_sublists,
                                                                 ubicacion_comp_AQ,
                                                                 respuestas_simuladas)

### this is the function for the attencion to detail subscale of AQ
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ_atencion_detail.R"))

puntaje_AQ_sujetos_atencion_detail <- puntaje_AQ_atencion_detail(n_participants,
                                                                 n_AQ_sublists,
                                                                 ubicacion_comp_AQ,
                                                                 respuestas_simuladas)


### ### this is the function for the comunication subscale of AQ
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ_communication.R"))

puntaje_AQ_sujetos_communication <- puntaje_AQ_communication(n_participants,
                                                             n_AQ_sublists,
                                                             ubicacion_comp_AQ,
                                                             respuestas_simuladas)

### ### this is the function for the imagination subscale of AQ
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ_imagination.R"))

puntaje_AQ_sujetos_imagination <- puntaje_AQ_imagination(n_participants,
                                                         n_AQ_sublists,
                                                         ubicacion_comp_AQ,
                                                         respuestas_simuladas)

# add to df_NotExperimentData
# create a fake df_NotExperimentData
fake_df_NotExperimentData <- data.frame(
  participants = 1:n_participants,
  AQ = puntaje_AQ_sujetos,
  AQ_social = puntaje_AQ_sujetos_social,
  AQ_attentional_switches = puntaje_AQ_sujetos_atencion_switch,
  AQ_attention_detail = puntaje_AQ_sujetos_atencion_detail,
  AQ_communication = puntaje_AQ_sujetos_communication,
  AQ_imagination = puntaje_AQ_sujetos_imagination,
  stringsAsFactors = FALSE
)


write.csv(fake_df_NotExperimentData, "Big_pilot_4(dot_reference_first)/fake_df_NotExperimentData.csv", row.names=FALSE)

