#######################################################
# Perceptual stability - ASD experiment preprocessing #
#######################################################

####### Read the .txt results from JATOS and perform the data preprocessing.

# (To reado the .txt results from JATOS it was necesary to add an "enter" before the curly brackets
# that open the component "sincericidio").

library(jsonlite)

# Read the .txt result
root <- rprojroot::is_rstudio_project
basename(getwd())

#read each line and convert

# data
content<-readLines(root$find_file("pilot/jatos_results_20230605155613.txt"))
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

################## aca filtrar lo que no me interesa del df_exp
library(dplyr)

# first of all, I filter by columns of interest. Then I filter by rows of interest.
df_exp_filtered <- df_exp %>%
  # columns
  select(rt, stimulus, response, trial_type, trial_index) %>%
  # rows
  filter(trial_index %in% c(5,6,32,34,35,41,43,44,50))

# add participants index column

n_rows <- nrow(df_exp_filtered)/ nrow(df_NotExperimentData)
participants <- rep(df_NotExperimentData$participants, each = n_rows)
df_exp_filtered$participants <- participants

# reorder face videos values and dots labels

for (i in 1:nrow(df_NotExperimentData)) {
  if(i ==1){
    d <- df_exp_filtered %>%
      filter(participants==i) %>%
      mutate(stimulus = case_when(row_number() == 2 ~ as.character(.$stimulus[1]),
                                  row_number() == 3 ~ "dot_40_60",
                                  row_number() == 5 ~ as.character(.$stimulus[4]),
                                  row_number() == 6 ~ "dot_40",
                                  row_number() == 8 ~ as.character(.$stimulus[7]),
                                  row_number() == 9 ~ "dot_60"),
             response = case_when(row_number() == 2 ~ as.numeric(.$response[[2]]$Estimation),
                                  row_number() == 3 ~ as.numeric(.$response[[3]]$Estimation),
                                  row_number() == 5 ~ as.numeric(.$response[[5]]$Estimation),
                                  row_number() == 6 ~ as.numeric(.$response[[6]]$Estimation),
                                  row_number() == 8 ~ as.numeric(.$response[[8]]$Estimation),
                                  row_number() == 9 ~ as.numeric(.$response[[9]]$Estimation)))
  }else{
    d_mod <- df_exp_filtered %>%
      filter(participants==i) %>%
      mutate(stimulus = case_when(row_number() == 2 ~ as.character(.$stimulus[1]),
                                  row_number() == 3 ~ "dot_40_60",
                                  row_number() == 5 ~ as.character(.$stimulus[4]),
                                  row_number() == 6 ~ "dot_40",
                                  row_number() == 8 ~ as.character(.$stimulus[7]),
                                  row_number() == 9 ~ "dot_60"),
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
  filter(trial_index %in% c(6,32,35,41,44,50)) %>%
  # columns
  select(-trial_type, -trial_index)

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
puntaje_AQ_sujetos <- puntaje_AQ_corregido(n_participants,
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


<<<<<<< HEAD
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

######## pruebo meter todo a lo largo  
=======
prolific_info<-read.csv("pilot/prolific_export_05062023_40_participants.csv") 
prolific_info$Participant.id %in%  df_NotExperimentData$prolific
df_NotExperimentData$prolific %in% prolific_info$Participant.id
>>>>>>> 9a32a3bf110af3a6d4086cc73f458cd565ad1947

##### empiezo a chusmear algunos analisis porque si
fs2yo <- df_exp_filtered_mod %>%
  filter(stimulus== "videos/fs2yo.mp4")

fs2o <- df_exp_filtered_mod %>%
  filter(stimulus== "videos/fs2o.mp4")

fs2y <- df_exp_filtered_mod %>%
  filter(stimulus== "videos/fs2y.mp4")

fs1yo <- df_exp_filtered_mod %>%
  filter(stimulus== "videos/fs1yo.mp4")

fs1o <- df_exp_filtered_mod %>%
  filter(stimulus== "videos/fs1o.mp4")

fs1y <- df_exp_filtered_mod %>%
  filter(stimulus== "videos/fs1y.mp4")

dot_40_60 <- df_exp_filtered_mod %>%
  filter(stimulus== "dot_40_60")

dot_40 <- df_exp_filtered_mod %>%
  filter(stimulus== "dot_40")

dot_60 <- df_exp_filtered_mod %>%
  filter(stimulus== "dot_60")

fsyo <- rbind(fs2yo, fs1yo)
fso <- rbind(fs1o, fs2o)
fsy <- rbind(fs2y, fs1y)

mean(dot_40_60$response)
mean(dot_40$response)
mean(dot_60$response)

mean(fsyo$response)
mean(fso$response)
mean(fsy$response)

sd(dot_40_60$response)
sd(dot_40$response)
sd(dot_60$response)

sd(fsyo$response)
sd(fso$response)
sd(fsy$response)

write.csv(df_exp_filtered_mod, "pilot/df_exp_filtered_mod.csv", row.names=FALSE)
write.csv(df_NotExperimentData, "pilot/df_NotExperimentData.csv", row.names=FALSE)

######################################################################################### HASTA ACA

####### Unifying the format of columns values
# (horaSueno, medicacion affeccionPsico, TeEscuchamos from df_DatosUnicos)

# If the script does not recognize a certain value, it asks the user for help with it 
# (keep an eye on the console). In this case, the user must write the corresponding value 
# that is requested.
# The script saves the response in a df that the script will review the next time it is run.
# The script returns the df_DatosUnicos_mod that will have all the column values in the same format.
# In addition, in this part, the age column of the participants is created and filled
# in from their date of birth.
# It is recommended to run the script by column to unify.

## column: horaSueno

# df that will have the data in a unified and readable format.
# The values in the hoursSleep column are converted to numeric. If not possible in NA
df_DatosUnicos_mod <- transform(df_DatosUnicos, 
                                horasSueno = as.numeric(as.character(horasSueno)))

# the corresponding function is loaded
source(root$find_file("Analysis/AuxiliaryFunctions/numeriza_col_horasSuenos.R"))

df_DatosUnicos_mod <- numeriza_col_horasSuenos(df_DatosUnicos_mod,df_DatosUnicos)

## column: edad

# save the age of the subjects in df_DatosUnicos_mod
library(eeptools)
is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x),
                                                     tz = 'UTC', format = '%Y-%m-%d'))

edad <- rep(NA, nrow(df_DatosUnicos_mod))

for(i in 1:nrow(df_DatosUnicos_mod)){
  if(sapply(df_DatosUnicos_mod$fechaNac[i], is.convertible.to.date)){
    a <- as.Date( df_DatosUnicos_mod$fechaNac[i])
    age <- as.integer( age_calc(a, units='years') )
    edad[i] <- age
  }else{ # if it is not possible to obtain the age it is also a NA
    edad[i] <- NA
  }
}

df_DatosUnicos_mod$edad <- edad

## column: medicacion

# the corresponding function is loaded
source(root$find_file("Analysis/AuxiliaryFunctions/unifica_col_medicacion.R"))

# converts the medication values in: Si, No , noSabe
df_DatosUnicos_mod <- unifica_col_medicacion(df_DatosUnicos_mod,df_DatosUnicos)

## column: affeccionPsico

# the corresponding function is loaded
source(root$find_file("Analysis/AuxiliaryFunctions/unifica_col_affeccionPsico.R"))

# converts the affeccionPsico values in Si, No , noSabe
df_DatosUnicos_mod <- unifica_col_affeccionPsico(df_DatosUnicos_mod,df_DatosUnicos)

## columna: TeEscuchamos

# the corresponding function is loaded
source(root$find_file("Analysis/AuxiliaryFunctions/unifica_col_TeEscuchamos.R"))

# converts the TeEscuchamos values in Si, No , noSabe
df_DatosUnicos_mod <- unifica_col_TeEscuchamos(df_DatosUnicos_mod,df_DatosUnicos)

####### Add the confidence columns to df_DatosUnicos_mod

# Confidence columns for all the subjects
confidence_key_1 <- rep(NA, nrow(df_DatosUnicos_mod))
confidence_key_2 <- rep(NA, nrow(df_DatosUnicos_mod))
confidence_key_3 <- rep(NA, nrow(df_DatosUnicos_mod))
confidence_key_4 <- rep(NA, nrow(df_DatosUnicos_mod))

ExistingSubjects <- unique(df_exp_mod$sujetos)

for(i in 1:nrow(df_DatosUnicos_mod)){
  # confidence columns are created to iterate by subject
  confidence_key_1_total <- 0
  confidence_key_2_total <- 0
  confidence_key_3_total <- 0
  confidence_key_4_total <- 0
  
  df_prueba <- df_exp_mod[df_exp_mod$confidence_key =='1',]
  confidence_key_1_total <- nrow(df_prueba[df_prueba$sujetos== ExistingSubjects[i],])
  confidence_key_1[i] <- confidence_key_1_total
  
  df_prueba <- df_exp_mod[df_exp_mod$confidence_key =='2',]
  confidence_key_2_total <- nrow(df_prueba[df_prueba$sujetos==ExistingSubjects[i],])
  confidence_key_2[i] <- confidence_key_2_total
  
  df_prueba <- df_exp_mod[df_exp_mod$confidence_key =='3',]
  confidence_key_3_total <- nrow(df_prueba[df_prueba$sujetos==ExistingSubjects[i],])
  confidence_key_3[i] <- confidence_key_3_total
  
  df_prueba <- df_exp_mod[df_exp_mod$confidence_key =='4',]
  confidence_key_4_total <- nrow(df_prueba[df_prueba$sujetos==ExistingSubjects[i],])
  confidence_key_4[i] <- confidence_key_4_total
}

# Add the columns to df_DatosUnicos_mod
df_DatosUnicos_mod$confidence_key_1 <- confidence_key_1
df_DatosUnicos_mod$confidence_key_2 <- confidence_key_2
df_DatosUnicos_mod$confidence_key_3 <- confidence_key_3
df_DatosUnicos_mod$confidence_key_4 <- confidence_key_4

## Get the sd and mean of confidence by subject
media_confidence <- rep(NA, nrow(df_DatosUnicos_mod))
sd_confidence <- rep(NA, nrow(df_DatosUnicos_mod))

for(i in 1:nrow(df_DatosUnicos_mod)){
  media_confidence[i] <- mean(df_exp_mod[df_exp_mod$sujetos==ExistingSubjects[i],"confidence_key"])
  sd_confidence[i] <- sd(df_exp_mod[df_exp_mod$sujetos==ExistingSubjects[i],"confidence_key"])
}

df_DatosUnicos_mod$media_confidence <- media_confidence
df_DatosUnicos_mod$sd_confidence <- sd_confidence

####### Get the sd and mean of reaction times by subject in the discrimination task
media_tr_discri <- rep(NA, nrow(df_DatosUnicos_mod))
sd_tr_discri <- rep(NA, nrow(df_DatosUnicos_mod))

for(i in 1:nrow(df_DatosUnicos_mod)){
  media_tr_discri[i] <- mean(df_exp_mod[df_exp_mod$sujetos==ExistingSubjects[i],"t_ensayo_discriminacion"])
  sd_tr_discri[i] <- sd(df_exp_mod[df_exp_mod$sujetos==ExistingSubjects[i],"t_ensayo_discriminacion"])
}

df_DatosUnicos_mod$media_tr_discri <- media_tr_discri
df_DatosUnicos_mod$sd_tr_discri <- sd_tr_discri

####### get the sd and mean of reaction times by subject in the confidence task
media_tr_confi <- rep(NA, nrow(df_DatosUnicos_mod))
sd_tr_confi <- rep(NA, nrow(df_DatosUnicos_mod))

for(i in 1:nrow(df_DatosUnicos_mod)){
  media_tr_confi[i] <- mean(df_exp_mod[df_exp_mod$sujetos==ExistingSubjects[i],"t_ensayo_confianza"])
  sd_tr_confi[i] <- sd(df_exp_mod[df_exp_mod$sujetos==ExistingSubjects[i],"t_ensayo_confianza"])
}

df_DatosUnicos_mod$media_tr_confi <- media_tr_confi
df_DatosUnicos_mod$sd_tr_confi <- sd_tr_confi

####### Inclusion criteria, data is not included in future analysis
## Comment / uncomment or modify filters as required

## Filter for hours of sleep, leaving me only with > 4
#df_DatosUnicos_mod2 <- df_DatosUnicos_mod[df_DatosUnicos_mod$horasSueno > 4,] 

cat("Cantidad de sujetos antes de todo filtro: ", nrow(df_DatosUnicos_mod))

## Filter by psychological disorder, staying only with those who do not have.
df_DatosUnicos_mod2 <- df_DatosUnicos_mod[df_DatosUnicos_mod$affeccionPsico ==
                                               'No',]

cat("Cantidad de sujetos luego de filtrar por trastorno psi: ", nrow(df_DatosUnicos_mod2))

## Filter by medication, leaving only with those who do not take.
df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[df_DatosUnicos_mod2$medicacion ==
                                               'No',]
cat("Cantidad de sujetos luego de filtrar por medicacion: ", nrow(df_DatosUnicos_mod2))

## Filter by age, leaving only those who are age > 17, < 100, and are not NA
df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[df_DatosUnicos_mod2$edad > 17,]
df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[df_DatosUnicos_mod2$edad < 100,]
df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[!is.na(df_DatosUnicos_mod2$edad),]

cat("Cantidad de sujetos luego de filtrar por edad: ", nrow(df_DatosUnicos_mod2))
## filter in df_exp those who survived inclusion criteria applied to 
## df_DatosUnicos_mod2
library(dplyr)
df_exp_mod2 <- df_exp_mod %>% 
  filter(df_exp_mod$sujetos %in% df_DatosUnicos_mod2$sujetos)

####### putting it all together 

df_total <- df_DatosUnicos_mod2[0,]

#  existing subjects
ExistingSubjects <- unique(df_exp_mod2$sujetos)

for (i in 1:length(ExistingSubjects)){
  
  sujeto_df_exp <- df_exp_mod2[df_exp_mod2$sujetos== ExistingSubjects[i],]
  n_trials <- nrow(sujeto_df_exp)
  
  sujeto_df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[df_DatosUnicos_mod2$sujetos== ExistingSubjects[i],]
  df <- as.data.frame(lapply(sujeto_df_DatosUnicos_mod2, rep, n_trials))
  
  df_total <- rbind(df_total, df)
}

# combino las columnas de df_exp_mod2 que me interesan con el df_total
df_total <- cbind(df_total, 
                  dots_num_left = df_exp_mod2$dots_num_left,
                  dots_num_right = df_exp_mod2$dots_num_right,
                  df_total, discrimination_is_correct = df_exp_mod2$discrimination_is_correct,
                  confidence_key = df_exp_mod2$confidence_key, 
                  trials = df_exp_mod2$trials,
                  diferencia_puntitos = df_exp_mod2$diferencia_puntitos, 
                  t_ensayo_discriminacion = df_exp_mod2$t_ensayo_discriminacion,
                  t_ensayo_confianza = df_exp_mod2$t_ensayo_confianza
                  )

####### save the df_total

# # RESULTS_EXP
filepath <- root$find_file("Data/All_exp_inclusion_criteria/df_total.Rda")
save(df_total,file = filepath)


####### Exclusion criteria, data is excluded of future analysis

cat("Cantidad de sujetos antes de filtrar por criterios de exclusion: ", length(unique(df_total$sujetos)))

## Filter by sincericide, leaving only those who tell us that we can count on their answers.
library (stringr)
library (tidyverse)
df_total <- df_total %>% 
  filter(str_detect(df_total$sincericidio, "Pueden")) # if start with "Pueden"
#                                                                  # it stays

cat("Cantidad de sujetos luego de filtrar por sincericidio: ", length(unique(df_total$sujetos)))

## Filter by TeEscuchamos leaving only those who did not interrup the 
# task drastically (= ok)
df_total <- df_total[df_total$TeEscuchamos == 'ok',] 

cat("Cantidad de sujetos luego de filtrar por te escuchamos: ", length(unique(df_total$sujetos)))

## Filter by performance, leaving only those who have PC > 60 
df_total <- df_total[df_total$PC > 0.60,]

cat("Cantidad de sujetos luego de filtrar por desempeno: ", length(unique(df_total$sujetos)))

## sujetos que tienen un 85 % de trials en una misma respuesta de confianza
source(root$find_file("Analysis/AuxiliaryFunctions/discard_by_x_same_confidence_new.R"))
sujetos_a_descartar <- discard_by_x_same_confidence_new(85,df_total)  
df_total <- df_total[! df_total$sujetos %in% sujetos_a_descartar,]

cat("Cantidad de sujetos luego de filtrar por X% de trials con la misma confianza: ", length(unique(df_total$sujetos)))

cat("Cantidad de trials antes de filtrar por RT: ", nrow(df_total))
## Filter by reaction times
df_total <- df_total[df_total$t_ensayo_discriminacion <= 5000,]
cat("Cantidad de trials luego de filtrar por <5000 en tarea t1: ", nrow(df_total))
df_total <- df_total[df_total$t_ensayo_discriminacion >= 200,]
cat("Cantidad de trials luego de filtrar por >200 en tarea t1: ", nrow(df_total))
df_total <- df_total[df_total$t_ensayo_confianza <=5000,]
cat("Cantidad de trials luego de filtrar por <5000 en tarea t2: ", nrow(df_total))
df_total <- df_total[df_total$t_ensayo_confianza >=0,] 
cat("Cantidad de trials luego de filtrar por >0 en tarea t2: ", nrow(df_total))

## burning the first 20 trials of each subject
df_total <- df_total[df_total$trials > 20,]
cat("Cantidad de trials luego de quemar los primeros 20 trials: ", nrow(df_total))

## Filter by trails needed to calculate AUROC2
## discarding because very few trials
n_trials_por_sujeto <- rep(NaN, length(unique(df_total$sujetos)))
existing_subject <- unique(df_total$sujetos)

for (i in 1:length(n_trials_por_sujeto)) {
  n_trials_por_sujeto[i] <- nrow(df_total[df_total$sujetos == existing_subject[i],])
}

# I see who are the ones who have fewer trials than X
indices_n_trials <- which(n_trials_por_sujeto < 90)
subj_pocos_trials<- existing_subject[indices_n_trials]

# I discard them
df_total <- df_total[! df_total$sujetos %in% subj_pocos_trials,]

cat("Cantidad de sujetos luego de filtrar por trials insuficientes para calcular AUROC2: ", length(unique(df_total$sujetos)))

####### AUROC2
## get metacognitive sensivity

# load the type 2 ROC analysis function
source(root$find_file("Analysis/AuxiliaryFunctions/auroc2.R"))
library(dplyr)

Nsuj <- length(unique(df_total$sujetos))
# saving metacog = mc for each RT discarded
mc <- rep(NA, Nsuj)
ExistingSubjects <- unique(df_total$sujetos)

for (i in 1:Nsuj){
  mc[i] <- type2roc(correct = df_total$discrimination_is_correct[df_total$sujetos==ExistingSubjects[i]],
                    conf = df_total$confidence_key[df_total$sujetos==ExistingSubjects[i]], 
                    Nratings = 4)}

## adding column mc to df_total

todos_sujetos_mc <- c()

for (i in 1:length(ExistingSubjects)) {
  
  sujeto_df_exp <- df_total[df_total$sujetos == ExistingSubjects[i],]
  n_trials <- nrow(sujeto_df_exp)
  
  sujeto_mc <-rep(mc[i],n_trials)
  
  todos_sujetos_mc <- c(todos_sujetos_mc,sujeto_mc)
}

df_total$mc <- todos_sujetos_mc

####### filter those who have an AUROC2 less than 1.5 standard deviations from the mean
mean_mc <- mean(mc)
sd_mc <-sd(mc)
df_total <- df_total[df_total$mc >= mean_mc - sd_mc* 1.5,]

cat("Cantidad de sujetos luego de filtrar por AUROC2: ", length(unique(df_total$sujetos)))

####### save the df_total

# # RESULTS all exp
filepath <- root$find_file("Data/All_exp_exclusion_criteria/df_total.Rda")
save(df_total,file = filepath)

# # RESULTS_EXP1
#filepath <- root$find_file("Data/Results_Exp1/df_total.Rda")
#save(df_total,file = filepath)

# RESULTS_EXP2(REPLICA)
#filepath <- root$find_file("Data/Results_Exp2(replica)/df_total.Rda")
#save(df_total,file = filepath)

# # RESULTS_EXP2+3
#filepath <- root$find_file("Data/Exp2+3/df_total.Rda")
#save(df_total,file = filepath)
