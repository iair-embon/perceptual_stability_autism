##################################################
# Metacognition and ASD experiment preprocessing #
##################################################

####### Read the .txt results from JATOS and perform the data preprocessing.

# (To reado the .txt results from JATOS it was necesary to add an "enter" before the curly brackets
# that open the component "sincericidio").

library(jsonlite)

# Read the .txt result
root <- rprojroot::is_rstudio_project
basename(getwd())

#read each line and convert

# exp 1.1
content<-readLines(root$find_file("jatos_results_20230515181739.txt"))

res<-lapply(content,fromJSON)


# each subject has 6 lists in order of arrival and by subjects.
# res[[1]] are the demographic data of subject 1
# res[[2]] are the data of the face-dot numeric estimation of subject 1
# res[[3]] are the AQ data of subject 1
# res[[4]] are the data of the browser of subject 1
# res[[4]] are the quality question and email data of subject 1
# res[[6]] are the demographic data of subject 2
# res[[7]] are the data of the face-dot numeric estimation of subject 1
# ....

iSub      <- 0
sleep <- c()
birthday  <- c()
country      <- c()
sex    <- c()
Studies   <- c()
AffectionPsycho <- c()
medication_1 <- c()
Estimation <- c()
Stimulus <- c()
Rt <- 
AQ      <- c()
Browser <- c()
QualityTest    <- c()
TeEscuchamos <- c()

# Experiment data frame
df_exp <- as.data.frame( res[[2]]) ## asi lo creo como df

for (s in 1:(length(res)-4)){
  
  ind_suenio <- NaN  
  ind_fecha <- NaN  
  ind_pais <- NaN  
  ind_genero <- NaN  
  ind_estudio <- NaN  
  ind_affeccion <- NaN  
  ind_medicacion <- NaN  
  
  for (item in 1:length(res[[s]])){
    if (is.null(res[[s]][item]$sueno)           ==FALSE){   ind_suenio <- item   }
    if (is.null(res[[s]][item]$Cumpleanos)      ==FALSE){   ind_fecha  <- item   }
    if (is.null(res[[s]][item]$Pais)            ==FALSE){   ind_pais   <- item   }
    if (is.null(res[[s]][item]$Genero)          ==FALSE){   ind_genero <- item   }
    if (is.null(res[[s]][item]$Estudio)         ==FALSE){   ind_estudio <- item   }
    if (is.null(res[[s]][item]$AffeccionPsico)  ==FALSE){   ind_affeccion <- item   }
    if (is.null(res[[s]][item]$medicacion)      ==FALSE){   ind_medicacion <- item   }
  }
  
  # Condition 1 will be TRUE if there is a response to the first component of demographic data
  condicion1 <-  is.nan(ind_suenio) == FALSE
  # Condition 2 will be TRUE if there is an answer to the AQ questions (component 4)
  condicion2 <-  is.null(res[[s+3]][1]$question) ==FALSE
  
  if(condicion1 & condicion2 ){ # new participant
    iSub <- iSub + 1;
    # I take data from component 1 (demographic)
    horasSuen <- c(horasSuen,res[[s]][ind_suenio]$sueno)
    fechaNac  <- c(fechaNac,res[[s]][ind_fecha]$Cumpleanos)
    pais <- c(pais, res[[s]][ind_pais]$Pais)
    genero <- c(genero,res[[s]][ind_genero]$Genero)
    estudio <- c(estudio,res[[s]][ind_estudio]$Estudio)
    affeccionPsico <- c(affeccionPsico,res[[s]][ind_affeccion]$AffeccionPsico)
    medicacion <- c(medicacion,res[[s]][ind_medicacion]$medicacion)
    
    # Experiment data 
    df_exp <- rbind(df_exp, res[[s+2]])
    
    # AQ data
    AQ <- c(AQ, res[[s+3]])  
    
    if(is.null(res[[s+4]][1]$browser) ==FALSE){
      Browser <- c(Browser, res[[s+4]][1]$browser)
    }else{
      Browser <- c(Browser, NaN)}
    
    if(length(res)-s >= 5 ){
      
      if(is.null(res[[s+5]][1]$sincericidio) ==FALSE){
        Sinc <- c(Sinc, res[[s+5]][1]$sincericidio)
      }else{
        Sinc <- c(Sinc, NaN)}
      
      if(is.null(res[[s+5]]$TeEscuchamos) ==FALSE){
        TeEscuchamos <- c(TeEscuchamos, res[[s+5]]$TeEscuchamos)
      }else{
        TeEscuchamos <- c(TeEscuchamos, NaN)}
    }
  }
}
####### df 

# df_DatosUnicos: for data of each subject.
# df_exp: save each trial of metacognition exp (already created in previous loop)

## df_DatosUnicos
sujetos <-  1:iSub
df_DatosUnicos <- data.frame(
  sujetos = sujetos, 
  horasSueno = horasSuen,
  fechaNac = fechaNac,
  pais = pais,
  genero = genero,
  estudio = estudio,
  affeccionPsico = affeccionPsico,
  medicacion = medicacion,
  Browser = Browser,
  sincericidio = Sinc,
  TeEscuchamos = TeEscuchamos,
  stringsAsFactors = FALSE
)
####################3 desde aca continua con el original








# load the function to read the .txt results from JATOS and create a dataframe
source(root$find_file("Analysis/AuxiliaryFunctions/initial_df.R"))
df_list <- initial_df(res)

# df_DatosUnicos: for data of each subject.
# df_exp: save each trial of metacognition exp (already created in previous loop)
df_DatosUnicos <- df_list$a
df_exp <- df_list$b
AQ <- df_list$c

# exp 1.2 + 1.3
content<-readLines(root$find_file("Data/Exp2+3/jatos_results_20210824144041.txt"))

res<-lapply(content,fromJSON)

df_list <- initial_df(res)
df <- df_list$a 
df$sujetos <- df$sujetos + 1000
df_DatosUnicos <- rbind(df_DatosUnicos, df) 
df_exp <- rbind(df_exp, df_list$b)
AQ <- c(AQ,df_list$c)

####### adding subjects and trials columns to df_exp

# get the number of trials per subject
cant_trials <- nrow(df_exp)/ nrow(df_DatosUnicos)

# prepare subject column to add in df_exp
sujetos <- rep(df_DatosUnicos$sujetos, each = cant_trials)

# prepare trials column
col_trials <- 1:cant_trials
trials <- rep(col_trials, times = nrow(df_DatosUnicos))

# add columns to df_exp
df_exp$sujetos <- sujetos
df_exp$trials <- trials

####### get the AQ quotient 

# number of AQ sublists for each subject 
cant_componentes_por_sujetos <- 2

# number of subject 
cant_sujetos <- nrow(df_DatosUnicos)

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
puntaje_AQ_sujetos <- puntaje_AQ_corregido(cant_sujetos,
                                           cant_componentes_por_sujetos,
                                           ubicacion_comp_AQ,
                                           AQ)
### this is the function for the social subscale of AQ 
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ_social.R"))

puntaje_AQ_sujetos_social <- puntaje_AQ_social(cant_sujetos,
                                           cant_componentes_por_sujetos,
                                           ubicacion_comp_AQ,
                                           AQ)

### this is the function for the attencion switch subscale of AQ
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ_atencion_switch.R"))

puntaje_AQ_sujetos_atencion_switch <- puntaje_AQ_atencion_switch(cant_sujetos,
                                               cant_componentes_por_sujetos,
                                               ubicacion_comp_AQ,
                                               AQ)

### this is the function for the attencion to detail subscale of AQ
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ_atencion_detail.R"))

puntaje_AQ_sujetos_atencion_detail <- puntaje_AQ_atencion_detail(cant_sujetos,
                                                                 cant_componentes_por_sujetos,
                                                                 ubicacion_comp_AQ,
                                                                 AQ)


### ### this is the function for the comunication subscale of AQ
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ_communication.R"))

puntaje_AQ_sujetos_communication <- puntaje_AQ_communication(cant_sujetos,
                                                                 cant_componentes_por_sujetos,
                                                                 ubicacion_comp_AQ,
                                                                 AQ)

### ### this is the function for the imagination subscale of AQ
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ_imagination.R"))

puntaje_AQ_sujetos_imagination <- puntaje_AQ_imagination(cant_sujetos,
                                                            cant_componentes_por_sujetos,
                                                            ubicacion_comp_AQ,
                                                            AQ)

# add to df_DatosUnicos
df_DatosUnicos$AQ <- puntaje_AQ_sujetos 
df_DatosUnicos$AQ_social <- puntaje_AQ_sujetos_social
df_DatosUnicos$AQ_atencion_switch <- puntaje_AQ_sujetos_atencion_switch
df_DatosUnicos$AQ_atencion_detail <- puntaje_AQ_sujetos_atencion_detail
df_DatosUnicos$AQ_communication <- puntaje_AQ_sujetos_communication
df_DatosUnicos$AQ_imagination <- puntaje_AQ_sujetos_imagination

####### Adding columns of

## Reaction Times
df_exp_mod <- df_exp

df_exp_mod$t_ensayo_discriminacion <- df_exp_mod$discrimination_t_keydown - 
  df_exp_mod$discrimination_t_onset
df_exp_mod$t_ensayo_confianza <- df_exp_mod$confidence_t_keydown -
  df_exp_mod$confidence_t_onset

## Percentage of correct answers

Pc   <- rep(NA, nrow(df_DatosUnicos)) 
existing_subjects <- unique(df_DatosUnicos$sujetos)
for (s in 1:nrow(df_DatosUnicos)){
  Pc[s]   <- mean(df_exp_mod$discrimination_is_correct[df_exp_mod$sujetos== existing_subjects[s]])
}

# add to df_DatosUnicos
df_DatosUnicos$PC <- Pc

# add difference in dots in every trial to df_exp
df_exp_mod$diferencia_puntitos <- abs(df_exp_mod$dots_num_left- df_exp_mod$dots_num_right)

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
  cant_trials <- nrow(sujeto_df_exp)
  
  sujeto_df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[df_DatosUnicos_mod2$sujetos== ExistingSubjects[i],]
  df <- as.data.frame(lapply(sujeto_df_DatosUnicos_mod2, rep, cant_trials))
  
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
cant_trials_por_sujeto <- rep(NaN, length(unique(df_total$sujetos)))
existing_subject <- unique(df_total$sujetos)

for (i in 1:length(cant_trials_por_sujeto)) {
  cant_trials_por_sujeto[i] <- nrow(df_total[df_total$sujetos == existing_subject[i],])
}

# I see who are the ones who have fewer trials than X
indices_cant_trials <- which(cant_trials_por_sujeto < 90)
subj_pocos_trials<- existing_subject[indices_cant_trials]

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
  cant_trials <- nrow(sujeto_df_exp)
  
  sujeto_mc <-rep(mc[i],cant_trials)
  
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
