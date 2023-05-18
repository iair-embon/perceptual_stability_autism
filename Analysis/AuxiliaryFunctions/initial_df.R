# Read the .txt results from JATOS and create a dataframe
initial_df <- function(res){
  
  # each subject has 6 lists in order of arrival and by subjects.
  # res[[1]] are the demographic data of subject 1
  # res[[2]] are the data of the practice of the experiment of subject 1
  # res[[3]] are the data from subject 1's experiment
  # res[[4]] are the AQ data of subject 1
  # res[[4]] are the data of the browser of subject 1
  # res[[6]] are the email data of subject 1
  # res[[7]] are the demographic data of subject 2
  # res[[8]] are the data of the practice of the experiment of subject 2
  # ....
  
  iSub      <- 0
  horasSuen <- c()
  fechaNac  <- c()
  pais      <- c()
  genero    <- c()
  estudio   <- c()
  affeccionPsico <- c()
  medicacion <- c()
  AQ      <- c()
  Browser <- c()
  Sinc    <- c()
  TeEscuchamos <- c()
  
  # Experiment data frame
  df_exp <- data.frame(t0 =character(), 
                       t_offset =character(), 
                       dots_num_left =character(), 
                       dots_num_right =character(), 
                       discrimination_is_correct =character(), 
                       discrimination_t_onset =character(), 
                       discrimination_t_keydown =character(), 
                       confidence_key =character(), 
                       confidence_t_onset=character(), 
                       confidence_t_keydown=character(), 
                       stringsAsFactors=FALSE) 
  
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
  
  
  df_list <- list(a = df_DatosUnicos, b = df_exp, c = AQ)
  return(df_list)
}