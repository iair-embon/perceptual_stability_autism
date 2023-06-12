### get the AQ quotient communication subscale

puntaje_AQ_communication <- function (cant_sujetos,
                               cant_componentes_por_sujetos,
                               ubicacion_comp_AQ,
                               AQ){
  # cant_sujetos = cantidad de sujetos; cant_componentes_por_sujetos = cantidad de componentes en un sujeto;
  # ubicacion_comp_AQ = la ubicacion del componente AQ en el primer sujeto
  
  # items que cuentan para esta subescala y suman 1
  agree <- c(7,18,26,33,35,39)
  disagree <- c(17,27,31,38)
  
  # inicializo la variable que va a contener los puntajes del AQ para cada sujeto
  puntaje_AQ_sujetos <- rep(NA,cant_sujetos)
  
  # para cada sujeto, que saque el puntaje de AQ
  for (s in 1:cant_sujetos){
    
    # creo una variable con las respuestas del participante
    respuestas_AQ <- AQ[ubicacion_comp_AQ]$value
    
    # Modifico la ubicacion de las respuestas del AQ para el siguiente sujeto
    ubicacion_comp_AQ <- ubicacion_comp_AQ + cant_componentes_por_sujetos
    
    # inicializo variable que va a contener el puntaje del participante
    puntaje_AQ <- 0 
    
    # Saco el puntaje del participante de acuerdo a sus respuestas
    for (i in 1:length(respuestas_AQ)){
      if (i %in% agree){
        if(respuestas_AQ[i]== 1){
          puntaje_AQ <- puntaje_AQ + 1
        }else if (respuestas_AQ[i]== 2){
          puntaje_AQ <- puntaje_AQ + 1
        }
      }
      if (i %in% disagree){
        if(respuestas_AQ[i]==4){
          puntaje_AQ <- puntaje_AQ + 1
        }else if (respuestas_AQ[i]==3){
          puntaje_AQ <- puntaje_AQ + 1
        }
      }
    } 
    # Completo el puntaje del sujeto en su lugar correspondiente
    puntaje_AQ_sujetos[s] <- puntaje_AQ
  }
  # Le pido que me devuelva un vector con los puntajes de cada sujeto en orden 
  return(puntaje_AQ_sujetos)
}
