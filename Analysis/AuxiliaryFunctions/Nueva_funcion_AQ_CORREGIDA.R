### get the AQ quotient

puntaje_AQ_corregido <- function (n_participants,
                                  n_AQ_sublists,
                        ubicacion_comp_AQ,
                        AQ){
  # n_participants = cantidad de sujetos; n_AQ_sublists = cantidad de componentes en un sujeto;
  # ubicacion_comp_AQ = la ubicacion del componente AQ en el primer sujeto
  
  # intems que suman uno con agree
  agree <- c(1,2,4,5,6,7,9,12,13,16,18,19,20,21,22,23,26,33,35,39,41,42,43,45,46)
  # items que suman uno con disagree
  disagree <- c(3,8,10,11,14,15,17,24,25,27,28,29,30,31,32,34,36,37,38,40,44,47,48,49,50)
  
  # inicializo la variable que va a contener los puntajes del AQ para cada sujeto
  puntaje_AQ_sujetos <- rep(NA,n_participants)
  
  # para cada sujeto, que saque el puntaje de AQ
  for (s in 1:n_participants){
    
    # creo una variable con las respuestas del participante
    respuestas_AQ <- AQ[ubicacion_comp_AQ]$value
    
    # Modifico la ubicacion de las respuestas del AQ para el siguiente sujeto
    ubicacion_comp_AQ <- ubicacion_comp_AQ + n_AQ_sublists
    
    # inicializo variable que va a contener el puntaje del participante
    puntaje_AQ <- 0 
    
    # Saco el puntaje del participante de acuerdo a sus respuestas. (hasta el indice 25 (incluido) total acuerdo 
    # y acuerdo parcial puntea 1. Desde el indice 26 hasta el 40, desacuerdo total y desacuerdo parcial puntea 1
    for (i in 1:length(respuestas_AQ)){
      if (i %in% agree){
        if(respuestas_AQ[i]== 1){
          puntaje_AQ <- puntaje_AQ + 1
        }else if (respuestas_AQ[i]== 2){
          puntaje_AQ <- puntaje_AQ + 1
        }
      }else {
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

