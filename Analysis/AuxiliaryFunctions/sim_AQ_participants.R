# función que simula respuestas random en el AQ para cada participante

sim_AQ_participants <- function(n_s) {
  simulated_responses <- vector("list", length = (n_s*2))
  for (i in 1:(n_s*2)) {
    if (i %% 2 != 0) {
      simulated_responses[[i]] <- paste0("Question_", 1:50)
      names(simulated_responses)[i] <- "question"
    } else {
      simulated_responses[[i]] <- as.character(sample(1:4, 50, replace = TRUE))
      names(simulated_responses)[i] <- "value"
    }
  }
  return(simulated_responses)
}
