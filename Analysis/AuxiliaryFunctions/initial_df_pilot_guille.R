# Read the .txt results from JATOS and create a dataframe
initial_df <- function(res){
  
# each subject has 5 lists in order of arrival and by subjects.
# res[[1]] are the demographic data of subject 1
# res[[2]] are the data of the face-dot numeric estimation of subject 1
# res[[3]] are the AQ data of subject 1
# res[[4]] are the data of the face-dot numeric estimation of subject 1
# res[[5]] are the data of the broweser of subject 1
# res[[6]] are the quality question and email data of subject 1
# res[[7]] are the demographic data of subject 2
# res[[8]] are the data of the face-dot numeric estimation of subject 1
# ....

iSub      <- 0
prolific <- c()
sleep <- c()
birthday  <- c()
country  <- c()
sex    <- c()
Studies   <- c()
AffectionPsycho_1 <- c()
AffectionPsycho_2 <- c()
medication_1 <- c()
medication_2 <- c()
AQ      <- c()
Browser <- c()
QualityTest  <- c()
Problems <- c()

for (s in 1:(length(res)-5)){
  
  ind_prolific <- NaN
  ind_sleep <- NaN  
  ind_birthday <- NaN  
  ind_country <- NaN  
  ind_sex <- NaN  
  ind_Studies <- NaN  
  ind_AffectionPsycho_1 <- NaN  
  ind_AffectionPsycho_2 <- NaN  
  ind_medication_1 <- NaN
  ind_medication_2 <- NaN
  
  for (item in 1:length(res[[s]])){
    if (is.null(res[[s]][item]$prolific)           ==FALSE){   ind_prolific <- item   }
    if (is.null(res[[s]][item]$sleep)           ==FALSE){   ind_sleep <- item   }
    if (is.null(res[[s]][item]$birthday)      ==FALSE){   ind_birthday   <- item   }
    if (is.null(res[[s]][item]$country)            ==FALSE){   ind_country  <- item   }
    if (is.null(res[[s]][item]$sex)          ==FALSE){   ind_sex <- item   }
    if (is.null(res[[s]][item]$Studies)         ==FALSE){   ind_Studies <- item   }
    if (is.null(res[[s]][item]$AffectionPsycho_1)  ==FALSE){   ind_AffectionPsycho_1 <- item   }
    if (is.null(res[[s]][item]$AffectionPsycho_2)  ==FALSE){   ind_AffectionPsycho_2 <- item   }
    if (is.null(res[[s]][item]$medication_1)      ==FALSE){   ind_medication_1 <- item   }
    if (is.null(res[[s]][item]$medication_2)      ==FALSE){   ind_medication_2 <- item   }
  }
  
  # Condition 1 will be TRUE if there is a response to the first component of demographic data
  condition1 <-  is.nan(ind_sleep) == FALSE
  #if(is.nan(ind_sleep) == TRUE){print(s)}
  # Condition 2 will be TRUE if there is an answer to the AQ questions (component 3)
  condition2 <-  is.null(res[[s+5]]$QualityTest) ==FALSE
  
  if(condition1 & condition2 ){ # new participant
    if(!(res[[s]][ind_prolific]$prolific %in% prolific)){
      iSub <- iSub + 1;
      # I take data from component 1 (demographic)
      prolific <- c(prolific,res[[s]][ind_prolific]$prolific)
      sleep <- c(sleep,res[[s]][ind_sleep]$sleep)
      birthday  <- c(birthday,res[[s]][ind_birthday]$birthday)
      country <- c(country, res[[s]][ind_country]$country)
      sex <- c(sex,res[[s]][ind_sex]$sex)
      Studies <- c(Studies,res[[s]][ind_Studies]$Studies)
      AffectionPsycho_1 <- c(AffectionPsycho_1,res[[s]][ind_AffectionPsycho_1]$AffectionPsycho_1)
      if(!is.nan(ind_AffectionPsycho_2)){
        AffectionPsycho_2 <- c(AffectionPsycho_2,res[[s]][ind_AffectionPsycho_2]$AffectionPsycho_2)
      }else{
        AffectionPsycho_2 <-c(AffectionPsycho_2,NaN) 
      }
      medication_1 <- c(medication_1,res[[s]][ind_medication_1]$medication_1)
      if(!is.nan(ind_medication_2)){
        medication_2 <- c(medication_2,res[[s]][ind_medication_2]$medication_2)
      }else{
        medication_2 <-c(medication_2,NaN)
      }
      
      # Experiment data 
      if(s<3){
        df_exp <- as.data.frame(res[[2]])
        df_exp2 <- as.data.frame(res[[4]])
        }else{
          df_exp <- rbind(df_exp, res[[s+1]]) 
          df_exp2 <- rbind(df_exp2, res[[s+3]])
        }
      
      # AQ data
      AQ <- c(AQ, res[[s+2]])  
      
      if(is.null(res[[s+4]][1]$browser) ==FALSE){
        Browser <- c(Browser, res[[s+4]][1]$browser)
      }else{
        Browser <- c(Browser, NaN)}
      
      if(length(res)-s >= 5 ){
        
        if(is.null(res[[s+5]][1]$QualityTest) ==FALSE){
          QualityTest <- c(QualityTest, res[[s+5]][1]$QualityTest)
        }else{
          QualityTest <- c(QualityTest, NaN)}
        
        if(is.null(res[[s+5]]$Problems) ==FALSE){
          Problems <- c(Problems, res[[s+5]]$Problems)
        }else{
          Problems <- c(Problems, NaN)}
      }
    }
  }
}
####### df 

# birthday to age
date <- as.Date(birthday)
age <- as.numeric(difftime(Sys.Date(), date, units = "weeks") / 52.1775)

# df_NotExperimentData: for data of each subject.
# df_exp: save each trial of metacognition exp (already created in previous loop)

## df_NotExperimentData
participants <-  1:iSub
df_NotExperimentData <- data.frame(
  participants = participants,
  prolific = prolific,
  sleep = sleep,
  birthday = birthday,
  age = age,
  country = country,
  sex = sex,
  Studies = Studies,
  AffectionPsycho_1 = AffectionPsycho_1,
  AffectionPsycho_2 = AffectionPsycho_2,
  medication_1 = medication_1,
  medication_2 = medication_2,
  Browser = Browser,
  QualityTest = QualityTest,
  Problems = Problems
)

  # add the position column, which stimulus was presented first, and type (morph, many)
  df_exp$position <- rep(1,nrow(df_exp))
  df_exp$type <- rep("morph",nrow(df_exp))
  df_exp2$position <- rep(2,nrow(df_exp2))
  df_exp2$type <- rep("many",nrow(df_exp2))
  
  # add participant column
  df_exp$participant <- rep(participants,each=(nrow(df_exp)/iSub))
  df_exp2$participant <- rep(participants,each=(nrow(df_exp2)/iSub))
  
  df_exp <- rbind(df_exp, df_exp2)   

  df_list <- list(a = df_NotExperimentData, b = df_exp, c = AQ)
  return(df_list)
}