# Read the .txt results from JATOS and create a dataframe
initial_df <- function(res){
  
# each subject has 5 lists in order of arrival and by subjects.
# res[[1]] are the demographic data of subject 1
# res[[2]] are the data of the dot numeric estimation for reference of subject 1
# res[[3]] confidence to the reference response of subject 1
# res[[4]] are the AQ data of subject 1
# res[[5]] are the data of the dot numeric estimation for morph of subject 1
# res[[6]] confidence to the morph response of subject 1
# res[[7]] are the data of the broweser of subject 1
# res[[8]] are the quality question and email data of subject 1
  # next subject
# res[[9]] are the demographic data of subject 2
# res[[10]] are the data of the dot numeric estimation for reference of subject 2

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
estimation_reference_pos1 <- c()
estimation_morph_pos2 <- c()
confidence_1 <- c()
confidence_2 <- c()
Browser <- c()
QualityTest  <- c()
Problems <- c()

for (s in 1:(length(res)-6)){
  
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
  
  # this condition for the real experiment
  # condition2 <-  is.null(res[[s+7]]$QualityTest) ==FALSE
  
  # this condition for pilot study
  condition2 <-  is.null(res[[s+5]]$confidence) ==FALSE
  
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
        estimation_reference_pos1 <- c(estimation_reference_pos1, df_exp$response[[10]]$Estimation)      
        df_exp2 <- as.data.frame(res[[5]])
        estimation_morph_pos2 <- c(estimation_morph_pos2, df_exp2$response[[36]]$Estimation)
        }else{
          df_exp <- as.data.frame(res[[s+1]])
          estimation_reference_pos1 <- c(estimation_reference_pos1, df_exp$response[[10]]$Estimation)
          
          df_exp2 <- as.data.frame(res[[s+4]]) 
          estimation_morph_pos2 <- c(estimation_morph_pos2, df_exp2$response[[36]]$Estimation)
        }
      
      # confidence
      if(length(res[[s+2]]$confidence> 1)){
        conf_1 <- res[[s+2]]$confidence
        confidence_1 <- c(confidence_1, conf_1[1])
      }else{
        confidence_1 <- c(confidence_1, res[[s+2]]$confidence)
      }
      
      if(length(res[[s+5]]$confidence> 1)){
        conf_2 <- res[[s+5]]$confidence
        confidence_2 <- c(confidence_2, conf_2[1])
      }else{
        confidence_2 <- c(confidence_2, res[[s+5]]$confidence)
      }
      
#      if(iSub == 13){
#        print(conf_1[1])
#        print(conf_1)
#        print(conf_2[1])
#        print(conf_2)
#      }
      
      # AQ data
      AQ <- c(AQ, res[[s+3]])  
      
      if(is.null(res[[s+6]][1]$browser) ==FALSE){
        Browser <- c(Browser, res[[s+6]][1]$browser)
      }else{
        Browser <- c(Browser, NaN)}
      
      if(length(res)-s >= 5 ){
        
        if(is.null(res[[s+7]][1]$QualityTest) ==FALSE){
          QualityTest <- c(QualityTest, res[[s+7]][1]$QualityTest)
        }else{
          QualityTest <- c(QualityTest, NaN)}
        
        if(is.null(res[[s+7]]$Problems) ==FALSE){
          Problems <- c(Problems, res[[s+7]]$Problems)
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
participants <- rep(participants, 2)
type <- c(rep("few",length(estimation_reference_pos1)) , rep("morph",length(estimation_morph_pos2)))
confidence <- c(confidence_1, confidence_2)
estimation <- c(estimation_reference_pos1, estimation_morph_pos2)
position_stim <- rep(c(1,2), each = length(participants)/2)
  
df_exp <- data.frame(
  participants = participants,
  type = type,
  confidence = confidence,
  estimation = as.numeric(estimation),
  position_stim = position_stim
)

  df_list <- list(a = df_NotExperimentData, b = df_exp, c = AQ)
  return(df_list)
}