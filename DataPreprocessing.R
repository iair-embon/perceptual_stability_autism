#################
# preprocessing #
#################

root <- rprojroot::is_rstudio_project
basename(getwd())               

# load dataframe 
filepath <- root$find_file("Data/df_total.Rda")
load(file= filepath)

### Exclusion criteria, data is excluded of future analysis

# leaving only those who tell us that we can count on their answers.
library (stringr)
library (tidyverse)
df_total <- df_total %>% 
  filter(str_detect(df_total$RelyOn, "Pueden")) # if start with "Pueden"
                                                      # it stays

# leaving only those who did not interrup the 
# task drastically (= ok)
df_total <- df_total[df_total$Problems == 'ok',] 

# Filter by performance, leaving only those who have PC > 60 
df_total <- df_total[df_total$PC > 0.60,]

#  having pressed the same confidence key more than 85% of trials
source(root$find_file("Analysis/AuxiliaryFunctions/discard_by_x_same_confidence.R"))
discard_participant <- discard_by_x_same_confidence_new(85,df_total)  
df_total <- df_total[! df_total$Participant %in% discard_participant,]

# Filter by reaction times
df_total <- df_total[df_total$TimeDiscTrial <= 5000,]
df_total <- df_total[df_total$TimeDiscTrial >= 200,]
df_total <- df_total[df_total$TimeConfTrial <=5000,]

# burning the first 20 trials of each subject
df_total <- df_total[df_total$trial > 20,]

# Filter by trails needed to calculate AUROC2
# discarding because very few trials
TrialByParticipant <- rep(NaN, length(unique(df_total$Participant)))
existing_subject <- unique(df_total$Participant)

for (i in 1:length(TrialByParticipant)) {
  TrialByParticipant[i] <- nrow(df_total[df_total$Participant == existing_subject[i],])
}

# I see who are the ones who have fewer trials than X
IndexTrial <- which(TrialByParticipant < 90)
FewTrialsParticipant<- existing_subject[IndexTrial]

# I discard them
df_total <- df_total[! df_total$Participant %in% FewTrialsParticipant,]

### AUROC2
# get metacognitive sensivity

# load the type 2 ROC analysis function
source(root$find_file("Analysis/AuxiliaryFunctions/auroc2.R"))

Nsuj <- length(unique(df_total$Participant))
# saving metacog = mc for each RT discarded
mc <- rep(NA, Nsuj)
ExistingSubjects <- unique(df_total$Participant)

for (i in 1:Nsuj){
  mc[i] <- type2roc(correct = df_total$discrimination_is_correct[df_total$Participant==ExistingSubjects[i]],
                    conf = df_total$confidence_key[df_total$Participant==ExistingSubjects[i]], 
                    Nratings = 4)}

## adding column mc to df_total

All_participants_mc <- c()
for (i in 1:length(ExistingSubjects)) {
  participant_df_exp <- df_total[df_total$Participant == ExistingSubjects[i],]
  trials <- nrow(participant_df_exp)
  participant_mc <-rep(mc[i],trials)
  All_participants_mc <- c(All_participants_mc,participant_mc)
}

df_total$mc <- All_participants_mc

# filter those who have an AUROC2 less than 1.5 standard deviations from the mean
mean_mc <- mean(mc)
sd_mc <-sd(mc)
df_total <- df_total[df_total$mc >= mean_mc - sd_mc* 1.5,]

# filter participants who did not report a choice of binary male or female 
# gender (8 participants; as they were too few to be accounted for in the 
# regression models)
df_total <-  df_total[df_total$gender == 'Female' | df_total$gender == 'Male',]

### Save the df_total, now df_total_filtered
filepath <- root$find_file("Data/df_total_filtered.Rda")
save(df_total_filtered,file = filepath)

