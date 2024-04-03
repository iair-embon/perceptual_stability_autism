library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)

root <- rprojroot::is_rstudio_project
basename(getwd())

# load dataframe 
filepath <- root$find_file("pilot_6(ref26_aq_ref26)/df_exp.csv")
#load(file= filepath)
df_exp <- read.csv(filepath)

# load dataframe 
filepath <- root$find_file("pilot_6(ref26_aq_ref26)/df_NotExperimentData.csv")
#load(file= filepath)
df_NotExperimentData <- read.csv(filepath)


# Long format
df_merge_long <- df_exp %>%
  left_join(df_NotExperimentData, by = 'participants') %>%
  arrange(participants)


# save data set
filepath <- root$find_file("pilot_6(ref26_aq_ref26)/df_merge_long.csv")
write.csv(df_merge_long, file = filepath, row.names = FALSE)


########### from long to wide

df_merge_wide <- df_exp %>%
  pivot_wider(
    names_from = position_stim,
    values_from = estimation,
    names_glue = "{.value}_{position_stim}"
  ) %>%
  left_join(df_NotExperimentData, by = 'participants') %>%
  arrange(participants)

# save data set
filepath <- root$find_file("pilot_6(ref26_aq_ref26)/df_merge_wide.csv")
write.csv(df_merge_wide, file = filepath, row.names = FALSE)



