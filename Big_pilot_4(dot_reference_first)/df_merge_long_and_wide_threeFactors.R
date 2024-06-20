library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)

root <- rprojroot::is_rstudio_project
basename(getwd())

# load dataframe 
filepath <- root$find_file("Big_pilot_4(dot_reference_first)/df_exp_threeFactors.csv")
#load(file= filepath)
df_exp <- read.csv(filepath)

# load dataframe 
filepath <- root$find_file("Big_pilot_4(dot_reference_first)/df_NotExperimentData_threeFactors.csv")
#load(file= filepath)
df_NotExperimentData <- read.csv(filepath)


# Long format
df_merge_long_threeFactors <- df_exp %>%
  left_join(df_NotExperimentData, by = 'participants') %>%
  arrange(participants)


# save data set
filepath <- root$find_file("Big_pilot_4(dot_reference_first)/df_merge_long_threeFactors.csv")
write.csv(df_merge_long_threeFactors, file = filepath, row.names = FALSE)


########### from long to wide

df_merge_wide_threeFactors <- df_exp %>%
  pivot_wider(
    names_from = type,
    values_from = c(confidence, estimation, position_stim),
    names_glue = "{.value}_{type}"
  ) %>%
  left_join(df_NotExperimentData, by = 'participants') %>%
  arrange(participants)

# save data set
filepath <- root$find_file("Big_pilot_4(dot_reference_first)/df_merge_wide_threeFactors.csv")
write.csv(df_merge_wide_threeFactors, file = filepath, row.names = FALSE)



