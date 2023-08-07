# Here I will read the df and make a filtering based on psychiatric condition and medication
# After that, I will combine both df in a unique long df

library(dplyr)

# Read the .txt result
root <- rprojroot::is_rstudio_project
basename(getwd())

filepath <- root$find_file("pilot_2/df_exp_filtered_mod.csv")
df_exp <- read.csv(file = filepath)

filepath <- root$find_file("pilot_2/df_NotExperimentData.csv")
df_demographic <- read.csv(file = filepath)

## make only one df

df_demographic_colnames <- colnames(df_demographic)

for (column_name in df_demographic_colnames) {
  df_exp[[column_name]] <-  rep(df_demographic[[column_name]], each= 6) 
}

## save the dataframe
df_exp_long <- df_exp

filepath <- root$find_file("pilot_2/df_exp_long.Rda")
save(df_exp_long,file = filepath)

# save it in csv
filepath <- root$find_file("pilot_2/df_exp_long.csv")
write.csv(df_exp_long, file = filepath, row.names = FALSE)


## Filter by psychiatric condition and medication

df_demographic_filter <- df_demographic %>%
  filter(AffectionPsycho_1 == "No") 

paste(nrow(df_demographic) - nrow(df_demographic_filter)," participant/s were removed by psychiatric condition")

df_demographic_filter <- df_demographic_filter %>%
  filter(medication_1 == "No") 

paste(nrow(df_demographic) - nrow(df_demographic_filter)," participant/s were removed by medication condition")

## filter the participant also in df_exp

df_exp_filter <- df_exp %>%
  filter((participant %in% df_demographic_filter$participant))

paste("Total participants after filter: ", length(unique(df_exp_filter$participant)))

## save the filter dataframe
df_exp_filter_long <- df_exp_filter

filepath <- root$find_file("pilot_2/df_exp_filter_long.Rda")
save(df_exp_filter_long,file = filepath)

# save it in csv
filepath <- root$find_file("pilot_2/df_exp_filter_long.csv")
write.csv(df_exp_filter_long, file = filepath, row.names = FALSE)

## from long to wide data set
library(tidyverse)

# with out filter

df_exp_wide <- df_exp_long %>%
  pivot_wider(
    names_from = trial,
    values_from = c(stimulus, response, rt),
    names_prefix = "trial_"
  ) %>%
  select(-starts_with("trial_NA")) %>%
  distinct(participants, .keep_all = TRUE)

# show new data set wide
head(df_exp_wide)


# with filter
df_exp_filter_wide <- df_exp_filter_long %>%
  pivot_wider(
    names_from = trial,
    values_from = c(stimulus, response, rt),
    names_prefix = "trial_"
  ) %>%
  select(-starts_with("trial_NA")) %>%
  distinct(participants, .keep_all = TRUE)

# show new data set wide
head(df_exp_filter_wide)

# save both data set
filepath <- root$find_file("pilot_2/df_exp_filter_wide.csv")
write.csv(df_exp_filter_wide, file = filepath, row.names = FALSE)

filepath <- root$find_file("pilot_2/df_exp_wide.csv")
write.csv(df_exp_wide, file = filepath, row.names = FALSE)
