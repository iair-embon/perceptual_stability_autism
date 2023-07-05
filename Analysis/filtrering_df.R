# Here I will read the df and make a filtering based on psychiatric condition and medication
# After that, I will combine both df in a unique long df

library(dplyr)

# Read the .txt result
root <- rprojroot::is_rstudio_project
basename(getwd())

filepath <- root$find_file("pilot/df_exp_filtered_mod.csv")
df_exp <- read.csv(file = filepath)

filepath <- root$find_file("pilot/df_NotExperimentData.csv")
df_demographic <- read.csv(file = filepath)

## Filter by psychiatric condition and medication

df_demographic_filter <- df_demographic %>%
  filter(AffectionPsycho_1 == "No") 

paste(nrow(df_demographic) - nrow(df_demographic_filter)," participant/s were removed by psychiatric condition")

df_demographic_filter <- df_demographic_filter %>%
  filter(medication_1 == "No") 

paste(nrow(df_demographic) - nrow(df_demographic_filter)," participant/s were removed by medication condition")

## filter the participant also in df_exp

df_exp_filter <- df_exp %>%
  filter((participants %in% df_demographic_filter$participants))

paste("Total participants after filter: ", length(unique(df_exp_filter$participants)))

## make only one df

df_demographic_filter_colnames <- colnames(df_demographic_filter)

for (column_name in df_demographic_filter_colnames) {
  df_exp_filter[[column_name]] <-  rep(df_demographic_filter[[column_name]], each= 6) 
}

## save the dataframe
df_exp_filter_long <- df_exp_filter

filepath <- root$find_file("pilot/long_df_filtered/df_exp_filter_long.Rda")
save(df_exp_filter_long,file = filepath)
