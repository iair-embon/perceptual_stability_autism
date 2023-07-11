# here I will carry out a few correlations

root <- rprojroot::is_rstudio_project
basename(getwd())

# load dataframe 
filepath <- root$find_file("pilot/long_df_filtered/df_exp_filter_grouped_long.Rda")
load(file= filepath)


# Is there correlation between AI of face aging and of numerosity? We will use the spearman correlation.

print("Relation between: Individual attraction Index (mean) based on Option 1 of attraction Index (face vs dots): ")
cor.test(df_exp_grouped$dot_attraction_index_individual_mean_d1,
         df_exp_grouped$face_attraction_index_individual_mean_d1, method="spearman")

print("Relation between: Individual attraction Index (median) based on Option 1 of attraction Index (face vs dots): ")
cor.test(df_exp_grouped$dot_attraction_index_individual_median_d1,
         df_exp_grouped$face_attraction_index_individual_median_d1, method="spearman")

print("Relation between: Individual attraction Index (mean) based on Option 2 of attraction Index (face vs dots): ")
cor.test(df_exp_grouped$dot_attraction_index_individual_mean_d2,
         df_exp_grouped$face_attraction_index_individual_mean_d2, method="spearman")

print("Relation between: Individual attraction Index (median) based on Option 2 of attraction Index (face vs dots): ")
cor.test(df_exp_grouped$dot_attraction_index_individual_median_d2,
         df_exp_grouped$face_attraction_index_individual_median_d2, method="spearman")


## We will use the spearman correlation to test AQ vs attraction index of faces

print("AQ - Individual attraction Index (mean) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ, df_exp_grouped$face_attraction_index_individual_mean_d1, method="spearman")

print("AQ - Individual attraction Index (median) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ, df_exp_grouped$face_attraction_index_individual_median_d1, method="spearman")

print("AQ - Individual attraction Index (mean) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ, df_exp_grouped$face_attraction_index_individual_mean_d2, method="spearman")

print("AQ - Individual attraction Index (median) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ, df_exp_grouped$face_attraction_index_individual_median_d2, method="spearman")

## We will use the spearman correlation to test AQ vs attraction index of dots

print("AQ - Individual attraction Index (mean) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ, df_exp_grouped$dot_attraction_index_individual_mean_d1, method="spearman")

print("AQ - Individual attraction Index (median) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ, df_exp_grouped$dot_attraction_index_individual_median_d1, method="spearman")

print("AQ - Individual attraction Index (mean) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ, df_exp_grouped$dot_attraction_index_individual_mean_d2, method="spearman")

print("AQ - Individual attraction Index (median) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ, df_exp_grouped$dot_attraction_index_individual_median_d2, method="spearman")

## AQ Social Subscale for faces

print("AQ Social subscale - Individual attraction Index (mean) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_social, df_exp_grouped$face_attraction_index_individual_mean_d1, method="spearman")

print("AQ Social subscale - Individual attraction Index (median) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_social, df_exp_grouped$face_attraction_index_individual_median_d1, method="spearman")

print("AQ Social subscale - Individual attraction Index (mean) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_social, df_exp_grouped$face_attraction_index_individual_mean_d2, method="spearman")

print("AQ  Social subscale - Individual attraction Index (median) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_social, df_exp_grouped$face_attraction_index_individual_median_d2, method="spearman")

## AQ Attentional Swithces Subscale for faces

print("AQ Attentional Switches subscale - Individual attraction Index (mean) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_attentional_switches, df_exp_grouped$face_attraction_index_individual_mean_d1, method="spearman")

print("AQ Attentional Switches subscale - Individual attraction Index (median) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_attentional_switches, df_exp_grouped$face_attraction_index_individual_median_d1, method="spearman")

print("AQ Attentional Switches subscale - Individual attraction Index (mean) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_attentional_switches, df_exp_grouped$face_attraction_index_individual_mean_d2, method="spearman")

print("AQ Attentional Switches subscale - Individual attraction Index (median) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_attentional_switches, df_exp_grouped$face_attraction_index_individual_median_d2, method="spearman")

## AQ Attention to Detail Subscale for faces

print("AQ Attention to Detail subscale - Individual attraction Index (mean) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_attencion_detail, df_exp_grouped$face_attraction_index_individual_mean_d1, method="spearman")

print("AQ Attention to Detail subscale - Individual attraction Index (median) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_attencion_detail, df_exp_grouped$face_attraction_index_individual_median_d1, method="spearman")

print("AQ Attention to Detail subscale - Individual attraction Index (mean) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_attencion_detail, df_exp_grouped$face_attraction_index_individual_mean_d2, method="spearman")

print("AQ Attention to Detail subscale - Individual attraction Index (median) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_attencion_detail, df_exp_grouped$face_attraction_index_individual_median_d2, method="spearman")

## AQ communication Subscale for faces

print("AQ communication subscale - Individual attraction Index (mean) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_communication, df_exp_grouped$face_attraction_index_individual_mean_d1, method="spearman")

print("AQ communication subscale - Individual attraction Index (median) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_communication, df_exp_grouped$face_attraction_index_individual_median_d1, method="spearman")

print("AQ communication subscale - Individual attraction Index (mean) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_communication, df_exp_grouped$face_attraction_index_individual_mean_d2, method="spearman")

print("AQ communication subscale - Individual attraction Index (median) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_communication, df_exp_grouped$face_attraction_index_individual_median_d2, method="spearman")

## AQ imagination Subscale for faces

print("AQ imagination subscale - Individual attraction Index (mean) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_imagination, df_exp_grouped$face_attraction_index_individual_mean_d1, method="spearman")

print("AQ imagination subscale - Individual attraction Index (median) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_imagination, df_exp_grouped$face_attraction_index_individual_median_d1, method="spearman")

print("AQ imagination subscale - Individual attraction Index (mean) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_imagination, df_exp_grouped$face_attraction_index_individual_mean_d2, method="spearman")

print("AQ imagination subscale - Individual attraction Index (median) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_imagination, df_exp_grouped$face_attraction_index_individual_median_d2, method="spearman")

## AQ Social Subscale for dots

print("AQ Social subscale - Individual attraction Index (mean) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_social, df_exp_grouped$dot_attraction_index_individual_mean_d1, method="spearman")

print("AQ Social subscale - Individual attraction Index (median) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_social, df_exp_grouped$dot_attraction_index_individual_median_d1, method="spearman")

print("AQ Social subscale - Individual attraction Index (mean) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_social, df_exp_grouped$dot_attraction_index_individual_mean_d2, method="spearman")

print("AQ  Social subscale - Individual attraction Index (median) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_social, df_exp_grouped$dot_attraction_index_individual_median_d2, method="spearman")

## AQ Attentional Swithces Subscale for dots

print("AQ Attentional Switches subscale - Individual attraction Index (mean) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_attentional_switches, df_exp_grouped$dot_attraction_index_individual_mean_d1, method="spearman")

print("AQ Attentional Switches subscale - Individual attraction Index (median) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_attentional_switches, df_exp_grouped$dot_attraction_index_individual_median_d1, method="spearman")

print("AQ Attentional Switches subscale - Individual attraction Index (mean) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_attentional_switches, df_exp_grouped$dot_attraction_index_individual_mean_d2, method="spearman")

print("AQ Attentional Switches subscale - Individual attraction Index (median) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_attentional_switches, df_exp_grouped$dot_attraction_index_individual_median_d2, method="spearman")

## AQ Attention to Detail Subscale for dots

print("AQ Attention to Detail subscale - Individual attraction Index (mean) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_attencion_detail, df_exp_grouped$dot_attraction_index_individual_mean_d1, method="spearman")

print("AQ Attention to Detail subscale - Individual attraction Index (median) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_attencion_detail, df_exp_grouped$dot_attraction_index_individual_median_d1, method="spearman")

print("AQ Attention to Detail subscale - Individual attraction Index (mean) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_attencion_detail, df_exp_grouped$dot_attraction_index_individual_mean_d2, method="spearman")

print("AQ Attention to Detail subscale - Individual attraction Index (median) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_attencion_detail, df_exp_grouped$dot_attraction_index_individual_median_d2, method="spearman")

## AQ communication Subscale for dots

print("AQ communication subscale - Individual attraction Index (mean) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_communication, df_exp_grouped$dot_attraction_index_individual_mean_d1, method="spearman")

print("AQ communication subscale - Individual attraction Index (median) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_communication, df_exp_grouped$dot_attraction_index_individual_median_d1, method="spearman")

print("AQ communication subscale - Individual attraction Index (mean) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_communication, df_exp_grouped$dot_attraction_index_individual_mean_d2, method="spearman")

print("AQ communication subscale - Individual attraction Index (median) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_communication, df_exp_grouped$dot_attraction_index_individual_median_d2, method="spearman")

## AQ imagination Subscale for dots

print("AQ imagination subscale - Individual attraction Index (mean) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_imagination, df_exp_grouped$dot_attraction_index_individual_mean_d1, method="spearman")

print("AQ imagination subscale - Individual attraction Index (median) based on Option 1 of attraction Index: ")
cor.test(df_exp_grouped$AQ_imagination, df_exp_grouped$dot_attraction_index_individual_median_d1, method="spearman")

print("AQ imagination subscale - Individual attraction Index (mean) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_imagination, df_exp_grouped$dot_attraction_index_individual_mean_d2, method="spearman")

print("AQ imagination subscale - Individual attraction Index (median) based on Option 2 of attraction Index: ")
cor.test(df_exp_grouped$AQ_imagination, df_exp_grouped$dot_attraction_index_individual_median_d2, method="spearman")

