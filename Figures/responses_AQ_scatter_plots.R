## Scatter plots of correlation

library(ggplot2)

## idx attraction

root <- rprojroot::is_rstudio_project
basename(getwd())

# load dataframe 
filepath <- root$find_file("pilot/long_df_filtered/df_exp_filter_long.Rda")
load(file= filepath)

filepath <- root$find_file("pilot/long_df_filtered/df_exp_filter_grouped_long.Rda")
load(file= filepath)


d <- df_exp_grouped

## for faces

# for AQ
ggplot(d, aes(AQ,
              face_attraction_index_individual_mean_d1)) +
  geom_point()+
  geom_smooth(method="lm", se=T) +
  labs(x = "AQ", y = "Attraction index (ind. bias)",
       title = "Aging Face - predictive line") +
  annotate("text", x = 35, y = 1, 
           label = paste("ρ =", 0.12),
           hjust=1, vjust=0,
           size = 5) +
  theme(
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    plot.title = element_text(size=20, face="bold", hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

# for AQ subscale attendion to detail

ggplot(d, aes(AQ_attencion_detail,
              face_attraction_index_individual_mean_d1)) +
  geom_point()+
  geom_smooth(method="lm", se=T) +
  labs(x = "AQ: Attention to detail", y = "Attraction index (ind. bias)",
       title = "Aging Face - predictive line") +
  annotate("text", x = 7.5, y = 1, 
           label = paste("ρ =", -0.05),
           hjust=1, vjust=0,
           size = 5) +
  theme(
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    plot.title = element_text(size=20, face="bold", hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

## for dots

# for AQ
ggplot(d, aes(AQ,
              dot_attraction_index_individual_mean_d1)) +
  geom_point()+
  geom_smooth(method="lm", se=T) +
  labs(x = "AQ", y = "Attraction index (ind. bias)",
       title = "Dots Numerosity - predictive line") +
  annotate("text", x = 35, y = 2, 
           label = paste("ρ =", -0.07),
           hjust=1, vjust=0,
           size = 5) +
  theme(
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    plot.title = element_text(size=20, face="bold", hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

# for AQ subscale attendion to detail

ggplot(d, aes(AQ_attencion_detail,
              dot_attraction_index_individual_mean_d1)) +
  geom_point()+
  geom_smooth(method="lm", se=T) +
  labs(x = "AQ: Attention to detail", y = "Attraction index (ind. bias)",
       title = "Dots Numerosity - predictive line") +
  annotate("text", x = 7, y = 2, 
           label = paste("ρ =", -0.31),
           hjust=1, vjust=0,
           size = 5)+
  theme(
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    plot.title = element_text(size=20, face="bold", hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )
