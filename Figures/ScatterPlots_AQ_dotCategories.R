## scatter plot dot categories and responses in the dot task


root <- rprojroot::is_rstudio_project
basename(getwd())

# load dataframe 
filepath <- root$find_file("pilot/long_df_filtered/df_exp_filter_long.Rda")
load(file= filepath)


library(dplyr)
library(ggplot2)


d <- df_exp_filter_long %>%
  filter(stimulus == "dot_40_60" |
           stimulus == "dot_60" |
           stimulus == "dot_40") %>%
  mutate(stimulus = factor(stimulus, levels = c("dot_40_60", "dot_60", "dot_40")))

#cols <-  c("#619CFF", "#F8766D" ,"#00BA38")#c("lightblue", "red", "green")

d %>% 
  ggplot(aes(x = AQ, y = response, color = stimulus)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  #scale_color_manual(values = cols) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))




## only to attention to detail 
d %>% 
  ggplot(aes(x = AQ_attencion_detail, y = response, color = stimulus)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  theme_classic() +
  #scale_color_manual(values = cols) +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
