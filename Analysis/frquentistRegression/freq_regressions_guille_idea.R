## libraries
library(stringr)
library(dplyr)
library(ggplot2)

root <- rprojroot::is_rstudio_project
basename(getwd())

# load dataframe 
filepath <- root$find_file("pilot_2/df_exp_filter_long.Rda")
load(file= filepath)

## I discard those responses that are 3 deviations away from the mean of their respective group.

# Calculates the mean and standard deviation for each stimulus.
stimulus_stats <- df_exp_filter_long %>%
  group_by(stimulus) %>%
  summarize(
    mean_response = mean(response),
    sd_response = sd(response)
  )

# Join the statistics back to the original dataframe.
df_exp_with_stats <- df_exp_filter_long %>%
  left_join(stimulus_stats, by = "stimulus")

# Filter out responses that are more than 3 standard deviations away from the mean.
df_exp_filter_long <- df_exp_with_stats %>%
  filter(abs(response - mean_response) <= 2 * sd_response)


## for faces

# I will work only with faces first
d_faces <- df_exp_filter_long %>%
  filter(stimulus == "videos/fs1yo.mp4"|
           stimulus == "videos/fs1o.mp4" |
           stimulus == "videos/fs1y.mp4" |
           stimulus == "videos/fs2yo.mp4"|
           stimulus == "videos/fs2o.mp4" |
           stimulus == "videos/fs2y.mp4") %>%
  mutate(face_type = case_when(
    str_detect(stimulus, "(videos/fs1yo.mp4|videos/fs1o.mp4|videos/fs1y.mp4)") ~ 0,
    str_detect(stimulus, "(videos/fs2yo.mp4|videos/fs2o.mp4|videos/fs2y.mp4)") ~ 1,
    TRUE ~ NA 
  ),
  face_age = case_when(
    str_detect(stimulus, "(videos/fs1yo.mp4|videos/fs2yo.mp4)") ~ "morph",
    str_detect(stimulus, "(videos/fs2y.mp4|videos/fs1y.mp4)") ~ "young",
    str_detect(stimulus, "(videos/fs2o.mp4|videos/fs1o.mp4)") ~ "old",
    TRUE ~ NA 
  )) %>%
  select(!(stimulus))


# I need the avarage of old face tipe 1 and old face tip2
mean_old_t1 <- d_faces %>%
  filter(face_age == "old" & face_type == 0) %>%
  summarise(media_response = mean(response)) %>%
  .$media_response

mean_old_t2 <- d_faces %>%
  filter(face_age == "old" & face_type == 1) %>%
  summarise(media_response = mean(response)) %>%
  .$media_response

# we dont need young or old either. Only keep morph
d_guille <- d_faces %>%
  filter(face_age != "young",
         face_age != "old")

# now I do morph1 - mean_old_t1 and morph2 - mean_old_t1
d_guille <- d_guille %>%
  mutate(bias = if_else(face_type == 0, 
                        response - mean_old_t1,
                        response - mean_old_t2))

# now predict the bias based on AQ
# so, we have: [morph_i - mean(old)] ~ AQ_i. i is the participant
m_1 <- lm(bias ~ AQ, data= d_guille)
summary(m_1)

ggplot(d_guille, aes(AQ, bias)) +
  geom_point()+
  labs(x = "AQ", y = "bias")+
  geom_smooth(method = "lm")

m_2 <- lm(bias ~ AQ * face_type , data= d_guille)
summary(m_2)

ggplot(d_guille, aes(AQ, bias, color = as.factor(face_type))) +
  geom_point()+
  labs(x = "AQ", y = "bias")+
  geom_smooth(method = "lm") + 
  scale_color_discrete(name = "face type")

## now repete all with the other subscales

# AQ_attencion_detail
m_1_AQ_attencion_detail <- lm(bias ~ AQ_attencion_detail, data= d_guille)
summary(m_1_AQ_attencion_detail)

ggplot(d_guille, aes(AQ_attencion_detail, bias)) +
  geom_point()+
  labs(x = "AQ_attencion_detail", y = "bias")+
  geom_smooth(method = "lm")

m_2_AQ_attencion_detail <- lm(bias ~ AQ_attencion_detail * face_type , data= d_guille)
summary(m_2_AQ_attencion_detail)

ggplot(d_guille, aes(AQ_attencion_detail, bias, color = as.factor(face_type))) +
  geom_point()+
  labs(x = "AQ_attencion_detail", y = "bias")+
  geom_smooth(method = "lm") + 
  scale_color_discrete(name = "face type")

# AQ_social
m_1_AQ_social <- lm(bias ~ AQ_social, data= d_guille)
summary(m_1_AQ_social)

ggplot(d_guille, aes(AQ_social, bias)) +
  geom_point()+
  labs(x = "AQ_social", y = "bias")+
  geom_smooth(method = "lm")

m_2_AQ_social <- lm(bias ~ AQ_social * face_type , data= d_guille)
summary(m_2_AQ_social)

ggplot(d_guille, aes(AQ_social, bias, color = as.factor(face_type))) +
  geom_point()+
  labs(x = "AQ_social", y = "bias")+
  geom_smooth(method = "lm") + 
  scale_color_discrete(name = "face type")

# AQ_attentional_switches
m_1_AQ_attentional_switches <- lm(bias ~ AQ_attentional_switches, data= d_guille)
summary(m_1_AQ_attentional_switches)

ggplot(d_guille, aes(AQ_attentional_switches, bias)) +
  geom_point()+
  labs(x = "AQ_attentional_switches", y = "bias")+
  geom_smooth(method = "lm")

m_2_AQ_attentional_switches <- lm(bias ~ AQ_attentional_switches * face_type , data= d_guille)
summary(m_2_AQ_attentional_switches)

ggplot(d_guille, aes(AQ_attentional_switches, bias, color = as.factor(face_type))) +
  geom_point()+
  labs(x = "AQ_attentional_switches", y = "bias")+
  geom_smooth(method = "lm") + 
  scale_color_discrete(name = "face type")

# AQ_communication
m_1_AQ_communication <- lm(bias ~ AQ_communication, data= d_guille)
summary(m_1_AQ_communication)

ggplot(d_guille, aes(AQ_communication, bias)) +
  geom_point()+
  labs(x = "AQ_communication", y = "bias")+
  geom_smooth(method = "lm")

m_2_AQ_communication <- lm(bias ~ AQ_communication * face_type , data= d_guille)
summary(m_2_AQ_communication)

ggplot(d_guille, aes(AQ_communication, bias, color = as.factor(face_type))) +
  geom_point()+
  labs(x = "AQ_communication", y = "bias")+
  geom_smooth(method = "lm") + 
  scale_color_discrete(name = "face type")

# AQ_imagination
m_1_AQ_imagination <- lm(bias ~ AQ_imagination, data= d_guille)
summary(m_1_AQ_imagination)

ggplot(d_guille, aes(AQ_imagination, bias)) +
  geom_point()+
  labs(x = "AQ_imagination", y = "bias")+
  geom_smooth(method = "lm")

m_2_AQ_imagination <- lm(bias ~ AQ_imagination * face_type , data= d_guille)
summary(m_2_AQ_imagination)

ggplot(d_guille, aes(AQ_imagination, bias, color = as.factor(face_type))) +
  geom_point()+
  labs(x = "AQ_imagination", y = "bias")+
  geom_smooth(method = "lm") + 
  scale_color_discrete(name = "face type")


###### now I will do the same for dots

d_dots_regression <- df_exp_filter_long %>%
  filter(stimulus == "dot1_13_26" |
           stimulus == "dot2_26_52" |
           stimulus == "dot2_26"|
           stimulus == "dot1_13" |
           stimulus == "dot2_52" |
           stimulus == "dot1_26") %>%
  mutate(dot_type = case_when(
    str_detect(stimulus, "(dot1_13_26|dot1_13|dot1_26)") ~ 0,
    str_detect(stimulus, "(dot2_26_52|dot2_26|dot2_52)") ~ 1,
    TRUE ~ NA 
  ),
  dot_amount = case_when(
    str_detect(stimulus, "(dot1_13_26|dot2_26_52)") ~ "morph",
    str_detect(stimulus, "(dot1_13|dot2_26)") ~ "few",
    str_detect(stimulus, "(dot1_26|dot2_52)") ~ "many",
    TRUE ~ NA 
  )) %>%
  select(!(stimulus))

# I need the avarage of old face tipe 1 and old face tip2
mean_many_t1 <- d_dots_regression %>%
  filter(dot_amount == "many" & dot_type == 0) %>%
  summarise(media_response = mean(response)) %>%
  .$media_response

mean_many_t2 <- d_dots_regression %>%
  filter(dot_amount == "many" & dot_type == 1) %>%
  summarise(media_response = mean(response)) %>%
  .$media_response

# we dont need young or old either. Only keep morph
dots_guille <- d_dots_regression %>%
  filter(dot_amount != "few",
         dot_amount != "many")

# now I do morph1 - mean_old_t1 and morph2 - mean_old_t1
dots_guille <- dots_guille %>%
  mutate(bias = if_else(dot_type == 0, 
                        response - mean_many_t1,
                        response - mean_many_t2))

# now predict the bias based on AQ
# so, we have: [morph_i - mean(old)] ~ AQ_i. i is the participant
m_1 <- lm(bias ~ AQ, data= dots_guille)
summary(m_1)

ggplot(dots_guille, aes(AQ, bias)) +
  geom_point()+
  labs(x = "AQ", y = "bias")+
  geom_smooth(method = "lm")

m_2 <- lm(bias ~ AQ * dot_type , data= dots_guille)
summary(m_2)

ggplot(dots_guille, aes(AQ, bias, color = as.factor(dot_type))) +
  geom_point()+
  labs(x = "AQ", y = "bias")+
  geom_smooth(method = "lm") + 
  scale_color_discrete(name = "dot type")

## now repete all with the other subscales

# AQ_attencion_detail
m_1_AQ_attencion_detail <- lm(bias ~ AQ_attencion_detail, data= dots_guille)
summary(m_1_AQ_attencion_detail)

ggplot(dots_guille, aes(AQ_attencion_detail, bias)) +
  geom_point()+
  labs(x = "AQ_attencion_detail", y = "bias")+
  geom_smooth(method = "lm")

m_2_AQ_attencion_detail <- lm(bias ~ AQ_attencion_detail * dot_type , data= dots_guille)
summary(m_2_AQ_attencion_detail)

ggplot(dots_guille, aes(AQ_attencion_detail, bias, color = as.factor(dot_type))) +
  geom_point()+
  labs(x = "AQ_attencion_detail", y = "bias")+
  geom_smooth(method = "lm") + 
  scale_color_discrete(name = "dot type")

# AQ_social
m_1_AQ_social <- lm(bias ~ AQ_social, data= dots_guille)
summary(m_1_AQ_social)

ggplot(dots_guille, aes(AQ_social, bias)) +
  geom_point()+
  labs(x = "AQ_social", y = "bias")+
  geom_smooth(method = "lm")

m_2_AQ_social <- lm(bias ~ AQ_social * dot_type , data= dots_guille)
summary(m_2_AQ_social)

ggplot(dots_guille, aes(AQ_social, bias, color = as.factor(dot_type))) +
  geom_point()+
  labs(x = "AQ_social", y = "bias")+
  geom_smooth(method = "lm") + 
  scale_color_discrete(name = "face type")

# AQ_attentional_switches
m_1_AQ_attentional_switches <- lm(bias ~ AQ_attentional_switches, data= dots_guille)
summary(m_1_AQ_attentional_switches)

ggplot(dots_guille, aes(AQ_attentional_switches, bias)) +
  geom_point()+
  labs(x = "AQ_attentional_switches", y = "bias")+
  geom_smooth(method = "lm")

m_2_AQ_attentional_switches <- lm(bias ~ AQ_attentional_switches * dot_type , data= dots_guille)
summary(m_2_AQ_attentional_switches)

ggplot(dots_guille, aes(AQ_attentional_switches, bias, color = as.factor(dot_type))) +
  geom_point()+
  labs(x = "AQ_attentional_switches", y = "bias")+
  geom_smooth(method = "lm") + 
  scale_color_discrete(name = "dot type")

# AQ_communication
m_1_AQ_communication <- lm(bias ~ AQ_communication, data= dots_guille)
summary(m_1_AQ_communication)

ggplot(dots_guille, aes(AQ_communication, bias)) +
  geom_point()+
  labs(x = "AQ_communication", y = "bias")+
  geom_smooth(method = "lm")

m_2_AQ_communication <- lm(bias ~ AQ_communication * dot_type , data= dots_guille)
summary(m_2_AQ_communication)

ggplot(dots_guille, aes(AQ_communication, bias, color = as.factor(dot_type))) +
  geom_point()+
  labs(x = "AQ_communication", y = "bias")+
  geom_smooth(method = "lm") + 
  scale_color_discrete(name = "dot type")

# AQ_imagination
m_1_AQ_imagination <- lm(bias ~ AQ_imagination, data= dots_guille)
summary(m_1_AQ_imagination)

ggplot(dots_guille, aes(AQ_imagination, bias)) +
  geom_point()+
  labs(x = "AQ_imagination", y = "bias")+
  geom_smooth(method = "lm")

m_2_AQ_imagination <- lm(bias ~ AQ_imagination * dot_type , data= dots_guille)
summary(m_2_AQ_imagination)

ggplot(dots_guille, aes(AQ_imagination, bias, color = as.factor(dot_type))) +
  geom_point()+
  labs(x = "AQ_imagination", y = "bias")+
  geom_smooth(method = "lm") + 
  scale_color_discrete(name = "dot type")

