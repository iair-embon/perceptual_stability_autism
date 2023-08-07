# here I will carry out a few regression models

root <- rprojroot::is_rstudio_project
basename(getwd())

# load dataframe 
filepath <- root$find_file("pilot_2/df_exp_filter_long.Rda")
load(file= filepath)


## for faces

library(stringr)
library(dplyr)

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

m_1 <- lm(response ~ face_type * face_age, data= d_faces)
summary(m_1)
# Ok. When predicting responses, the face_type (1 or 2) is not a significant predictor. Good new

m_2 <- lm(response ~  face_age, data= d_faces)
summary(m_2)
# Wow, so there is an effect of face_age. Between all categories. More good news.

d_faces_NotFace1 <- d_faces %>%
  filter(!(face_type == 1))

m_3 <- lm(response ~  face_age* scale(AQ), data= d_faces_NotFace1)
summary(m_3)

m_3_bis <- lm(response ~  face_age* scale(AQ_attencion_detail), data= d_faces_NotFace1)
summary(m_3_bis)

m_4 <- lm(response ~  face_type* face_age* scale(AQ), data= d_faces)
summary(m_4)

m_4_bis_1 <- lm(response ~  face_type* face_age* scale(AQ_attencion_detail), data= d_faces)
summary(m_4_bis_1)

m_4_bis_2 <- lm(response ~  face_type* face_age* scale(AQ_social), data= d_faces)
summary(m_4_bis_2)

m_4_bis_3 <- lm(response ~  face_type* face_age* scale(AQ_attentional_switches), data= d_faces)
summary(m_4_bis_3)

m_4_bis_4 <- lm(response ~  face_type* face_age* scale(AQ_communication), data= d_faces)
summary(m_4_bis_4)

m_4_bis_5 <- lm(response ~  face_type* face_age* scale(AQ_imagination), data= d_faces)
summary(m_4_bis_5)

## for dots

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

# Reorder the categorical variable
d_dots_regression$dot_amount <- as.factor(d_dots_regression$dot_amount)
d_dots_regression <- d_dots_regression %>%
  mutate(dot_amount = relevel(dot_amount, ref = "morph"))

m_1_dots <- lm(response ~ dot_type + dot_amount, data= d_dots_regression)
summary(m_1_dots)

m_2_dots <- lm(response ~  dot_type* dot_amount* scale(AQ), data= d_dots_regression)
summary(m_2_dots)

m_2_dots_bis_1 <- lm(response ~  dot_type* dot_amount* scale(AQ_attencion_detail), data= d_dots_regression)
summary(m_2_dots_bis_1)

m_2_dots_bis_2 <- lm(response ~  dot_type* dot_amount* scale(AQ_social), data= d_dots_regression)
summary(m_2_dots_bis_2)

m_2_dots_bis_3 <- lm(response ~  dot_type* dot_amount* scale(AQ_attentional_switches), data= d_dots_regression)
summary(m_2_dots_bis_3)

m_2_dots_bis_4 <- lm(response ~  dot_type* dot_amount* scale(AQ_communication), data= d_dots_regression)
summary(m_2_dots_bis_4)

m_2_dots_bis_5 <- lm(response ~  dot_type* dot_amount* scale(AQ_imagination), data= d_dots_regression)
summary(m_2_dots_bis_5)
