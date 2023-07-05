# here I will carry out a few regression models

root <- rprojroot::is_rstudio_project
basename(getwd())

# load dataframe 
filepath <- root$find_file("pilot/long_df_filtered/df_exp_filter_long.Rda")
load(file= filepath)


## for faces

library(stringr)

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

m_1 <- lm(response ~ face_type + face_age, data= d_faces)
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

m_4_bis <- lm(response ~  face_type* face_age* scale(AQ_attencion_detail), data= d_faces)
summary(m_4_bis)


## for dots

d_dots_regression <- df_exp_filter_long %>%
  filter(stimulus == "dot_40_60" |
           stimulus == "dot_60" |
           stimulus == "dot_40") 

# Reorder the categorical variable
d_dots_regression$stimulus <- as.factor(d_dots_regression$stimulus)
d_dots_regression <- d_dots_regression %>%
  mutate(stimulus = relevel(stimulus, ref = "dot_40_60"))

m_1_dots <- lm(response ~  stimulus, data= d_dots_regression)
summary(m_1_dots)

m_2_dots <- lm(response ~  stimulus*AQ, data= d_dots_regression)
summary(m_2_dots)

m_3_dots <- lm(response ~  stimulus*AQ_attencion_detail, data= d_dots_regression)
summary(m_3_dots)
