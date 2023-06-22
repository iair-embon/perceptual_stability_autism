library(tidyverse)
d1 <- df_exp_filter
d2 <- df_demographic_filter

# junto los datos
d  <- left_join(d1, d2, by = "participants" )

# construyo las variables cara (c1, c2) y edad (young, old y yound-old)
d.caras <- d %>% 
  filter(grepl('videos', stimulus) ) %>% 
  mutate(
  cara = case_when(
    stimulus == "videos/fs1y.mp4" ~ "c1",
    stimulus == "videos/fs1o.mp4" ~ "c1",
    stimulus == "videos/fs2y.mp4" ~ "c2",
    stimulus == "videos/fs2o.mp4" ~ "c2",
    stimulus == "videos/fs1yo.mp4" ~ "c1",
    stimulus == "videos/fs2yo.mp4" ~ "c2"
  ),
  edad = case_when(
    stimulus == "videos/fs1y.mp4" ~ "young",
    stimulus == "videos/fs1o.mp4" ~ "old",
    stimulus == "videos/fs2y.mp4" ~ "young",
    stimulus == "videos/fs2o.mp4" ~ "old",
    stimulus == "videos/fs1yo.mp4" ~ "morph",
    stimulus == "videos/fs2yo.mp4" ~ "morph"
  ))

# respuesta por cara y edad
ggplot(data = d.caras, aes(y = response, x = edad, fill = edad)) + 
  geom_boxplot() + 
  facet_wrap(~ cara) + 
  theme_classic()


# - elijo cara 1 (aparentemente efecto más grande)
# - solo old y morph
# - vemos si la dif entre resp con old y morph depende de AQ
d.test = d.caras %>% filter(cara == "c2", !edad=="young") 
summary( lm( data = d.test, response ~ edad*scale(AQ) ) )

d.caras %>% 
  filter(!edad=="young") %>% 
  ggplot(aes(x = AQ, y = response, color = edad)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ cara) + 
  theme_classic()


d.caras %>% 
  filter(edad=="young") %>% 
  ggplot(aes(x = AQ, y = response)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ cara) + 
  theme_classic()


d.caras %>% 
  ggplot(aes(x = AQ, y = response, color = edad)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  facet_wrap(~ cara) + 
  theme_classic()



summary( lm( data = d.caras, response ~ cara*edad*scale(AQ) ) )
