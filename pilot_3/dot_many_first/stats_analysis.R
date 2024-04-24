library(dplyr)
library(ggplot2)

root <- rprojroot::is_rstudio_project
basename(getwd())

# load dataframe 
filepath <- root$find_file("pilot_3/dot_many_first/df_merge_wide_threeFactors.csv")
#load(file= filepath)
df <- read.csv(filepath)

df$indv_bias = (df$morph- df$many)/df$many

df_filt <- df %>%
  filter((morph<=100) & (many <=100))

## AQ_threeFactor

summary(lm(indv_bias ~ AQ_threeFactor, data = df))

# Crear el gráfico de dispersión con la línea de regresión y ajustes personalizados
ggplot(df, aes(x = AQ_threeFactor, y = indv_bias)) +
  geom_point(color = "black", fill = "grey", size = 3, shape = 21, stroke = 1) +  # Puntos de dispersión personalizados
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  # Línea de regresión azul
  labs(x = "AQ_threeFactor", y = "indv_bias") +
  theme_minimal() +  # Tema minimalista
  theme(panel.grid = element_blank(),  # Eliminar la grilla
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 20),  # Tamaño de la fuente de los ejes
        axis.title = element_text(size = 20))  # Tamaño de la fuente de los títulos de los ejes


## AQ_social_threeFactor

summary(lm(indv_bias ~ AQ_social_threeFactor, data = df))

# Crear el gráfico de dispersión con la línea de regresión y ajustes personalizados
ggplot(df, aes(x = AQ_social_threeFactor, y = indv_bias)) +
  geom_point(color = "black", fill = "grey", size = 3, shape = 21, stroke = 1) +  # Puntos de dispersión personalizados
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  # Línea de regresión azul
  labs(x = "AQ_social_threeFactor", y = "indv_bias") +
  theme_minimal() +  # Tema minimalista
  theme(panel.grid = element_blank(),  # Eliminar la grilla
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 20),  # Tamaño de la fuente de los ejes
        axis.title = element_text(size = 20))  # Tamaño de la fuente de los títulos de los ejes

## AQ_detail_threeFactor

summary(lm(indv_bias ~ AQ_detail_threeFactor, data = df))

# Crear el gráfico de dispersión con la línea de regresión y ajustes personalizados
ggplot(df, aes(x = AQ_detail_threeFactor, y = indv_bias)) +
  geom_point(color = "black", fill = "grey", size = 3, shape = 21, stroke = 1) +  # Puntos de dispersión personalizados
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  # Línea de regresión azul
  labs(x = "AQ_detail_threeFactor", y = "indv_bias") +
  theme_minimal() +  # Tema minimalista
  theme(panel.grid = element_blank(),  # Eliminar la grilla
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 20),  # Tamaño de la fuente de los ejes
        axis.title = element_text(size = 20))  # Tamaño de la fuente de los títulos de los ejes

## AQ_communication_threeFactor

summary(lm(indv_bias ~ AQ_communication_threeFactor, data = df))

# Crear el gráfico de dispersión con la línea de regresión y ajustes personalizados
ggplot(df, aes(x = AQ_communication_threeFactor, y = indv_bias)) +
  geom_point(color = "black", fill = "grey", size = 3, shape = 21, stroke = 1) +  # Puntos de dispersión personalizados
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  # Línea de regresión azul
  labs(x = "AQ_communication_threeFactor", y = "indv_bias") +
  theme_minimal() +  # Tema minimalista
  theme(panel.grid = element_blank(),  # Eliminar la grilla
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 20),  # Tamaño de la fuente de los ejes
        axis.title = element_text(size = 20))  # Tamaño de la fuente de los títulos de los ejes
