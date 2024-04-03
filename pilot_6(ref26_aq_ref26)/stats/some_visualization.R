library(readr)
library(ggplot2)
library(dplyr)


df_merge_long <- read_csv("pilot_6(ref26_aq_ref26)/df_merge_long.csv")


# Crear el gráfico
ggplot(df_merge_long, aes(x = as.factor(position_stim), y = estimation, color = as.factor(position_stim))) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  labs(x = "Position Stim", y = "Estimation") +
  #facet_wrap(~type, scales = "free") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),  # Tamaño del texto del título de los ejes
    axis.text = element_text(size = 10),   # Tamaño del texto de los números en los ejes
    legend.title = element_text(size = 14),  # Tamaño del texto del título de la leyenda
    legend.text = element_text(size = 12)   # Tamaño del texto en la leyenda
  )


# Filtrar el dataframe para eliminar las estimaciones por encima de 100
df_filtered <- df_merge_long %>% filter(estimation <= 99)

# Crear el gráfico con el dataframe filtrado
ggplot(df_filtered, aes(x = as.factor(position_stim), y = estimation, color = as.factor(position_stim))) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  labs(x = "Position Stim", y = "Estimation") +
  #facet_wrap(~type, scales = "free") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),  # Tamaño del texto del título de los ejes
    axis.text = element_text(size = 10),   # Tamaño del texto de los números en los ejes
    legend.title = element_text(size = 14),  # Tamaño del texto del título de la leyenda
    legend.text = element_text(size = 12)   # Tamaño del texto en la leyenda
  )



## realizo un t test pareado

t.test(estimation ~ position_stim, data = df_merge_long, paired = TRUE)

wilcox.test(estimation ~ position_stim, data = df_merge_long, paired = TRUE)

