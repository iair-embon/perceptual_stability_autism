library(dplyr)
library(ggplot2)
library(patchwork)

root <- rprojroot::is_rstudio_project
basename(getwd())

# load dataframe 
filepath <- root$find_file("pilot_4(dot_reference_first)/df_exp.csv")
#load(file= filepath)
df_exp <- read.csv(filepath)

########### plot estimation by type (few - morph)

# Calcular el promedio de estimation por tipo usando dplyr
df_avg <- df_exp %>%
  group_by(type) %>%
  summarise(avg_estimation = mean(estimation))

# Crear un gráfico de caja (boxplot)
boxplot <- ggplot(df_exp, aes(x = type, y = estimation)) +
  geom_boxplot() +
  theme_bw()

# Crear un scatterplot con jitter
scatterplot <- ggplot(df_exp, aes(x = type, y = estimation, group = participants)) +
  geom_jitter(width = 0.05, alpha = 0.7) +
  geom_line(aes(group = participants), alpha = 0.5) + 
  theme_bw()

# Combinar los gráficos usando patchwork (si tienes la librería instalada)
combined_plot <- boxplot + scatterplot
combined_plot


########### plot confidence by type (few - morph)

# Calcular el promedio de estimation por tipo usando dplyr
df_avg <- df_exp %>%
  group_by(type) %>%
  summarise(avg_estimation = mean(confidence))

# Crear un gráfico de caja (boxplot)
boxplot <- ggplot(df_exp, aes(x = type, y = confidence)) +
  geom_boxplot() +
  theme_bw()

# Crear un scatterplot con jitter
scatterplot <- ggplot(df_exp, aes(x = type, y = confidence, group = participants)) +
  geom_jitter(width = 0.00025, alpha = 0.7) +
  geom_line(aes(group = participants), alpha = 0.5) + 
  theme_bw()


# Combinar los gráficos usando patchwork (si tienes la librería instalada)
combined_plot <- boxplot + scatterplot
combined_plot




