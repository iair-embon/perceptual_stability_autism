library(dplyr)
library(ggplot2)
library(patchwork)

root <- rprojroot::is_rstudio_project
basename(getwd())

# load dataframe 
filepath <- root$find_file("Big_pilot_4(dot_reference_first)/df_exp.csv")
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

## Pruebo lo mismo eliminando por mayores a 100 y estimaciones > a 2 std de la media

participants_to_exclude_1 <- df_exp %>%
  filter(estimation > 100) %>% # elimino por aquellos que pusieron arriba de 100
  distinct(participants)

df_filtered_1 = df_exp %>% 
  filter(!(participants %in% participants_to_exclude_1$participants))

df_summary <- df_filtered_1 %>%
  group_by(type) %>%
  summarise(avg_estimation = mean(estimation),
            sd_estimation = sd(estimation))  

participants_to_exclude_2 <- df_filtered_1 %>%
  left_join(df_summary, by = "type") %>%
  filter(estimation >= avg_estimation + (2 * sd_estimation)) %>% ## elimino por aquellos que pusieron estimaciones 2 std arriba de la media
  distinct(participants)

df_filtered_2 = df_filtered_1 %>% 
  filter(!(participants %in% participants_to_exclude_2$participants))
  

df_filtered_2 %>%
  group_by(type) %>%
  summarise(avg_estimation = mean(estimation),
            sd_estimation = sd(estimation))

data_few <- df_filtered_2 %>%
  filter(type == "few") %>%
  .$estimation

data_morph <- df_filtered_2 %>%
  filter(type == "morph") %>%
  .$estimation

# Realizar un t-test
t.test(data_few, data_morph, paired = T)

wilcox.test(data_few, data_morph, paired = T)

########### plot estimation by type (few - morph)

# Calcular el promedio de estimation por tipo usando dplyr
df_avg <- df_filtered_2 %>%
  group_by(type) %>%
  summarise(avg_estimation = mean(estimation))

# Crear un gráfico de caja (boxplot)
boxplot <- ggplot(df_filtered_2, aes(x = type, y = estimation)) +
  geom_boxplot() +
  theme_bw()

# Crear un scatterplot con jitter
scatterplot <- ggplot(df_filtered_2, aes(x = type, y = estimation, group = participants)) +
  geom_jitter(width = 0.05, alpha = 0.7) +
  geom_line(aes(group = participants), alpha = 0.5) + 
  theme_bw()

# Combinar los gráficos usando patchwork (si tienes la librería instalada)
combined_plot <- boxplot + scatterplot
combined_plot

### Hago un análisis intra participant

diff_estimation <- c()

for (i in unique(df_filtered_2$participants)) {
  few_est <- df_filtered_2 %>%
    filter(participants == i & type == 'few') %>%
    pull(estimation)
  
  morph_est <- df_filtered_2 %>%
    filter(participants == i & type == 'morph') %>%
    pull(estimation)
  
  result <- few_est - morph_est
  diff_estimation <- c(diff_estimation, rep(result, 2))
}

df_filtered_2$diff_estimation <- diff_estimation

# Realizar un t-test contra 0 en las diferencias

t.test(diff_estimation, alternative = "two.sided")

wilcox.test(diff_estimation)

## Yuval index 

yuval_index <- c()

for (i in unique(df_filtered_2$participants)) {
  few_est <- df_filtered_2 %>%
    filter(participants == i & type == 'few') %>%
    pull(estimation)
  
  morph_est <- df_filtered_2 %>%
    filter(participants == i & type == 'morph') %>%
    pull(estimation)
  
  result <- (morph_est - few_est) / few_est
  yuval_index <- c(yuval_index, rep(result, 2))
}

df_filtered_2$yuval_index <- yuval_index
