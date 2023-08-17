## libraries
library(stringr)
library(dplyr)
library(ggplot2)

root <- rprojroot::is_rstudio_project
basename(getwd())

# load dataframe 
filepath <- root$find_file("pilot_3/dot_morph_first/df_exp_filter_long.Rda")
load(file= filepath)


# create bias column
d <- df_exp_filter_long %>%
  group_by(participant) %>%
  mutate(bias = response[type == "many"] - response[type == "morph"])

#### I discard those responses that are 2 deviations away from the mean of their respective group.

# Calculates the mean and standard deviation for each stimulus.
stimulus_stats <- d %>%
  group_by(type) %>%
  summarize(
    mean_response = mean(response),
    sd_response = sd(response)
  )

# Join the statistics back to the original dataframe.
df_exp_with_stats <- d %>%
  left_join(stimulus_stats, by = "type")

# Filter out responses that are more than 3 standard deviations away from the mean.
 participants_to_remove <- df_exp_with_stats %>%
  filter(abs(response - mean_response) >= 2 * sd_response) %>%
  distinct(participant) %>%
  pull(participant)
   
d_without_outliers <- df_exp_with_stats %>%
  filter(!participant %in% participants_to_remove)

#### Regression analysis

# similar to t test between type morph and many in resposnes
# with outliers
m <- lm(response ~ type, data= d)
summary(m)
# without outliers
m <- lm(response ~ type, data= d_without_outliers)
summary(m)


# now predict the bias based on AQ
m_1 <- lm(bias ~ AQ, data= d_without_outliers)
summary(m_1)

ggplot(d, aes(AQ, bias)) +
  geom_point()+
  labs(x = "AQ", y = "bias")+
  geom_smooth(method = "lm")


# now predict the bias based on AQ_social
m_1 <- lm(bias ~ AQ_social, data= d_without_outliers)
summary(m_1)

ggplot(d, aes(AQ_social, bias)) +
  geom_point()+
  labs(x = "AQ_social", y = "bias")+
  geom_smooth(method = "lm")


# now predict the bias based on AQ_attentional_switches
m_1 <- lm(bias ~ AQ_attentional_switches, data= d_without_outliers)
summary(m_1)

ggplot(d, aes(AQ_attentional_switches, bias)) +
  geom_point()+
  labs(x = "AQ_attentional_switches", y = "bias")+
  geom_smooth(method = "lm")

# now predict the bias based on AQ_attencion_detail
m_1 <- lm(bias ~ AQ_attencion_detail, data= d_without_outliers)
summary(m_1)

ggplot(d, aes(AQ_attencion_detail, bias)) +
  geom_point()+
  labs(x = "AQ_attencion_detail", y = "bias")+
  geom_smooth(method = "lm")

# now predict the bias based on AQ_communication
m_1 <- lm(bias ~ AQ_communication, data= d_without_outliers)
summary(m_1)

ggplot(d, aes(AQ_communication, bias)) +
  geom_point()+
  labs(x = "AQ_communication", y = "bias")+
  geom_smooth(method = "lm")

# now predict the bias based on AQ_imagination
m_1 <- lm(bias ~ AQ_imagination, data= d_without_outliers)
summary(m_1)

ggplot(d, aes(AQ_imagination, bias)) +
  geom_point()+
  labs(x = "AQ_imagination", y = "bias")+
  geom_smooth(method = "lm")

