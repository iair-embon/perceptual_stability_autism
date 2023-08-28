## libraries
library(stringr)
library(dplyr)
library(ggplot2)

root <- rprojroot::is_rstudio_project
basename(getwd())

# load dataframe 
filepath <- root$find_file("pilot_3/df_exp_long_DotsTotal.csv")
#load(file= filepath)
df_exp_long_DotsTotal <- read.csv(filepath)


#### I discard those responses that are 2 deviations away from the mean of their respective group.
d <- df_exp_long_DotsTotal %>%
  filter(order == "manyFirst")

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

# paired t-test
mo <- d_without_outliers %>% filter(type == "morph") %>% select(response)
ma <- d_without_outliers %>% filter(type == "many") %>% select(response)
t.test(mo$response, ma$response, paired = TRUE)

## plot the type and responses

ggplot(d_without_outliers, aes(x = type, y = response, fill = type)) +
  geom_violin(color = "black", alpha = 0.7) +  # Violin plot
  geom_jitter(width = 0.1, alpha = 0.5) +  # Puntos con jitter
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.5,
               colour = "black")+
  ylab("Responses") +
  xlab("Type") +
  scale_fill_discrete(name = "Type") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_text(size = 30),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        legend.position = "none")

# now predict the bias based on AQ
m_1 <- lm(bias ~ AQ, data= d_without_outliers)
summary(m_1)

ggplot(d_without_outliers, aes(x=AQ, y=bias)) + 
  geom_point()+
  geom_smooth(method = "lm",se = FALSE, color = "darkred")+
  geom_ribbon(
    aes(ymin = predict(lm(bias ~ AQ, data = d_without_outliers), newdata = data.frame(AQ = AQ), interval = "confidence")[, "lwr"],
        ymax = predict(lm(bias ~ AQ, data = d_without_outliers), newdata = data.frame(AQ = AQ), interval = "confidence")[, "upr"]),
    fill = "lightpink", alpha = 0.5)+
  ylab("Bias [many-morph]") +
  xlab("AQ") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_text(size = 30),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30))


# now predict the bias based on AQ_social
m_1 <- lm(bias ~ AQ_social, data= d_without_outliers)
summary(m_1)

ggplot(d_without_outliers, aes(x=AQ_social, y=bias)) + 
  geom_point()+
  geom_smooth(method = "lm",se = FALSE, color = "darkred")+
  geom_ribbon(
    aes(ymin = predict(lm(bias ~ AQ_social, data = d_without_outliers), newdata = data.frame(AQ_social = AQ_social), interval = "confidence")[, "lwr"],
        ymax = predict(lm(bias ~ AQ_social, data = d_without_outliers), newdata = data.frame(AQ_social = AQ_social), interval = "confidence")[, "upr"]),
    fill = "lightpink", alpha = 0.5)+
  ylab("Bias [many-morph]") +
  xlab("AQ_social") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_text(size = 30),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30))


# now predict the bias based on AQ_attentional_switches
m_1 <- lm(bias ~ AQ_attentional_switches, data= d_without_outliers)
summary(m_1)

ggplot(d_without_outliers, aes(x=AQ_attentional_switches, y=bias)) + 
  geom_point()+
  geom_smooth(method = "lm",se = FALSE, color = "darkred")+
  geom_ribbon(
    aes(ymin = predict(lm(bias ~ AQ_attentional_switches, data = d_without_outliers), newdata = data.frame(AQ_attentional_switches = AQ_attentional_switches), interval = "confidence")[, "lwr"],
        ymax = predict(lm(bias ~ AQ_attentional_switches, data = d_without_outliers), newdata = data.frame(AQ_attentional_switches = AQ_attentional_switches), interval = "confidence")[, "upr"]),
    fill = "lightpink", alpha = 0.5)+
  ylab("Bias [many-morph]") +
  xlab("AQ attentional switches") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_text(size = 30),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30))


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

ggplot(d_without_outliers, aes(x=AQ_communication, y=bias)) + 
  geom_point()+
  geom_smooth(method = "lm",se = FALSE, color = "darkred")+
  geom_ribbon(
    aes(ymin = predict(lm(bias ~ AQ_communication, data = d_without_outliers), newdata = data.frame(AQ_communication = AQ_communication), interval = "confidence")[, "lwr"],
        ymax = predict(lm(bias ~ AQ_communication, data = d_without_outliers), newdata = data.frame(AQ_communication = AQ_communication), interval = "confidence")[, "upr"]),
    fill = "lightpink", alpha = 0.5)+
  ylab("Bias [many-morph]") +
  xlab("AQ communication") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_text(size = 30),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30))

# now predict the bias based on AQ_imagination
m_1 <- lm(bias ~ AQ_imagination, data= d_without_outliers)
summary(m_1)

ggplot(d_without_outliers, aes(x=AQ_imagination, y=bias)) + 
  geom_point()+
  geom_smooth(method = "lm",se = FALSE, color = "darkred")+
  geom_ribbon(
    aes(ymin = predict(lm(bias ~ AQ_imagination, data = d_without_outliers), newdata = data.frame(AQ_imagination = AQ_imagination), interval = "confidence")[, "lwr"],
        ymax = predict(lm(bias ~ AQ_imagination, data = d_without_outliers), newdata = data.frame(AQ_imagination = AQ_imagination), interval = "confidence")[, "upr"]),
    fill = "lightpink", alpha = 0.5)+
  ylab("Bias [many-morph]") +
  xlab("AQ imagination") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_text(size = 30),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30))

### now I will explore the relation between bias, AQ and order

# discard outliers in the total data set

stimulus_stats <- df_exp_long_DotsTotal %>%
  group_by(type) %>%
  summarize(
    mean_response = mean(response),
    sd_response = sd(response)
  )

df_exp_with_stats <- df_exp_long_DotsTotal %>%
  left_join(stimulus_stats, by = "type")

participants_to_remove <- df_exp_with_stats %>%
  filter(abs(response - mean_response) >= 2 * sd_response) %>%
  distinct(participant) %>%
  pull(participant)

df_exp_long_DotsTotal_without_outliers <- df_exp_with_stats %>%
  filter(!participant %in% participants_to_remove)

# regression model
m_1 <- lm(bias ~ order, data= df_exp_long_DotsTotal_without_outliers)
summary(m_1)

ggplot(df_exp_long_DotsTotal_without_outliers, aes(x = order, y = bias, fill = order)) +
  geom_violin(color = "black", alpha = 0.7) +  # Violin plot
  geom_jitter(width = 0.1, alpha = 0.5) +  # Puntos con jitter
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.5,
               colour = "black")+
  ylab("Bias") +
  xlab("Order") +
  scale_fill_discrete(name = "Order") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_text(size = 30),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        legend.position = "none")

# so, the results show that the order affected the bias

m_1 <- lm(bias ~ order + AQ + order:AQ, data= df_exp_long_DotsTotal_without_outliers)
summary(m_1)

# Crear el gráfico
ggplot(df_exp_long_DotsTotal_without_outliers, aes(x = AQ, y = bias)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  ylab("Bias [many-morph]") +
  xlab("AQ") +
  facet_wrap(~order, scales = "fixed") +  # Dividir el gráfico por niveles de "order" con escalas fijas
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.background = element_blank(),
    axis.title.x = element_text(size = 30),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    strip.text = element_text(size = 20)
  )

## testing differences between many and morph, interparticipants


