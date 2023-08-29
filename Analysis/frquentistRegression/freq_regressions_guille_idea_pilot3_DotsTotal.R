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

d <- df_exp_long_DotsTotal

#### outliers
outliers <- function(d, by = "bias", sd_out = 2){
  if(by == "bias"){
    sd_bias <- sd(d$bias)
    mean_bias <- mean(d$bias)
    # Filter out responses that are more than X standard deviations away from the mean.
    participants_to_remove <- d %>%
      filter(abs(bias - mean_bias) >= sd_out * sd_bias) %>%
      distinct(participant) %>%
      pull(participant)
    
    d_without_outliers <- d %>%
      filter(!participant %in% participants_to_remove)
  }
  
  if(by == "response"){
    ## discard those responses that are 2 deviations away from the mean of their respective group.
    
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
    # Filter out responses that are more than X standard deviations away from the mean.
    participants_to_remove <- df_exp_with_stats %>%
      filter(abs(response - mean_response) >= sd_out * sd_response) %>%
      distinct(participant) %>%
      pull(participant)
    
    d_without_outliers <- df_exp_with_stats %>%
      filter(!participant %in% participants_to_remove)
  }
  
  return(d_without_outliers)
} # by = "response"

d_without_outliers <- outliers(d, by = "bias", sd_out = 2)

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
d_without_outliers <- d_without_outliers %>%
  mutate(order_code = if_else(order == "morphFirst", -1 , 1)) %>%
  mutate(AQ_scaled = AQ-mean(AQ)) %>%
  mutate(age_scaled = age-mean(age))

m_1 <- lm(bias ~ AQ_scaled + order_code + sex + age_scaled, data= d_without_outliers)
summary(m_1)

ggplot(d_without_outliers, aes(x=AQ_scaled, y=bias)) + 
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

## diagnosis of the model

# Linearity of the data
plot(m_1, 1)

# Normality of residuals
plot(m_1, 2)

# distribution of studentized residuals
library(MASS)
sresid <- studres(m_1) 
shapiro.test(sresid)

# High leverage points
plot(m_1, 5)

#Cook's distance
plot(m_1, 4)

# homoscedasticity
library(car)
# non-constant error variance test
ncvTest(m_1)
plot(m_1, 3)

# independence (autocorrelation)
# durbin watson test
durbinWatsonTest(m_1)


m_1 <- lm(bias ~ AQ + order_code + sex + age, data= d_without_outliers)
summary(m_1)


## now predict the bias based on AQ_social
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

# regression model
m_1 <- lm(bias ~ order, data= d_without_outliers)
summary(m_1)

ggplot(d_without_outliers, aes(x = order, y = bias, fill = order)) +
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


# paired t-test, testing differences between many and morph, interparticipants

mo <- d_without_outliers %>% filter(type == "morph" & order == "morphFirst") %>% select(response)
ma <- d_without_outliers %>% filter(type == "many" & order == "manyFirst") %>% select(response)

t.test(mo$response, ma$response)

d_plot <- data.frame(type = c(rep("morph", nrow(mo)),
                              rep("many", nrow(ma))),
                     response = c(mo$response,ma$response))

ggplot(d_plot, aes(x = type, y = response, fill = type)) +
  geom_violin(color = "black", alpha = 0.7) +  # Violin plot
  geom_jitter(width = 0.1, alpha = 0.5) +  # Puntos con jitter
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.5,
               colour = "black")+
  ylab("response") +
  xlab("First Trial") +
  scale_fill_discrete(name = "First Trial") +
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

# linear regression model
summary(lm(response ~ type, data = d_plot))


m_1 <- lm(bias ~ order + AQ, data= d_without_outliers)
summary(m_1)

# Crear el gráfico
ggplot(d_without_outliers, aes(x = AQ, y = bias)) +
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


