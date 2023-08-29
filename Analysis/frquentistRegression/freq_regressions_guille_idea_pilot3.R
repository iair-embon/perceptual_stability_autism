## libraries
library(stringr)
library(dplyr)
library(ggplot2)

root <- rprojroot::is_rstudio_project
basename(getwd())

# load dataframe 
filepath <- root$find_file("pilot_3/dot_many_first/df_exp_long.Rda")
load(file= filepath)


# create bias column
d <- df_exp_long %>%
  group_by(participant) %>%
  mutate(bias = response[type == "many"] - response[type == "morph"]) %>%
  ungroup()


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

## plot the type and responses

ggplot(d_without_outliers, aes(x = bias)) +
  geom_histogram(binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  geom_vline(xintercept = 0, linetype='dotted', col = 'black')+
  xlab("Bias") +
  ylab("Participants")+
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


## Test agains 0 
shapiro.test(d_without_outliers$bias)
wilcox.test(d_without_outliers$bias, mu = 0)

# now predict the bias based on AQ
m_1 <- lm(bias ~ AQ, data= d_without_outliers)
summary(m_1)

ggplot(d_without_outliers, aes(x=AQ, y=bias)) + 
  geom_point()+
  geom_smooth(method = "lm",se = FALSE, color = "darkred")+
  geom_hline(yintercept = 0, linetype='dotted', col = 'black')+
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
  geom_hline(yintercept = 0, linetype='dotted', col = 'black')+
  geom_ribbon(
    aes(ymin = predict(lm(bias ~ AQ_social, data = d_without_outliers), newdata = data.frame(AQ_social = AQ_social), interval = "confidence")[, "lwr"],
        ymax = predict(lm(bias ~ AQ_social, data = d_without_outliers), newdata = data.frame(AQ_social = AQ_social), interval = "confidence")[, "upr"]),
    fill = "lightpink", alpha = 0.5)+
  ylab("Bias [many-morph]") +
  xlab("AQ social") +
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
  geom_hline(yintercept = 0, linetype='dotted', col = 'black')+
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

ggplot(d_without_outliers, aes(x=AQ_attencion_detail, y=bias)) + 
  geom_point()+
  geom_smooth(method = "lm",se = FALSE, color = "darkred")+
  geom_hline(yintercept = 0, linetype='dotted', col = 'black')+
  geom_ribbon(
    aes(ymin = predict(lm(bias ~ AQ_attencion_detail, data = d_without_outliers), newdata = data.frame(AQ_attencion_detail = AQ_attencion_detail), interval = "confidence")[, "lwr"],
        ymax = predict(lm(bias ~ AQ_attencion_detail, data = d_without_outliers), newdata = data.frame(AQ_attencion_detail = AQ_attencion_detail), interval = "confidence")[, "upr"]),
    fill = "lightpink", alpha = 0.5)+
  ylab("Bias [many-morph]") +
  xlab("AQ attencion to detail") +
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

# now predict the bias based on AQ_communication
m_1 <- lm(bias ~ AQ_communication, data= d_without_outliers)
summary(m_1)

ggplot(d_without_outliers, aes(x=AQ_communication, y=bias)) + 
  geom_point()+
  geom_smooth(method = "lm",se = FALSE, color = "darkred")+
  geom_hline(yintercept = 0, linetype='dotted', col = 'black')+
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
  geom_hline(yintercept = 0, linetype='dotted', col = 'black')+
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
