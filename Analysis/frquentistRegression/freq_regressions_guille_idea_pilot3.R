## libraries
library(stringr)
library(dplyr)
library(ggplot2)
require(gtsummary)
library(webshot2)
require(tidyverse)
require(jtools)
require(broom.mixed)


root <- rprojroot::is_rstudio_project
basename(getwd())

# load dataframe 
filepath <- root$find_file("pilot_3/dot_morph_first/df_exp_long.Rda")
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

d_without_outliers <- outliers(d, by = "response", sd_out = 2)

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


ggsave("pilot_3/dot_morph_first/hist_morph_first.png", 
       width = 10, height = 6)


## Test agains 0 
shapiro.test(d_without_outliers$bias)
wilcox.test(d_without_outliers$bias, mu = 0)

# now predict the bias based on AQ
d_without_outliers <- d_without_outliers %>%
  mutate(AQ_scaled = AQ-mean(AQ)) %>%
  mutate(sex_code = if_else(sex == 'Male',0,1)) %>%
  mutate(age_scaled = age-mean(age))

m_1 <- lm(bias ~ AQ_scaled + age_scaled + sex_code, data= d_without_outliers)
summary(m_1)

# plot of the regression model

# convert the normalized AQ scores to the original scores
intercept <- coefficients(m_1)[[1]] + coefficients(m_1)[[3]]
slope <- coefficients(m_1)[[2]] 


ggplot(d_without_outliers, aes(x = AQ_scaled, y = bias)) + 
  geom_point(colour = "darkred") +
  geom_abline(
    aes(intercept = intercept, slope = slope),
    color = "darkred", linewidth = 1.5
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("bias") +
  xlab("AQ_scaled") +
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
    axis.title.y = element_text(size = 30)
  )


ggsave("pilot_3/dot_morph_first/scatterplotRegLine_morph_first_AQ.png", 
       width = 7, height = 6)

# another plot

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

d_without_outliers <- d_without_outliers %>%
  mutate(AQ_sc_scaled = AQ_social-mean(AQ_social)) %>%
  mutate(AQ_at_sw_scaled = AQ_attentional_switches-mean(AQ_attentional_switches)) %>%
  mutate(AQ_at_dt_scaled = AQ_attencion_detail-mean(AQ_attencion_detail)) %>%
  mutate(AQ_cm_scaled = AQ_communication-mean(AQ_communication)) %>%
  mutate(AQ_im_scaled = AQ_imagination-mean(AQ_imagination))

m_1 <- lm(bias ~ AQ_sc_scaled + age_scaled + sex_code, data= d_without_outliers)
summary(m_1)

# plot of the regression model

# convert the normalized AQ scores to the original scores
intercept <- coefficients(m_1)[[1]] + coefficients(m_1)[[3]]
slope <- coefficients(m_1)[[2]] 


ggplot(d_without_outliers, aes(x = AQ_sc_scaled, y = bias)) + 
  geom_point(colour = "darkred") +
  geom_abline(
    aes(intercept = intercept, slope = slope),
    color = "darkred", linewidth = 1.5
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("bias") +
  xlab("AQ social scaled") +
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
    axis.title.y = element_text(size = 30)
  )


ggsave("pilot_3/dot_morph_first/scatterplotRegLine_morph_first_AQ_social.png", 
       width = 7, height = 6)


# other plot
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

m_1 <- lm(bias ~ AQ_at_sw_scaled + age_scaled + sex_code, data= d_without_outliers)
summary(m_1)

# plot of the regression model

## next plot, only for morph first (sex_code significant - two intercept)

# convert the normalized AQ scores to the original scores
intercept_male <- coefficients(m_1)[[1]] + coefficients(m_1)[[3]]
intercept_female <- coefficients(m_1)[[1]] + coefficients(m_1)[[3]] + coefficients(m_1)[[4]]
slope <- coefficients(m_1)[[2]] 

ggplot(d_without_outliers, aes(x = AQ_at_sw_scaled, y = bias)) + 
  geom_point(aes(color = factor(sex_code))) +
  geom_abline(
    aes(intercept = intercept_male, slope = slope),
    color = "darkred", linewidth = 1.5
  ) +
  geom_abline(
    aes(intercept = intercept_female, slope = slope),
    color = "red", linewidth = 1.5
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("bias") +
  xlab("AQ at sw scaled") +
  labs(color = "Sex") +
  scale_color_manual(
    values = c("0" = "darkred", "1" = "red"),
    labels = c("male", "female")
  ) +
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
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20)
  )

ggsave("pilot_3/dot_morph_first/scatterplotRegLine_morhp_first_AQ_at_sw.png", 
       width = 7, height = 6)

## next plot, only for many first

# intercept <- coefficients(m_1)[[1]] + coefficients(m_1)[[3]]
# slope <- coefficients(m_1)[[2]] 


# ggplot(d_without_outliers, aes(x = AQ_at_sw_scaled, y = bias)) + 
#   geom_point(colour = "darkred") +
#   geom_abline(
#     aes(intercept = intercept, slope = slope),
#     color = "darkred", linewidth = 1.5
#   ) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   ylab("bias") +
#   xlab("AQ Attentional Sw scaled") +
#   theme(
#     axis.line = element_line(colour = "black"),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank(),
#     plot.margin = margin(1, 1, 1, 1, "cm"),
#     panel.background = element_blank(),
#     axis.title.x = element_text(size = 30),
#     axis.text.x = element_text(size = 30),
#     axis.text.y = element_text(size = 30),
#     axis.title.y = element_text(size = 30)
#   )


# ggsave("pilot_3/dot_many_first/scatterplotRegLine_many_first_AQ_at_sw.png", 
#        width = 7, height = 6)


# other plot
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



# now predict the bias based on AQ_attention_to_detail

m_1 <- lm(bias ~ AQ_at_dt_scaled + age_scaled + sex_code, data= d_without_outliers)
summary(m_1)

# plot of the regression model

# convert the normalized AQ scores to the original scores
intercept <- coefficients(m_1)[[1]] + coefficients(m_1)[[3]]
slope <- coefficients(m_1)[[2]] 


ggplot(d_without_outliers, aes(x = AQ_at_dt_scaled, y = bias)) + 
  geom_point(colour = "darkred") +
  geom_abline(
    aes(intercept = intercept, slope = slope),
    color = "darkred", linewidth = 1.5
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("bias") +
  xlab("AQ att to det scaled") +
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
    axis.title.y = element_text(size = 30)
  )


ggsave("pilot_3/dot_morph_first/scatterplotRegLine_morph_first_AQ_at_dt.png", 
       width = 7, height = 6)


# other plot
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

m_1 <- lm(bias ~ AQ_cm_scaled + age_scaled + sex_code, data= d_without_outliers)
summary(m_1)

# plot of the regression model

# convert the normalized AQ scores to the original scores
intercept <- coefficients(m_1)[[1]] + coefficients(m_1)[[3]]
slope <- coefficients(m_1)[[2]] 


ggplot(d_without_outliers, aes(x = AQ_cm_scaled, y = bias)) + 
  geom_point(colour = "darkred") +
  geom_abline(
    aes(intercept = intercept, slope = slope),
    color = "darkred", linewidth = 1.5
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("bias") +
  xlab("AQ commun scaled") +
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
    axis.title.y = element_text(size = 30)
  )


ggsave("pilot_3/dot_morph_first/scatterplotRegLine_morph_first_AQ_cm.png", 
       width = 7, height = 6)


# other plot

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

m_1 <- lm(bias ~ AQ_im_scaled + age_scaled + sex_code, data= d_without_outliers)
summary(m_1)

# plot of the regression model

# convert the normalized AQ scores to the original scores
intercept <- coefficients(m_1)[[1]] + coefficients(m_1)[[3]]
slope <- coefficients(m_1)[[2]] 


ggplot(d_without_outliers, aes(x = AQ_im_scaled, y = bias)) + 
  geom_point(colour = "darkred") +
  geom_abline(
    aes(intercept = intercept, slope = slope),
    color = "darkred", linewidth = 1.5
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("bias") +
  xlab("AQ imagin scaled") +
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
    axis.title.y = element_text(size = 30)
  )


ggsave("pilot_3/dot_morph_first/scatterplotRegLine_morph_first_AQ_im.png", 
       width = 7, height = 6)


# other plot
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
