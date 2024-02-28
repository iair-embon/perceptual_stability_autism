
library(dplyr)

## Show the probability to get a false positive given that participants respond
## randomply to the AQ

columns <- unique(all_results_df$AQ_type)

for (col in columns){
  d <- all_results_df %>%
    filter(AQ_type == col)
  
  prob <- sum(d$ci_contains_zero == FALSE)/100
  
  print(col)
  print(prob)
  print('')
}

## plot
library(ggplot2)
library(dplyr)

# Filter data for AQ_type == "AQ"
aq_data <- all_results_df %>% filter(AQ_type == "AQ_imagination")

# Get counts of ci_contains_zero values
true_count <- sum(aq_data$ci_contains_zero == TRUE)
false_count <- sum(aq_data$ci_contains_zero == FALSE)

# Create histogram
hist_plot <- ggplot(aq_data, aes(x = AQ_slope, fill = factor(ci_contains_zero))) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20) +
  labs(title = "AQ imagination",
       x = "AQ slope", y = "Frequency",
       fill = "significant?") +
  scale_fill_manual(values = c("blue", "red"),
                    labels = c(paste("TRUE (N:", true_count, ")"),
                               paste("FALSE (N:", false_count, ")"))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Show the plot
print(hist_plot)

