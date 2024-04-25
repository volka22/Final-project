## Project:  STA 215, Spring 2024, Final Project
# Located:   Volk TCNJ Google Drive
# File Name: template
# Date:      2024_4_24
# Who:       Amanda N. Volk



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
##summary statistics for quantitative variables
mean(data$energy_of_song)
mean(data$new_vs_old_song)
sd(data$energy_of_song)
sd(data$new_vs_old_song)
hist(data$energy_of_song)
hist(data$new_vs_old_song)
summary(data$energy_of_song)
summary(data$new_vs_old_song)

##summary statistics for qualitative variables
table(data$connotation_of_song)
table(data$genre_of_song)

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
energy_of_song <- data.frame()
connnotation_of_song = c(rep("positive", 10), rep("neutral", 10), rep("negative", 10))
energy_of_song = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

plot1 <- ggplot(energy_of_song, aes(x = connotation_of_song, y = energy_of_song, color = connotation_of_song, label = name)) +
  geom_point(size = 3) +
  ggtitle("Energy of Song by Connotation of Song") +
  ylab("Energy") +
  xlab("Connotation") +
  geom_text(nudge_y = 1)
print(plot1)

# BOX PLOT
ggplot(energy_of_song, aes(x = connotation_of_song, y = energy_of_song)) +
  geom_boxplot() +
  labs(title = "Box Plot of the Energy of Songs by the Connotation",
       x = "Connotation of Song",
       y = "Energy of Song") +
  theme_minimal()

##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
# showing the relationship between campaign spending and observed vote share 

linear_plot <- plot(energy_of_song$new_vs_old_song, energy_of_song$new_vs_old_song)
print(linear_plot)

# add x line and y line for means
meany <- mean(energy_of_song$new_vs_old_song)
meanx <- mean(energy_of_song$new_vs_old_song)

abline(h = meanx, col = "black")
abline(v = meany, col = "black")



plot4 <- ggplot(energy_of_song, aes(x = energy_of_song, y = connnotation_of_song, label = name, fill = connnotation_of_song)) +
  geom_point(shape = 21, size = 3, stroke = 0.5) +
  geom_text(size = 3, nudge_x = 1, nudge_y = 0.2) +
  geom_vline(data = mean_energy, aes(xintercept = score, color = connotation_of_song, linetype = connotation_of_song), show.legend = FALSE) +
  geom_text(data = mean_energy, aes(x = score, y = connotation_of_song, label = paste("Mean:", round(energy, 2))), size = 3, hjust = -0.1, color = "black") +
  scale_fill_manual(values = c("positive" = "blue", "neutral" = "orange", "negative" = "green")) +
  scale_y_discrete(limits = rev(levels(energy_of_song$connotation_of_song))) +
  labs(title = "Energy of Song by Connotation of Song", x = "Energy", y = "Connotation")
print(plot4)

##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################

# Plot the residuals
plot(energy_of_song$new_vs_old_song, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(data$connotation_of_song, data$genre_of_song)
