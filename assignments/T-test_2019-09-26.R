# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()
library(readr)
birth_diff_data <- read_csv("datasets/demos/birth_diff.csv")
View(birth_diff)

birth_summ <- birth_diff_data %>%
summarise(
mean_resp = mean(birth_diff),
median_resp = median(birth_diff),
IQR_resp = IQR(birth_diff),
sd_resp = sd(birth_diff),
var_resp = var(birth_diff)
)

# Compare the histograms and boxplots of EGGS and squareroot_eggs
ggplot(birth_diff_data) +
  geom_histogram(aes(birth_diff), binwidth = 1)

ggplot(birth_diff_data)+
  geom_boxplot(aes(x = "", y = birth_diff))
