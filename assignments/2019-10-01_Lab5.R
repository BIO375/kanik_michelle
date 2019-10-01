# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# To perform sign tests, install and load the package DescTools
#install.packages("DescTools")
library("DescTools")

#read in data
earth_spin_data<-read_csv("datasets/demos/earth_spin_data.csv", col_names = TRUE)

# First step, calculate t_sample.  You will need to define what the sample mean, null hypothesis mean, sample 
# standard deviation, and sample size are.  
null_mean <- 23.4722

# Identify your response variable using the form dataset$variable_name
y<-earth_spin_data$obliquity

# Calculate summary statistics
sample_mean <-mean(y)
sample_sd <- sd(y)
sample_n <- as.numeric(length(y))
df <- sample_n -1

# One-sided, HA that sample mean is greater than null mean
t.test(earth_spin_data$obliquity, 
       alternative = "two.sided", mu = 23.4722, conf.level = 0.95)

#Question2

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# To perform sign tests, install and load the package DescTools
#install.packages("DescTools")
library("DescTools")

#read in data
HeartAttack_short<-read_csv("datasets/demos/HeartAttack_short.csv",col_names = TRUE,
               col_types = cols(
                 group = col_character() )
)


# Look at the summary statistics
summ_cholest <- HeartAttack_short %>%
  group_by(group) %>% 
  summarise(mean_cholest = mean(cholest),
            sd_cholest = sd(cholest),
            n_cholest = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_cholest$sd_cholest))/(min(summ_cholest$sd_cholest))

# Look at histograms, box plots, q-q plots
ggplot(HeartAttack_short) +
  geom_histogram(aes(cholest), binwidth = 10)+
  facet_wrap(~group)

ggplot(HeartAttack_short) +
  geom_boxplot(aes(x = group, y = cholest))

ggplot(HeartAttack_short)+
  geom_qq(aes(sample = cholest, color = group))

# Two-sided
t.test(cholest ~ group, data = HeartAttack_short, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)
