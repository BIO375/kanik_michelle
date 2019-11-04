# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates, but not if your internet is bad
tidyverse_update()

# To perform sign tests, install and load the package DescTools
# install.packages("DescTools")
library("DescTools")

# # To simplify summary statistics, install and load the package summarytools
# install.packages("summarytools")
# library("summarytools")

# # For later plotting
# install.packages("Hmisc")
# library(Hmisc)



#Problem 9 ####
yellowness <- read_csv("datasets/exams/feathers.csv")

yellowness <- mutate(yellowness, diff = typical - odd)

ggplot(yellowness) +
  geom_histogram(aes(diff), binwidth = .5)

ggplot(yellowness) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(yellowness)+
  geom_qq(aes(sample = diff))

# Two-sided
t.test(yellowness$typical, yellowness$odd, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)



#problem 10 ####
# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates, but not if your internet is bad
tidyverse_update()

# To perform sign tests, install and load the package DescTools
# install.packages("DescTools")
library("DescTools")


#read in data
vaccine <- read_csv("datasets/exams/baker.csv")

# Since the assumptions of normality apply to differences, use mutate() to add a column called diff.
# Note that here diff = after - before

vaccine <- mutate(vaccine, diff = After - Before)

ggplot(vaccine) +
  geom_histogram(aes(diff), binwidth = 5)

ggplot(vaccine) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(vaccine)+
  geom_qq(aes(sample = diff))

#assumptions not met, have to perform sign test. 
# Two-sided
SignTest(vaccine$diff, alternative = "two.sided", mu = 0, conf.level = 0.95)

#(signtest, two-sided: s=18, n=20, p=0.0004)




#Problem 11 ####
# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates, but not if your internet is bad
tidyverse_update()

# To perform sign tests, install and load the package DescTools
# install.packages("DescTools")
library("DescTools")


#read in data
CO2levels <- read_csv("datasets/demos/CO2levels.csv")

#summary statistics
# Look at the summary statistics
summ_levels <- CO2levels %>%
  group_by(treatment) %>% 
  summarise(mean_levels = mean(growthrate),
            sd_levels = sd(growthrate),
            n_levels = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_levels$sd_levels))/(min(summ_levels$sd_levels))

# Look at histograms, box plots, q-q plots
ggplot(CO2levels) +
  geom_histogram(aes(growthrate), binwidth = .5)+
  facet_wrap(~treatment)

ggplot(CO2levels) +
  geom_boxplot(aes(x = treatment, y = growthrate))

ggplot(CO2levels)+
  geom_qq(aes(sample = growthrate, color = treatment))

# Two-sided
t.test(growthrate ~ treatment, data = CO2levels, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)


#(t=-0.53606, df= 12, p-value= 0.6017)

### NO CODE BREAKS, GOOD JOB!  6/6 PTS ####

