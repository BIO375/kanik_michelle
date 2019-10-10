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



# ch12 problem 20 ####
#readindata
SalmonColor <- read_csv("datasets/abd/chapter13/chap13q20SalmonColor.csv")

# Calculate summary statistics
summ_salmoncolor <- SalmonColor %>%
  group_by(species) %>% 
  summarise(mean_skinColor = mean(skinColor),
            sd_skinColor = sd(skinColor),
            n_skinColor = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_salmoncolor$sd_skinColor))/(min(summ_salmoncolor$sd_skinColor))

#mutate
mutate_SalmonColor<-mutate(SalmonColor, squareroot_skinColor = sqrt(skinColor))

mutate_SalmonColor<-mutate(SalmonColor, log_skinColor = log(skinColor))


# Look at histograms, box plots, q-q plots
ggplot(mutate_SalmonColor) +
  geom_histogram(aes(log_skinColor), binwidth = .1)+
  facet_wrap(~species)

ggplot(mutate_SalmonColor) +
  geom_boxplot(aes(x = species, y = log_skinColor))

ggplot(mutate_SalmonColor)+
  geom_qq(aes(sample = log_skinColor, color = species))


# Calculate summary statistics
summ_salmoncolorform <- mutate_SalmonColor %>%
  group_by(species) %>% 
  summarise(mean_log_skinColor = mean(log_skinColor),
            sd_log_skinColor = sd(log_skinColor),
            n_log_skinColor = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_salmoncolor$sd_skinColor))/(min(summ_salmoncolor$sd_skinColor))
ratio <-(max(summ_salmoncolorform$sd_log_skinColor))/(min(summ_salmoncolorform$sd_log_skinColor))

# since they are comparing them to each other (fish color) we use two sided bc we have no reason
# to believe one is more colorful than the other

# Two-sided
t.test(log_skinColor ~ species, data = mutate_SalmonColor, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)


# a
# List two methods that would be appropriate to test whether there was a difference 
# in mean skin color btwn the 2 groups

## two-sample t-test, Welche's


# b
# Use a transformation to test whether there is a difference in mean between these two
#groups. Is there a difference in the mean of kokanee and sockeye skin color?

# Kokanee has significantly more skin color than sockeye (t=12.133, df= 33, p-value= 1.038e-13)


# ch12 problem 25 ####

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
#tidyverse_update()

# To perform sign tests, install and load the package DescTools
# install.packages("DescTools")
library("DescTools")

# read in data
Clearcuts <- read_csv("datasets/abd/chapter13/chap13q25Clearcuts.csv")

# they already calculated diff so you don't have to mutate it


ggplot(Clearcuts) +
  geom_histogram(aes(biomassChange), binwidth = 1)

ggplot(Clearcuts) +
  geom_boxplot(aes(x = "", y = biomassChange))

ggplot(Clearcuts)+
  geom_qq(aes(sample = biomassChange))


# since there is a big skew in histogram you have to mutate it. When looking at
# table in notebook look under "what to do when violated" for Paired t-test. you
# would run *sign test* two-sided becuase you are looking at change between two #'s

# One-sample, Two-sided
SignTest(Clearcuts$biomassChange, 
         alternative = "two.sided", mu = 0, conf.level = 0.95)



#Test whether there is a change in biomass of rainforest areas following clear-cutting
# There is no change in biomass of rainforest areas following clear- cutting
# (S= 21, # of df= 36, p-value = 0.405) (we accepted null)





# ch12 problem 26 ####

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# To perform sign tests, install and load the package DescTools
# install.packages("DescTools")
library("DescTools")

# read in data
ZebraFinchBeaks <- read_csv("datasets/abd/chapter13/chap13q26ZebraFinchBeaks.csv")



# Choose Appropriate method and test whether females preferred one type of male over the other type






# Review Problems 2- #16 #### 

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# To perform sign tests, install and load the package DescTools
# install.packages("DescTools")
library("DescTools")

# read in data






