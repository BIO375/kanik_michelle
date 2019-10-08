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

#mutate
SalmonColor<-mutate(SalmonColor, squareroot_skinColor = sqrt(skinColor))

SalmonColor<-mutate(SalmonColor, log(skinColor))


# a
# List two methods that would be appropriate to test whether there was a difference 
# in mean skin color btwn the 2 groups

## two-sample t-test, 


# b
# Use a transformation to test whether there is a difference in mean between these two
#groups. Is there a difference in the mean of kokanee and sockeye skin color?



# ch12 problem 25 ####

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
Clearcuts <- read_csv("datasets/abd/chapter13/chap13q25Clearcuts.csv")





#Test whether there is a change in biomass of rainforest areas following clear-cutting






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





# Review Problems 2- #15 ####

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






