# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Install package ggfortify, *note* only do install.packages ONCE
# ggfortify is a package that works with ggplot2 to make nice plots
# install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
# install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
# install.packages("nlme")
library("nlme")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()


#### Problem 15-22 ####
# Complete parts a, b, c, d

walkingstick <- read_csv("datasets/abd/chapter15/chap15q22WalkingStickHeads.csv", 
                         col_types = cols(specimen = col_factor(levels = c("1", 
                                                                           "2", "3", "4", "5", "6", "7", "8", 
                                                                           "9", "10", "11", "12", "13", "14", 
                                                                           "15", "16", "17", "18", "19", "20", 
                                                                           "21", "22", "23", "24", "25"))))
#Random effects ANOVA ####
## The random effects ANOVA function requires two formulas, rather than just one. 
model02 <- lme(fixed = headwidth ~ 1,
               random = ~1|specimen, data = walkingstick)

# Obtain the variance components for the random effects using VarCorr.
model02_varcomp <- VarCorr(model02)
model02_varcomp
# This gives us the estimates of the variance components for groups/Intercept and 
# error/Residual


# To get repeatibility, tell R to do some math by extracting the first entry in the first
# column and calling it VarAmong

varAmong  <- as.numeric( model02_varcomp[1,1] )
# varAmong 0.00024

# And then extracting the second entry in the first column and calling it VarWithin
varWithin <- as.numeric( model02_varcomp[2,1] )
# varWithin 0.00016

# And then doing the math
repeatability <- varAmong / (varAmong + varWithin)
repeatability

#in this context, opposite of measurement error: about 60% variability 
#among walkingsticks
#0.597

#d.
#74% of walking stick femur length is due to variability among actual
# insects while 60% variabiliy in headwidth. So femur length has higher repeatability. 
# The higher the repeatability the less affected by measurement error,
#so headwidth is more affected by measurement error

#### Problem 15-23 ####
# Complete parts a only

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()
# Load tidyverse
library("tidyverse")

#read in data
pinecone <- read_csv("datasets/abd/chapter15/chap15q23LodgepolePineCones.csv", 
                     col_types = cols(habitat = col_factor(levels = c("island.absent", 
                                                                      "island.present", "mainland.present"))))


# planned multiple comparison of means 
model01 <- lm(conemass~habitat, data = pinecone)

# Planned comparisons ####
# because they picked before hand that main comparison was done prior
#to 

planned <- glht(model01, linfct = 
                  mcp(habitat = c("island.present - island.absent = 0"
                                  )))
confint(planned)
summary(planned)


#### Problem 15-26 ####
# Use the data to perform the correct test.  Please show code for all steps in your process.

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()
# Load tidyverse
library("tidyverse")

#read in data
fungus <- read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv", 
                   col_types = cols(treatmentGroup = col_factor(levels = c("Control", 
                                                                           "WT", "Scorpine"))))

#1-way fixed ANOVA #### 
ggplot(fungus, aes(x = treatmentGroup, y = logSporozoiteNumbers))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(fungus) +
  geom_histogram(aes(logSporozoiteNumbers), binwidth = 1)+
  facet_wrap(~treatmentGroup)
ggplot(fungus)+
  geom_qq(aes(sample = logSporozoiteNumbers, color = treatmentGroup))

#specify an equation (y~x) and the data

model01 <- lm(logSporozoiteNumbers~treatmentGroup, data = fungus)

#Check the assumptions 
summ_logSporozoiteNumbers <- fungus %>%
  group_by(treatmentGroup) %>% 
  summarise(mean_logSporozoiteNumbers = mean(logSporozoiteNumbers),
            sd_logSporozoiteNumbers = sd(logSporozoiteNumbers),
            n_logSporozoiteNumbers = n())
ratio <-(max(summ_logSporozoiteNumbers$sd_logSporozoiteNumbers))/(min(summ_logSporozoiteNumbers$sd_logSporozoiteNumbers))

#Q-Q plot of the RESIDUALS.
autoplot(model01)
anova(model01)
summary(model01)

# Unplanned comparisons
# The key things you need to specify here are the model name and the factor name

tukey <- glht(model01, linfct = mcp(treatmentGroup = "Tukey"))
summary(tukey)


#### Problem 15-30 and/or 15-31 (same data in both problems) ####
# Use the data to perform the correct test.  Please show code for all steps in your process.

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()
# Load tidyverse
library("tidyverse")

# read in data
crab <- read_csv("datasets/abd/chapter15/chap15q30FiddlerCrabFans.csv", 
                 col_types = cols(crabType = col_factor(levels = c("female", 
                                                                   "intact male", "male minor removed", 
                                                                   "male major removed"))))
