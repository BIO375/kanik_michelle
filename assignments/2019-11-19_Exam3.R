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

#10

aphids <- read_csv("datasets/exams/aphids.csv", 
                   col_types = cols(gall_number = col_factor(levels = c("1", 
                                                                        "2", "3", "4", "5", "6", "7", "8", 
                                                                        "9", "10", "11", "12", "13", "14", 
                                                                        "15", "16", "17", "18", "19", "20", 
                                                                        "21", "22", "23", "24", "25", "26", 
                                                                        "27", "28"))))

model02 <- lme(fixed = thorax_length ~ 1,
               random = ~1|gall_number, data = aphids)

model02_varcomp <- VarCorr(model02)
model02_varcomp

varAmong  <- as.numeric( model02_varcomp[1,1] )
# varAmong 0.40477513

# And then extracting the second entry in the first column and calling it VarWithin
varWithin <- as.numeric( model02_varcomp[2,1] )
# varWithin 0.08380952

# And then doing the math
repeatability <- varAmong / (varAmong + varWithin)
repeatability

#12
# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load ggfortify for plotting
library("ggfortify")

# Load broom to convert statistical objects to tidy tibbles and plotly
# for confidence bands
# If you have not installed broom before, you will need to execute
# install.packages("broom")
library("broom")


# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

DriverVision <- read_csv("datasets/exams/DriverVision.csv")

model01 <- lm(Age ~ Distance, data = DriverVision)

# Autoplot gives you a residual by predicted plot in the upper left panel
autoplot(model01, smooth.colour = NA)

driver_plus <- augment(model01)
ggplot(data = driver_plus)+
  geom_point(aes(x = Distance, y= .resid))

summary(model01)


#11- did not end up using!!
# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load ggfortify for plotting
library("ggfortify")

# Load broom to convert statistical objects to tidy tibbles and plotly
# for confidence bands
# If you have not installed broom before, you will need to execute
# install.packages("broom")
library("broom")


# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

glucose <- read_csv("datasets/exams/glucose.csv", 
                    col_types = cols(patient = col_factor(levels = c("A", 
                                                                     "B", "C", "D", "E", "F", "G", "H", 
                                                                     "I"))))


#### Code runs perfectly 5/5 ####




