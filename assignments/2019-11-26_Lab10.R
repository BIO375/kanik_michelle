# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

install.packages("ggmosaic")
library("ggmosaic")

install.packages("epitools")
library("epitools")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

### Exact Binomial test ####

# x: the number of observed successes
# n: the number of trials
# p: hypothesized probability of success *Needs to be a number between 0 and 1*
# alternative: the alternative hypothesis (takes "two.sided", "less", or "greater")
# conf.level: confidence level for the CI returned in analysis

model01 <- binom.test(x= 41, n=90, p=0.5, alternative = "greater", conf.level = 0.95 )
model01

#we do not reject the null because p>90, so relative frequency of success is Po
#(binomial test: P > 0.05, n=90)


### Chi-squared goodness of fit ####

# did it by hand

### Chi-squared contingency analysis ####

#by hand
#### 10/10 code runs without breaking ####