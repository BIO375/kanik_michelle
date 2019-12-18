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

# scenario 1 ####
#read in data
insulation<-read_csv("datasets/final/insulation.csv")


# The assumptions for a linear regression are difficult to test directly,
# so we mostly diagnose departures using residuals plots.  In order
# to plot residuals, however, we first need to fit a model!

model01 <- lm(heat_loss ~ leanness, data = insulation)

# Autoplot gives you a residual by predicted plot in the upper left panel
autoplot(model01, smooth.colour = NA)

# Option 1. Use the function augment.

insulation_plus <- augment(model01)
ggplot(data = insulation_plus)+
  geom_point(aes(x = leanness, y= .resid))

# Option 2.  Use the function resid() right in the plotting command
ggplot(data = insulation)+
  geom_point(aes(x = leanness, y = resid(model01)))

# So what are the actual statistical results???
summary(model01)


#results summarized on paper

#scenario 2 ####
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

# read in data
caffeine<-read_csv("datasets/final/caffeine.csv")

#read in data
caffeine <- read_csv("datasets/final/caffeine.csv", 
                     col_types = cols(group = col_factor(levels = c("male", 
                                                                    "norm_prog", "high_prog"))))

# planned multiple comparison of means 
model01 <- lm(half_life~group, data = caffeine)

# Planned comparisons ####
# because they picked before hand that main comparison was done prior
#to 

planned <- glht(model01, linfct = 
                  mcp(group = c("male - norm_prog = 0"
                  )))
confint(planned)
summary(planned)

planned <- glht(model01, linfct = 
                  mcp(group = c("norm_prog - high_prog = 0"
                  )))
confint(planned)
summary(planned)

# scenario 3####
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

 
#read in data
davis<-read_csv("datasets/final/davis.csv")

#### 10/10 code runs without breaking ####

