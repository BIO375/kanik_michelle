
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
fowler <- read_csv("datasets/demos/fowler.csv")
head(fowler)


#linear regression- linear relationship of data. amount of fertilizer should influence about of yield

model01 <- lm(YIELD ~ FERTILIZER, data = fowler)

# Autoplot gives you a residual by predicted plot in the upper left panel
autoplot(model01, smooth.colour = NA)

# Option 1. Use the function augment.

fowler_plus <- augment(model01)
ggplot(data = fowler_plus)+
  geom_point(aes(x = YIELD, y= .resid))

 #blob of data, no shape, assumption is met

summary(model01)

# under "estimate is where you get Bo/b1. So for this example 
#b0 is Intercept as intercept
# B1 is slope which is FERTILIZER


