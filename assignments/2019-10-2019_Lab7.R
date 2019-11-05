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
# install.packages(c("purrr", "rlang"))


### Fixed effects ANOVA ####

### Step 1.  Read in and plot data ####
#read in data
waterwaste <-read_csv("datasets/demos/Jaffe.csv", col_types = cols(
  Depth = col_factor() ))

# It is important to read in the predictor as a factor
# Look at the data
head(waterwaste)
summary(waterwaste)

# When you do a normal boxplot, the parasite species names overlap and are illegible
# so add a line that tells ggplot to flip the x and y axes, coord_flip()

#since there are two different y's. Do one and then do the other. 
### ALDRIN #### 
ggplot(waterwaste, aes(x = Depth, y = Aldrin))+
  geom_boxplot() +
  theme_bw() 
ggplot(waterwaste) +
  geom_histogram(aes(Aldrin), binwidth = 0.8)+
  facet_wrap(~Depth)
ggplot(waterwaste)+
  geom_qq(aes(sample = Aldrin, color = Depth))

### Step 2.  Construct your ANOVA ####

# Similar to our two-sample t-test, you specify an equation (y~x) and the data

model01 <- lm(Aldrin~Depth, data = waterwaste)

### Step 3.  Check the assumptions, again ####

summ_Aldrin <- waterwaste %>%
  group_by(Depth) %>% 
  summarise(mean_Aldrin = mean(Aldrin),
            sd_Aldrin = sd(Aldrin),
            n_Aldrin = n())

ratio <-(max(summ_Aldrin$sd_Aldrin))/(min(summ_Aldrin$sd_Aldrin))
# ratio is over 3 so you would mutate in later step.


# The function autoplot will give you a residuals by predicte plot, which is 
# called "Residuals vs. Fitted" here.  It also gives you a Q-Q plot of the RESIDUALS.
autoplot(model01)

### Step 4. Interpret results ####
anova(model01)
summary(model01)

# getting data with mutation
waterwaste<-mutate(waterwaste, log_Aldrin= log10(Aldrin))
# now for the data with mutation
model02 <- lm(log_Aldrin~Depth, data = waterwaste)

#Check the assumptions
summ_Aldrin <- waterwaste %>%
  group_by(Depth) %>% 
  summarise(mean_Aldrin = mean(log_Aldrin),
            sd_Aldrin = sd(log_Aldrin),
            n_Aldrin = n())

#ratio
ratio <-(max(summ_Aldrin$sd_Aldrin))/(min(summ_Aldrin$sd_Aldrin))

autoplot(model02)
anova(model02)
summary(model02)

# not specified so you would do unplanned
# Unplanned comparisons
# The key things you need to specify here are the model name and the factor name

tukey <- glht(model02, linfct = mcp(Depth = "Tukey"))
summary(tukey)


### HCB ####
ggplot(waterwaste, aes(x = Depth, y = HCB))+
  geom_boxplot() +
  theme_bw() 
ggplot(waterwaste) +
  geom_histogram(aes(HCB), binwidth = 0.7)+
  facet_wrap(~Depth)
ggplot(waterwaste)+
  geom_qq(aes(sample = HCB, color = Depth))

### Step 2.  Construct your ANOVA 

# Similar to our two-sample t-test, you specify an equation (y~x) and the data

model03 <- lm(HCB~Depth, data = waterwaste)

### Step 3.  Check the assumptions, again 

summ_HCB <- waterwaste %>%
  group_by(Depth) %>% 
  summarise(mean_HCB = mean(HCB),
            sd_HCB = sd(HCB),
            n_HCB = n())

ratio <-(max(summ_HCB$sd_HCB))/(min(summ_HCB$sd_HCB))

# ratio is under 3 so we do not have to mutate
autoplot(model03)
anova(model03)
summary(model03)

