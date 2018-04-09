library(lme4)
library(nlme)
library(tidyverse)
library(ggplot2)

# read-in your data
rawdat <- read.table(file = "D:/Research/Ant_GrowthCurve/AndyAnts.csv", sep = ",", header = TRUE, fill = TRUE)

# preping function for calculating SE
Se_Fun <- function (dat){
  means = c()
  for (i in 1:10000){
    means = c(means, mean(sample(dat, length(dat), replace = TRUE)))
  }
  sd(means)
}

# transforming the raw data into long format
dat_long <- rawdat %>%
  subset(yr != 17) %>%
  gather(week, weight, -trmt) %>%
  subset(weight != "NA") %>%
  subset(week != "yr")

# plotting 
dat_long %>% 
  subset(weight != "NA") %>%
  ggplot + 
  geom_point(aes(x = week, y = weight)) +
  geom_smooth(aes(x = week, y = weight, group = trmt)) + 
  facet_wrap(~trmt)

dat_sum <- dat_long %>% 
  group_by(trmt, week) %>%
  summarize(weight_avg = mean(weight),
            se = Se_Fun(weight))

dat_sum %>%  
  ggplot(aes(x = week, y = weight_avg, group = trmt)) + 
  geom_point(aes(color = trmt), size = 2) +
  geom_errorbar(aes(ymin = weight_avg - se, ymax = weight_avg + se, color = trmt), width = 0.1) +
  geom_line(aes(color = trmt)) + 
  facet_wrap(~trmt)
  
# run linear mixed model
mod <- lme(weight ~ trmt, random = ~1|week, data = dat_long)
summary(mod)


              