library(lme4)
library(nlme)
library(tidyverse)
library(ggplot2)

# read-in your data
rawdat <- read.table(file = "D:/Research/Ant_GrowthCurve/AndyAnts_ExpIII.csv",
                     sep = ",", header = TRUE, fill = TRUE)
grep()

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
  subset(yr != "17_1") %>%
  subset(Queen != "0Q") %>%
  gather(week, weight, -c(Queen, Worker, trmt, yr)) %>%
  subset(weight != "NA") %>%
  mutate(week = substr(week, 2, 3))
  

## plotting 
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

# for box plot  
dat_long %>%
  subset(week %in% c("w05")) %>%
  ggplot() + 
    geom_boxplot(aes(x = Worker, y = weight))

# for nice and pretty plots with mean and boostraped standard error!!!
dat_long %>%
  subset(week %in% c("w05")) %>% 
  group_by(Worker) %>%
  summarize(weight_avg = mean(weight),
            se = Se_Fun(weight)) %>%
  ggplot() +
    geom_point(aes(x = Worker, y = weight_avg)) +
    geom_errorbar(aes(x = Worker, ymin = weight_avg - se, ymax = weight_avg + se), width = 0.1)
    

## run linear mixed model
# model with only week as the random effect
mod <- lme(weight ~ trmt, random = ~1|week, data = dat_long)
summary(mod)

# model with both week and year as random effects
mod1 <- lmer(weight ~ trmt + (1|week) + (1|yr), data = dat_long)
summary(mod1)


              