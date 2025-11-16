#####################
### ZOO800: HW 11 ###
#####################

# Author: Rebekkah LaBlue
# Concept: ANCOVA
# Due: November 17, 2025



### --- PACKAGES --- ###

library(tidyverse)
library(dplyr)
library(purrr)
library(magrittr)
library(car)
library(units)
library(stats)
library(ggplot2)
library(viridis)
library(ggfortify)
library(here)



### --- PROBLEM --- ###

# HW Partner: Jillian Neece

### The following simulation data represents the seed density (in seeds per meter-square) 
# at increasing distances (m) from a forest edge. Two conifer seed species were collected 
# in seed traps (0 = lodgepole pine, 1 = Douglas-fir). 

### Ecological Question: Does seed delivery by distance from a forest edge 
# differ between lodgepole pine and Douglas-fir seeds?



### --- DATA, MODEL --- ###

# Import Data #
seed_data <- read.csv("Neece_seed.simulation.csv")
View(seed_data)


# Fit Model #
seed_mod <- lm(seed.delivery ~ distance.to.seed.source.m * species, data = seed_data)
summary(seed_mod)
# The interaction term is significant, so there is no need to reduce the model.


# Visualize #
autoplot(seed_mod, which = 1:6)

ggplot(seed_data, aes(x = distance.to.seed.source.m,
                      y = seed.delivery,
                      color = factor(species))) + 
  geom_point(alpha = 0.7, size = 2) + 
  geom_smooth(method = "lm", se = FALSE, line.width = 1) +
  scale_color_viridis_d(option = "viridis", name = "Species", labels = c("Lodgepole Pine", "Douglas-fir")) +
  labs(
    x = "Distance from forest edge (m)",
    y = "Seed delivery (seeds/m^2)"
  )



### --- INTERP --- ###

# Statistical interpret

### At 0 m of distance from the forest edge the estimated seed density of lodgepole pine is approx. 23 seeds/m^2.
### Lodgepole seed density decreases by about 1.50 seeds/m^2 with every additional meter of distance from the forest edge.
### At 0 m of distance from the forest edge there are approx. 9.8 more douglas-fir seeds/m^2 than lodgepole seeds/m^2 [different intercepts].
### Douglas-fir seeds/m^2 density still decreases with distance (lodgepole slope + douglas-fir slope, ie. -1.50 + 0.4278 = -1.9278), 
# but less steeply than than lodgepole seed density decreases [different slopes].


# Ecological interpret

# The efficacy of seed delivery declines for both Lodgepole and Douglas-fir trees with increased distance from the forest edge, 
# but that decline is more pronounced for Lodgepole pine. This marked difference in dispersal may indicate fundamental differences
# in each species' biology that reflect adaptation to different environmental conditions.