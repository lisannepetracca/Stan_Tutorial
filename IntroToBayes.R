library(tidyverse)
library(tidyverse)
library(broom.mixed)
library(rstan)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(gapminder)
library(patchwork)
library(xaringanExtra)
pres_theme <- theme_minimal(base_size = 16) + theme(axis.title = element_text(size = 18))
theme_set(pres_theme)

#MCMC Basics
set.seed(42)
x <- 1:500 #make up independent data
true_param <- .5 # true relationship between x and y
y <- true_param * x # simulate y
steps <- 1e5 # number of MCMC steps to take
param <- rep(0, steps) # vector of parameter values

#Log sum of squares:
old_fit <- log(sum((y - (x * param[1]))^2)) # fit at starting guess

jumpyness <- 1 # controls random jumping around

for (i in 2:steps){
  proposal <- rnorm(1,param[i - 1], 0.2) # propose a new parameter
  new_fit <- log(sum((y - (x *proposal))^2)) # calculate fit of new parameter
  rand_accept <- log(runif(1,0,jumpyness)) # adjust acceptance a bit
  if (new_fit < (old_fit - rand_accept)){ # accept in proportion to improvment
    param[i] <- proposal
  } else {
    param[i] <- param[i - 1]
  }
}

#quick plot
plot(seq(1,steps), param)

#Better plot:
trace <- tibble(i = 1:steps, param = param)
trace_plot <- trace %>% 
  ggplot(aes(i, param)) + 
  geom_line()

trace_plot

post_plot <- trace %>% 
  filter(i > 0.5 * steps,
         i %in% seq(1, steps, by = 1000)) %>% 
  filter(param < quantile(param, 0.95),
         param > quantile(param, 0.05)) %>% 
  ggplot(aes(param)) + 
  geom_histogram() + 
  geom_vline(aes(xintercept = true_param), color = "red")

trace_plot + post_plot & theme_minimal()