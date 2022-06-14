#Writing out own stan file
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

#### Stan Structure ####
# functions{
#   // store user declared functions
# }
# data{
#   // declare data being passed to the model
# }
# parameters{
#   // declare parameters to be estimated
# }
# transformed parameters{
#   // apply transformations to parametres (e.g log)
# }
# model{
#   // define the statistical parameters
# }
# generated quantities{
#   // calculate things like posterior predictives
# }

#In C++:
#Everything has to end with `;`
#You have to explicitly declare types and dimensions
#Use `//` instead of `#` for comments

#### Data Types: ####
#real: your basic continuous numbers
#integer: integers! mostly used for indexing

# real x; // a real number
# int n; // an integer
# vector[n] z; // a vector of length n
# int index[n] // an array of n 1-D objectes (I know, I know)

#Core matrix classes are
#vector for column vectors
#row_vector for row vectors
#matrix for 2D matrices -only store real #s
#.* or ./ for elementwise operations
#use ' to transpose, e.g. x' will convert x from a column vector to a row vector

#Arrays
#array[3, 4] real a;
#creates a 3 x 4 array with a 1D real number in each slot
#Think of arrays as storage, matrix as math.

#### Example ####
##### Data #####
arfloundbsai <- read_rds("ARFLOUNDBSAI.rds")
flound <- arfloundbsai$data[[1]]  %>% 
  na.omit() 
flound %>% 
  ggplot(aes(ssb, recruits)) + 
  geom_point(size = 4, shape = 21, fill = "steelblue") + 
  geom_smooth() +
  scale_x_continuous(limits = c(0,NA))

##### Data Block ####
# data{
#   int<lower = 1> n; //number of observations #MEANS N has to >= 1
#   vector[n] spawners; // vector of spawners #Now define spawners of length n
#   vector[n] recruits; // vector of recruits #Same with recruits
# }

# The Model

#simple Beverton-Holt Spawner-Recruit model to these data
#log normally distributed, params are
#r0, h, sigma

#What the model looks like:  
h <-  0.7
r0 <- max(flound$recruits) * .5
ssb0 <- max(flound$ssb) * .75
flound$pred <- (0.8 * r0 * h * flound$ssb) / (0.2 * ssb0 * (1 - h) + (h - 0.2) * flound$ssb)
flound %>% 
  ggplot() + 
  geom_point(aes(ssb, recruits),size = 4) +
  geom_line(aes(ssb, pred, color = "Model guess"),size = 1.5) + 
  scale_color_discrete(name = "") + 
  scale_x_continuous(limits = c(0, NA), name = "Spawners") + 
  scale_y_continuous(name = "Recruits") + 
  theme(legend.position = "top")

##### Transformed data block #####
#Where you are manipulating data

# transformed parameters{
#   vector[n] recruits_hat;
#   vector[n] log_recruits_hat;
#   real r0;
#   real s0;
#   r0 = exp(log_r0);
#   s0 = exp(log_s0);
#   recruits_hat = (0.8 * r0 * h * spawners) ./ (0.2 * s0 * (1 - h) +(h - 0.2) * spawners);
#   log_recruits_hat = log(recruits_hat);
# }

##### Parameters block #####
# parameters{
#   real<lower = 0.2, upper = 1> h; //steepness
#   real log_r0; // unfished recruitment
#   real log_s0; // unfished spawners
#   real<lower = 0> sigma; // observation error
# }

##### Transformed parameters block #####
# transformed parameters{
#   vector[n] recruits_hat;
#   vector[n] log_recruits_hat;
#   real r0;
#   real s0;
#   r0 = exp(log_r0);
#   s0 = exp(log_s0);
#   recruits_hat = (0.8 * r0 * h * spawners) ./ (0.2 * s0 * (1 - h) +(h - 0.2) * spawners);
#   log_recruits_hat = log(recruits_hat);
# }


##### Model #####
#Note: Note naturally vectorized performance!
# model{
#   log_recruits ~ normal(log_recruits_hat - 0.5 * sigma^2, sigma); // bias correction
#   sigma ~ cauchy(0,2.5);
#   log_s0 ~ normal(15,2);
#   log_r0 ~ normal(8,2);
#   h ~ beta(6,2);
# }

##### Generated quantities #####
#Inverse of the likelihood

# generated quantities{
#   vector[n] pp_rhat;
#   for (i in 1:n) {
#     pp_rhat[i] = exp(normal_rng(log_recruits_hat[i] - 0.5 * sigma^2, sigma));
#   }
# }

#Checkout Beverton Holt.stan model

##### Running model #####
chains <- 4
inits <- list(s0 = max(flound$ssb) * 1.5, r0 = max(flound$recruits), h = 0.7)
inits <- lapply(1:chains, function(x) map(inits, jitter,1))
bh_fit <- rstan::stan(
  file = "BevertonHolt.stan",
  data = list(spawners = flound$ssb,
              recruits = flound$recruits,
              n = length(flound$ssb)),
  init = inits,
  chains = chains,
  cores = 1, 
  iter = 10000,
  refresh = 0
)

##### Inits #####
# `init` takes the form of a "list of lists"
# One list per chain
# Inside each list per chain a list of initial parameter values
# Can provide initial values for as few or as many parameters as you want, missing parameters get Stan's defaults
# A reason that centering and scaling thing can be helpful

#Examples of inits you can pass through
inits <- list(s0 = max(flound$ssb) * 1.5, r0 = max(flound$recruits), h = 0.7)
inits <- lapply(1:chains, function(x) lapply(inits, jitter,10))
inits

##### Some weird things in passing data #####
#I lost days of my life to these. 
#If you want to pass a factor to Stan, you need to first convert it to an integer
data = list(x = as.integer(my_factor))


#In R, a scalar (one number) is a vector of length 1.
#This makes Stan mad. 
# data {                
#   int<lower=1> N;      
#   real y[N];
# }

#Suppose N = 1? To pass this to Stan from R
data = list(y = as.array(y))

#Note that 
# real y[N];
# array[N] real y;
# Are the same

##### Looking at model results #####
# Examining Fits
#Even though we're not using rstanarm anymore,
#the diagnostics are still the same. 

check_hmc_diagnostics(bh_fit)

#We can also use `shinystan` to explore our fit

# Looking at our fits
#Let's first examine fit of our estimated spawner recruit data. 
#By default, `stan` returns draws from anything defined in `parameters`,
#`transformed parameters`, and `generated quantities` block

#Let's use `tidybayes` to get our fitted and posterior predictive recruits out. 
#Note ability of `tidybayes` to deal with variables with more than one value

fitted_recruits <-
  tidybayes::gather_draws(bh_fit, recruits_hat[year], pp_rhat[year]) 
head(fitted_recruits,2)

#Let's calculate the mean and 80% credible interval
#for each type of prediction over time
fitted_recruits <-  fitted_recruits %>% 
  group_by(year, .variable) %>%
  summarise(rhat = mean(.value),
            lower = quantile(.value, .055),
            upper = quantile(.value, .945)) %>% 
  ungroup() %>% 
  mutate(year = year + min(flound$year) - 1)


#Why is the posterior predictive so much wider than the mean prediction?
fitted_recruits %>% 
  left_join(flound, by = "year") %>% 
  ggplot() + 
  geom_point(data = flound, aes(ssb, recruits), size = 4, shape = 21, fill = "lightgrey") + 
  geom_ribbon(aes(ssb,ymin = lower, ymax = upper, fill = .variable), alpha = 0.25) + 
  geom_line(aes(ssb,rhat,color = .variable)) + 
  scale_x_continuous(limits = c(0,NA), name = "SSB") +
  scale_y_continuous(name = "Recruits", limits = c(0, NA)) +
  theme(legend.position = "top") + 
  scale_fill_discrete(name = '', labels = c("Posterior Predictive","Mean Prediction")) + 
  scale_color_discrete(name = '',labels = c("Posterior Predictive","Mean Prediction")) + 
  labs(caption = "Bands show 89% credible interval")

#Mean prediction: estimate of true mean 
#Posterior prediction: if you went out to sample- this is the distribution of points you would draw from
#Capturing the data generation process