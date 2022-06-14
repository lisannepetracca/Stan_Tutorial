#link to slides

#https://danovando.github.io/learn-stan/slides#1

library(gapminder)
library(rstanarm)
simple_lifeexp_model <- rstanarm::stan_glm(lifeExp ~ gdpPercap, 
                                           data=gapminder,
                                           refresh=1000,
                                           chains=1)
rstanarm::prior_summary(simple_lifeexp_model)

#You can adjust these, and Stan recommends explicitly setting them since defaults may change.

#see ?rstanarm::priors and the vignette here

lifexp_model <-
  stan_glm(
    log(lifeExp) ~ log(gdpPercap),
    data = gapminder,
    refresh = 0,
    prior = normal(autoscale = TRUE), # prior on the model coefficients
    prior_intercept = normal(autoscale = TRUE), # prior for any intercepts
    prior_aux = exponential(autoscale = TRUE) # in the case prior sigma
  )

rstanarm::prior_summary(lifexp_model)

#Suppose I have a firm belief GDP is completely uncorrelated with life expectancy.

sp_lifexp_model <-
  stan_glm(
    log(lifeExp) ~ log(gdpPercap),
    data = gapminder,
    refresh = 0,
    prior = normal(0,0.025), # prior on the model coefficients
    prior_intercept = normal(autoscale = TRUE), # prior for any intercepts
    prior_aux = exponential(autoscale = TRUE) # in the case prior sigma
  )

plot(sp_lifexp_model, pars = "log(gdpPercap)") #+ labs(title = "Strong Prior")

plot(lifexp_model, pars = "log(gdpPercap)") #+ labs(title = "Weak Prior")

# Stan has numerous built in functions for looking at these diagnostics, 
# and will even include helpful suggestions about them! They generally stat with rstan::check_<something>
  
  rstan::check_hmc_diagnostics(lifexp_model$stanfit)
  
  # The built-in functions are great for diagnosing large numbers of models, make plots, etc.
  # 
  # Stan also comes with a built in model visualizer called shinystan that allows you to 
  # explore all standard model diagnostics, as well as lots of other features
  
  rstanarm::launch_shinystan(lifexp_model)

  sad_model <-
    stan_glmer(
      log(lifeExp) ~ log(gdpPercap) + (year | country),
      data = gapminder,
      cores = 4,
      refresh = 0,
      adapt_delta = 0.2,
      iter = 2000,
      warmup = 100
    )
  
  rstanarm::launch_shinystan(sad_model)
  
  #What happens if we successfully run a bad model?
    
  library(gapminder)
  library(rstanarm)
  hist(log(gapminder$gdpPercap / 1e4))
  gapminder$thing <- log(gapminder$gdpPercap / 1e4)
  bad_model <- rstanarm::stan_glm(thing ~ year, family = gaussian(link = "log"), data = gapminder)
  
  rstanarm::launch_shinystan(bad_model)

  #dist of test statistics in PP checks totally off for this one!  
  
  # For rstanarm models, a lot of the standard methods for getting summary results from regressions 
  # work (e.g. broom::tidy())
  
  library(tidyverse)
  library(listview)
  library(tidybayes)
  library(knitr)
  lifexp_model %>% broom.mixed::tidy()

  #Our simple model has two parameters: (Intercept) and log(gdpPercap)
  
  #Let's pull out the HMC draws for each one (note that with rstanarm you have to use the stanfit object)

 rstan::extract(lifexp_model$stanfit, permute = TRUE) %>% 
  listviewer::jsonedit()  

 #tidybayes is a great package for getting results out of Stan models (and any kind of Bayesian model 
 #actually, even JAGS!)
 
 tidybayes::tidy_draws(lifexp_model) %>% 
   select(1:5) %>% 
   head() %>% 
   knitr::kable(digits = 2, format = "html") 
 
 #spread_draws and gather_draws are more commonly used
 
 #No idea if these names will changes to reflect tidyr 1.0
 tidybayes::gather_draws(lifexp_model,`(Intercept)`,`log(gdpPercap)`)
 
 #ggplot
 tidybayes::gather_draws(lifexp_model,`(Intercept)`,`log(gdpPercap)`) %>% 
   ggplot(aes(.value, fill = factor(.chain))) + 
   geom_density(alpha = 0.75) + 
   facet_wrap(~.variable, scales = "free") + 
   theme_minimal() + 
   theme(legend.position = "top")

 #ggdist
 library(ggdist)
 lifexp_model %>% 
   as.data.frame() %>% 
   ggplot(aes(`log(gdpPercap)`)) + 
   stat_halfeye()

 #statistical tests w stan
 #In a Bayesian world, most statistical tests go from mathematical exercises to data wrangling!
   
#Let's augment our model a bit to play with this idea.

lifexp_model <- stan_glm(log(lifeExp) ~ log(gdpPercap) + country,
                         data = gapminder,
                         refresh = 0,
                         adapt_delta = 0.95,
                         iter = 15000,
                         cores = 4)
#Suppose we wanted to know the probability that the effect of log(gdpPercap) was greater than 0?
  
  lifexp_model %>% 
  tidybayes::tidy_draws() %>% 
  summarise(`Prob. log(gdp) effect is > 0` = mean(`log(gdpPercap)` > 0))

#Suppose we wanted to estimate the mean difference in the intercepts of Europe and the Americas?
    
  lifexp_model %>% 
    tidybayes::gather_draws(`country.*`, regex = TRUE) %>% 
    mutate(country = str_replace(.variable, "country",'')) %>% 
    left_join(gapminder %>% select(country, continent) %>% unique()) %>% 
    group_by(.draw, continent) %>% 
    summarise(mi = mean(.value)) %>% 
    group_by(.draw) %>% 
    summarise(`Continent Difference` = 
                mi[continent == "Europe"] - mi[continent == "Americas"]) -> continent_delta
  head(continent_delta,5) 
  
  #something missing here, he had a prob of having diff intercepts for eur and amer
  hist(continent_delta$`Continent Difference`)
  
  #model comparison w loo
  #elpd_diff measures how much worse a model is than the preferred model
  
  #Should be at least 2 times se_diff to be meaningful
  #model a is worse, best model is on top
  model_a <- stan_glm(log(lifeExp) ~ log(gdpPercap), data = gapminder,
                      refresh = 0)
  model_b <- stan_glm(log(lifeExp) ~ log(gdpPercap):continent, data = gapminder,
                      refresh = 0,
                      iter = 2000)
  loo::loo_compare(loo(model_a), loo(model_b))
  
  # Best way is to write a Stan model is create a new .stan file.
  # 
  # Each stan model is broken into blocks
  # 
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
  #this generated quantities only works on selected draws
  
  #syntax
#   For the most part, most things work like they do in R!
#     
#   e.g. for, if, while work exactly the same
#   x[1,1:5] gives the first five columns of the first row of x
#   Things are indexed at 1!
#     Major differences
#   
#   It's C++, so everything has to end with ;
# 
# You have to explicitly declare types and dimensions
# 
# This is usually the hardest part for people
# // instead of # for comments
  
  # R is very permissive about data types and sizes
  # 
  # real times an integer, no problem
  # Matrix times an array, no problem
  # Automatically creates storage of the right size for you
  # Most languages are not so forgiving, including Stan
  # 
  # Uses strong, static typing
  # You have to explicitly declare variable types (real, integer, logical, etc)
  # You have to explicitly declare their size
  # This and the need to compile seems to be what throws people the most
  # }
  
  # The main ones you will deal with are
  # 
  # real: your basic continuous numbers
  # 
  # integer: integers! mostly used for indexing
  # 
  # real x; // a real number
  # int n; // an integer
  # vector[n] z; // a vector of length n
  # int index[n] // an array of n 1-D objectes (I know, I know)
  # From there, you can store things in matrices or arrays
  
#   Core matrix classes are
#   
#   vector for column vectors
#   row_vector for row vectors
#   matrix for 2D matrices
#   Matrices can only store real numbers (we'll come back to this)
# 
# By default, Stan performs matrix operations (e.g. multiplication) on matrix objects
# 
# .* or ./ for elementwise operations
# use ' to transpose, e.g. x' will convert x from a column vector to a row vector
  
  # Matrices are by definition special classes of 2D objects
  # 
  # Think of them as your basic math building blocks
  # Arrays are a little more permissive: think of them more like lists in R
  # 
  # Can store arbitrary numbers of objects within individual slots (all of the same type)
  # array[3, 4] real a;
  # creates a 3 x 4 array with a 1D real number in each slot
  # 
  # Think of arrays as storage, matrix as math.
  
  # Arrays are the only object type that can store sequences of integers
  # 
  # Suppose that you want to store a sequence of integers to identify a set of columns in a matrix
  # 
  # int n; // number of integers
  # array[n] integer a; // an array with n slots each with an integer in them
  # matrix[2,8] x; // a 2 x 8 matrix
  # x[1,a]; // the entries of x at positions a in row 1
  
  #Once you've written the model, you just need to pass it to rstan::stan
readRDS("ARFLOUNDBSAI.rds")
getwd()

chains <- 4
inits <- list(s0 = max(flound$ssb) * 1.5, r0 = max(flound$recruits), h = 0.7)
inits <- lapply(1:chains, function(x) map(inits, jitter,1))
bh_fit <- rstan::stan(
  file = here::here("src","bh_model.stan"),
  data = list(spawners = flound$ssb,
              recruits = flound$recruits,
              n = length(flound$ssb)),
  init = inits,
  chains = chains,
  cores = 1, 
  iter = 10000,
  refresh = 0
)

#   Stan and HMC seem pretty magic at times, but they are still numerical optimiziers
#   
#   If you start them close to the right values, things will be easier
#   You can pass initial parameter values to Stan using the init command
#   
#   By default Stan does some wizardry behind the scenes to come up with good inits
#   init takes the form of a "list of lists"
#   
#   One list per chain
#   Inside each list per chain a list of initial parameter values
#   Can provide initial values for as few or as many parameters as you want, missing parameters get Stan's defaults
#   A reason that centering and scaling thing can be helpful
  
  #weird things in passing data
  # I lost days of my life to these.
  # 
  # If you want to pass a factor to Stan, you need to first convert it to an integer
  # 
  # data = list(x = as.integer(my_factor))
  # In R, a scalar (one number) is a vector of length 1. This makes Stan mad.
  # 
  # data {                
  #   int<lower=1> N;      
  #   real y[N];
  # }
  # Suppose N = 1? To pass this to Stan from R
  # 
  # data = list(y = as.array(y))
  # Note that
  # 
  # real y[N];
  # array[N] real y;
  # Are the same
  
#   Debugging compiled languages is a skill that takes practice: you can't just go line-by-line and try things!
# 
# There's a "check" button in the top-right corner of .stan files in RStudio
#   
#   This will check if your file is syntactically correct
#   print(this_should_work); prints out the content as the code runs
#   
#   A good way to see if you accidentally have negative fish
#   algorithm = "Fixed_param" is crazy handy
#   
#   Forces Stan to keep sampling from the same initial parameters you pass it
#   Good way to test model behavior under specific parameters
  
  #Even though we're not using rstanarm anymore, the diagnostics are still the same.

check_hmc_diagnostics(bh_fit)

# Let's first examine fit of our estimated spawner recruit data.
# 
# By default, stan returns draws from anything defined in parameters, transformed parameters, and generated quantities block
# 
# Let's use tidybayes to get our fitted and posterior predictive recruits out.
# 
# Note ability of tidybayes to deal with variables with more than one value

fitted_recruits <-
  tidybayes::gather_draws(bh_fit, recruits_hat[year], pp_rhat[year]) 
head(fitted_recruits,2)

#Let's calculate the mean and 80% credible interval for each type of prediction over time

fitted_recruits <-  fitted_recruits %>% 
  group_by(year, .variable) %>%
  summarise(rhat = mean(.value),
            lower = quantile(.value, .055),
            upper = quantile(.value, .945)) %>% 
  ungroup() %>% 
  mutate(year = year + min(flound$year) - 1)
  