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

#### Simple Regression ####
#Bayesian regression with rstanarm
#predict log(life expectancy) as a function of log(gdp)

#This is the data
gapminder %>% 
  ggplot(aes(log(gdpPercap), log(lifeExp))) + 
  geom_point()

#Runnign simple regression
simple_lifexp_model <- rstanarm::stan_glm(lifeExp ~ gdpPercap,
                                          data = gapminder,
                                          refresh = 1000,
                                          chains = 1)

#rstanarm function regression functions start with stan_MODEL

#rstanarm has some pretty clever selectors for weakly informative priors
#pulls weakly towards 0
#looking at the priors that it selected
rstanarm::prior_summary(simple_lifexp_model)

#### Setting Priors ####

lifexp_model <-
  stan_glm(
    log(lifeExp) ~ log(gdpPercap),
    data = gapminder,
    refresh = 0,
    prior = normal(autoscale = TRUE), # prior on the model coefficients
    prior_intercept = normal(autoscale = TRUE), # prior for any intercepts
    prior_aux = exponential(autoscale = TRUE) # in the case prior sigma
  )

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


p1 <- plot(sp_lifexp_model, pars = "log(gdpPercap)") + 
  labs(title = "Strong Prior")

p2 <- plot(lifexp_model, pars = "log(gdpPercap)") + 
  labs(title = "Weak Prior")

require(gridExtra)
grid.arrange(p1, p2, ncol=2)

#### Stan Diagnostics ####
rstan::check_hmc_diagnostics(lifexp_model$stanfit)

#Shiny stan:
#rstanarm::launch_shinystan(lifexp_model)

#An unhappy MCMC:
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

#What happens if we successfully run a bad model?
library(gapminder)
library(rstanarm)
hist(log(gapminder$gdpPercap / 1e4))
gapminder$thing <- log(gapminder$gdpPercap / 1e4)
bad_model <- rstanarm::stan_glm(thing ~ year, family = gaussian(link = "log"), data = gapminder)

rstanarm::prior_summary(bad_model)
rstanarm::launch_shinystan(bad_model)

#### Extracting Results ####
#broom.mixed
lifexp_model %>% broom.mixed::tidy()

#extract function
rstan::extract(lifexp_model$stanfit, permute = TRUE) %>% 
  listviewer::jsonedit()

#tidybayes
tidybayes::tidy_draws(lifexp_model) %>% 
  select(1:5) %>% 
  head() %>% 
  knitr::kable(digits = 2, format = "html")

tidybayes::gather_draws(lifexp_model,`(Intercept)`,`log(gdpPercap)`)

tidybayes::gather_draws(lifexp_model,`(Intercept)`,`log(gdpPercap)`) %>% 
  ggplot(aes(.value, fill = factor(.chain))) + 
  geom_density(alpha = 0.75) + 
  facet_wrap(~.variable, scales = "free") + 
  theme_minimal() + 
  theme(legend.position = "top")

#tidybayes and ggdist
lifexp_model %>% 
  as.data.frame()

lifexp_model %>% 
  as.data.frame() %>% 
  ggplot(aes(`log(gdpPercap)`)) + 
  stat_halfeye()

#### Statistical Tests ####
#Augmenting our model:
lifexp_model <- stan_glm(log(lifeExp) ~ log(gdpPercap) + country,
                         data = gapminder,
                         refresh = 0,
                         adapt_delta = 0.95,
                         iter = 15000,
                         cores = 4)

#Suppose we wanted to know the probability that the effect of 
#log(gdpPercap) was greater than 0?

lifexp_model %>% 
  tidybayes::tidy_draws() %>% 
  summarise(`Prob. log(gdp) effect is > 0` = mean(`log(gdpPercap)` > 0))


#Suppose we wanted to estimate the mean difference
#in the intercepts of Europe and the Americas?

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


pl <- scales::percent(mean(continent_delta$`Continent Difference` < 0),2)
continent_delta %>% 
  filter(between(`Continent Difference`, quantile(`Continent Difference`,0.055), quantile(`Continent Difference`,0.945))) %>% 
  ggplot(aes(`Continent Difference`)) + 
  geom_vline(aes(xintercept = 0)) + 
  geom_histogram(alpha = 0.5) + 
  scale_x_continuous(name = "Difference in Mean Intercepts of Europe and Americas") + 
  labs(caption = glue::glue("Prob that Europe intercept is less than Americas is {pl}"), title = "89% Credible Distribution of Difference in Mean Intercepts")

#### LOO package ####
model_a <- stan_glm(log(lifeExp) ~ log(gdpPercap), data = gapminder,
                    refresh = 0)
model_b <- stan_glm(log(lifeExp) ~ log(gdpPercap):continent, data = gapminder,
                    refresh = 0,
                    iter = 2000)
loo::loo_compare(loo(model_a), loo(model_b))

#Model B was better than A
#elpd_diff should be more than twice the se_diff