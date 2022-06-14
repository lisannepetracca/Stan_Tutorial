# BayesianStanWorkshop
Dan Ovando’s Workshop: ‘Fitting Bayesian Models Using R and Stan’ on June 13th from 10 - 2:30 pm.

Instructions for installing rstan are provided in the following wiki
https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

It doesn’t matter if you want to install the development or official release.

Stan is a compiled language, meaning that there is some behind-the-scenes setup to your C++ toolchain that needs to be done so your computer can handle it.

Follow these instructions prior to trying to install rstan.
The wiki contains links to instructions for setting up your C++ toolchain up here.

The instructions will vary depending on what operating system you use, and even the version of that operating system, and the version of R you are using. If you’re usually generally up-to-date systems this should be pretty straightforward, but the older the system you are running the trickier things might get, particularly if you are using R versions < 4.X

If you’ve never run any compiled languages before, the initial setup can take some time (e.g. downloading all the toolchain components on a Mac), so I recommend not trying to do this the morning of the workshop if your goal is to follow along.

Once you’ve configured your toolchain and installed rstan, you can test to make sure everything is working by opening R and running

library(rstan)

example(stan_model, package = "rstan", run.dontrun = TRUE)

If you read through and followed the instructions and done some googling and still are having trouble feel free to get in touch (danovan@uw.edu) and I’ll try and help you out.

Additional packages that will be useful if you don’t have them installed
install.packages("here")

install.packages("tidybayes")

install.packages("gapminder")

install.packages("rstanarm")

Check out: https://github.com/DanOvando/learn-stan
