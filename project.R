packages <- c("tidyverse", "brms", "bayesplot", "loo", "readr")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

library(tidyverse)
library(brms)
library(bayesplot)
library(loo)

# -----------------------------------------
# Step 1: Load and Prepare Dataset
# -----------------------------------------
# Sample dataset (replace with real data source)
df <- read_csv("https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/datasets/airquality.csv")

set.seed(123)
df <- df %>%
  mutate(
    Month = factor(Month),
    Resp_Disease = rbinom(n(), size = 1, prob = plogis((Ozone - 30) / 20))
  ) %>%
  drop_na(Ozone, Wind, Temp)

# -----------------------------------------
# Step 2: Fit Bayesian Logistic Regression
# -----------------------------------------
# Goal: Model probability of respiratory disease using Ozone, Wind, Temp
fit_logit <- brm(
  Resp_Disease ~ Ozone + Wind + Temp,
  data = df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 5), class = "b"),
    prior(normal(0, 10), class = "Intercept")
  ),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 123,
  control = list(adapt_delta = 0.95)
)

# -----------------------------------------
# Step 3: Posterior Summary & Diagnostics
# -----------------------------------------
print(summary(fit_logit))

# Plot posterior densities
plot(fit_logit)

# Posterior predictive check
pp_check(fit_logit)

# Trace plots
mcmc_trace(as.array(fit_logit), pars = c("b_Ozone", "b_Wind", "b_Temp"))

# -----------------------------------------
# Step 4: Model Evaluation
# -----------------------------------------
# Leave-One-Out Cross Validation
loo_result <- loo(fit_logit)
print(loo_result)

# -----------------------------------------
# Step 5: Change Point Analysis (Optional Extension)
# -----------------------------------------
# If time-series or monthly data available
# Here we assume Month as a proxy
fit_cp <- brm(
  Resp_Disease ~ Ozone + Wind + Temp + (1 | Month),
  data = df,
  family = bernoulli(),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 123
)

# Compare models
loo_compare(loo(fit_logit), loo(fit_cp))

# -----------------------------------------
# Step 6: Visualizing Posterior Effects
# -----------------------------------------
library(tidybayes)
df %>%
  data_grid(Ozone = seq_range(Ozone, 20), Wind = mean(Wind), Temp = mean(Temp)) %>%
  add_fitted_draws(fit_logit, re_formula = NA) %>%
  ggplot(aes(x = Ozone, y = .value)) +
  stat_lineribbon(aes(y = .value), .width = c(0.66, 0.89, 0.95)) +
  scale_fill_brewer() +
  labs(title = "Posterior Predictive Effect of Ozone", y = "Pr(Respiratory Disease)")

# -----------------------------------------
# End of Script
# -----------------------------------------
# Replace the CSV with actual air quality + health dataset for real results.

