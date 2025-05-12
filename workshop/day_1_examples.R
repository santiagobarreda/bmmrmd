
# Generate data

set.seed (1)

womens_height = data.frame (height = rnorm (30, 193, 8))

par (mar = c(4,4,1,1))
hist (womens_height$height, col = "lightblue",main="")


# Traditional tests

t.test (womens_height$height)

OLS_model = lm (height ~ 1, data = womens_height)
summary (OLS_model)

# Bayesian model (weak prior)

library (brms)

height_model = brm (
  height ~ 1,
  data = womens_height, chains = 4, cores = 4,
  prior = c(set_prior("normal(162, 10)", class = "Intercept"))
  )

saveRDS (height_model, 'height_model.RDS')
height_model = readRDS ('height_model.RDS')

height_model


# Bayesian model (strong prior)

height_model_prior = brm (
  height ~ 1,
  data = womens_height, chains = 4, cores = 4,
  prior = c(set_prior("normal(162.1, 0.14)", class = "Intercept"))
)

saveRDS (height_model_prior, 'height_model_prior.RDS')
height_model_prior = readRDS ('height_model_prior.RDS')

height_model_prior


# Plot of priors, posteriors, and likelihoods

heights = seq(130,200,.1)
likelihoods = heights*0
for (i in heights){
  likelihoods[which(heights == i)] = 
    prod(dnorm (womens_height$height, mean = i, sd = 8))
}

prior_1 = dnorm (heights, 162,10)
posterior_1 = likelihoods * prior_1

prior_2 = dnorm (heights, 162.1,.14)
posterior_2 = likelihoods * prior_2

posterior_2 = posterior_2 / max (posterior_2)
prior_2 = prior_2 / max (prior_2)
posterior_1 = posterior_1 / max (posterior_1)
prior_1 = prior_1 / max (prior_1)
likelihoods = likelihoods / max (likelihoods)

par (mfrow = c(2,1), mar = c(4,4,1,1))
plot (heights, likelihoods, type = 'l', lwd=2)
lines (heights, prior_1, col = 'red', lwd=2)
lines (heights, posterior_1, col = 'blue',lwd=2)

plot (heights, likelihoods, type = 'l', lwd=2)
lines (heights, prior_2, col = 'red', lwd=2)
lines (heights, posterior_2, col = 'blue',lwd=2)

