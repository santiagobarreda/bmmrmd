
# Initial set up

library (brms)
library (bmmb)
library (lme4)
options (contrasts = c("contr.sum", "contr.sum"))

# Load and see data
exp_data = read.csv ("exp_data.csv")
head (exp_data)


# Fit lmer model

lmer_model = lme4::lmer (
  height ~ vtl + C + vtl:C + (vtl + C + vtl:C|L) + (1|S),
  data = exp_data)

# error you will see:
# boundary (singular) fit: see help('isSingular')

# saveRDS (lmer_model, 'lmer_model.RDS')
# lmer_model = readRDS ('lmer_model.RDS')

summary (lmer_model)

# Fit brms model

brms_model = brm (
  height ~ vtl + C + vtl:C + (vtl + C + vtl:C|L) + (1|S),
  data = exp_data, 
  prior = c(set_prior("normal(162, 6)", class = "Intercept"),
            set_prior("normal(0, 6)", class = "b"),
            set_prior("normal(0, 6)", class = "sd"),
            set_prior("lkj_corr(2)", class = "cor")),
  chains = 4, 
  cores = 4,
  thin = 1,
  iter = 2000,
  warmup = 1000
  )

# saveRDS (brms_model, 'brms_model.RDS')
# brms_model = readRDS ('brms_model.RDS')

brms_model


# Inspect fixed effects

fixef (brms_model)

fixef_samples = fixef (brms_model, summary = FALSE)
head (fixef_samples)


hypothesis (brms_model, c("C1=0","C2=0","C3=0","-(C1+C2+C3)=0"))


bmmb::short_hypothesis (brms_model, c("C1=0","C2=0","C3=0","-(C1+C2+C3)=0"))


short_hypothesis (
  brms_model, 
  c("Intercept = 0",                       # overall intercept
    "Intercept + C1 = 0",                  # group 1 mean
    "Intercept + C2 = 0",                  # group 2 mean
    "Intercept + C3 = 0",                  # group 3 mean
    "Intercept + -(C1+C2+C3) = 0",         # group 4 mean
    "vtl = 0",                             # overall slope
    "vtl + vtl:C1 = 0",                    # group 1 slope
    "vtl + vtl:C2 = 0",                    # group 2 slope
    "vtl + vtl:C3 = 0",                    # group 3 slope   
    "vtl + -(vtl:C1+vtl:C2+vtl:C3) = 0"))  # group 4 slope

group_means = cbind(
  fixef_samples[,"Intercept"]+fixef_samples[,"C1"],
  fixef_samples[,"Intercept"]+fixef_samples[,"C2"],
  fixef_samples[,"Intercept"]+fixef_samples[,"C3"],
  fixef_samples[,"Intercept"]+ - (fixef_samples[,"C1"]+fixef_samples[,"C2"]+fixef_samples[,"C3"])
  )

head (group_means)

posterior_summary (group_means)

mean (group_means[,1] < 150)
mean (group_means[,1] - group_means[,2])
mean ((group_means[,1] - group_means[,2]) > 0)
posterior_summary (group_means[,1] - group_means[,2])

par (mar = c(4,4,1,1))
brmplot (fixef (brms_model), omit = 1)

mean_summary = posterior_summary (group_means)

bmmb::brmplot (mean_summary)



# Plot comparison of lmer and brms

pts = lme4::fixef (lmer_model)[-1]
err_bars = summary (lmer_model)$coefficients[-1,2]

fixefs = brms::fixef (brms_model)[-1,]

par (mfrow = c(1,1), mar = c(5,4,1,1))
bmmb::brmplot (brms::fixef (brms_model)[-1,], ylim = c(-13,13),las=2, 
               pch=pchs,lwd=2, ylab = "Centimeters")
points (brms::fixef (brms_model)[-1,1], pch=16,lwd=2,cex=1.5)
points ((1:7)+.2, pts, cex=1.5,lwd=2,col=2,pch=16)
segments((1:7)+.2, pts-2*err_bars,(1:7)+.2, pts+2*err_bars,lwd=2,col=2)


# Inspect random effects


random_intercepts = short_hypothesis(
  brms_model, "Intercept = 0",
  scope = "ranef", group = "L")

random_slopes = short_hypothesis(
  brms_model, "vtl = 0",
  scope = "ranef", group = "L")
  

par (mfrow = c(2,1), mar = c(4,4,1,1))

brmplot (random_intercepts, col = bmmb::cols,
         ylab = "Listener effects (cm)", xlab="Listener")
points (lme4::ranef (lmer_model)$L[,1], pch=4,lwd=2, cex=3, 
        col = bmmb::cols)
abline (h=0,lty=3)
brmplot (random_slopes, col = bmmb::cols, xlab="Listener",
         ylab = "Listener age effects (cm)")
points (lme4::ranef (lmer_model)$L[,2], pch=4,lwd=2, cex=3, 
        col = bmmb::cols)
abline (h=0,lty=3)



random_intercepts = short_hypothesis(
  brms_model, "Intercept = 0",
  scope = "coef", group = "L")

random_slopes = short_hypothesis(
  brms_model, "vtl = 0",
  scope = "coef", group = "L")


par (mfrow = c(2,1), mar = c(4,4,1,1))

brmplot (random_intercepts, col = bmmb::cols,
         ylab = "Listener effects (cm)", xlab="Listener")
abline (h=0,lty=3)
brmplot (random_slopes, col = bmmb::cols, xlab="Listener",
         ylab = "Listener age effects (cm)")
abline (h=0,lty=3)


random_effects = ranef(brms_model)
listener_effects = random_effects$L


random_effects = ranef(brms_model, summary = FALSE)
listener_effects = random_effects$L

























###############################################


# Fit lmer model

lmer_model = lme4::lmer (
  height ~ vtl + C + vtl:C + (vtl + C + vtl:C|L) + (1|S),
  data = exp_data)

# error you will see:
# boundary (singular) fit: see help('isSingular')

# saveRDS (lmer_model, 'lmer_model.RDS')
# lmer_model = readRDS ('lmer_model.RDS')

summary (lmer_model)

# Fit brms model

brms_model = brm (
  height ~ vtl + C + vtl:C + (vtl + C + vtl:C|L) + (1|S),
  data = exp_data, 
  prior = c(set_prior("normal(162, 6)", class = "Intercept"),
            set_prior("normal(0, 6)", class = "b"),
            set_prior("normal(0, 6)", class = "sd"),
            set_prior("lkj_corr(2)", class = "cor")),
  chains = 4, 
  cores = 4,
  thin = 1,
  iter = 2000,
  warmup = 1000
)

# saveRDS (brms_model, 'brms_model.RDS')
# brms_model = readRDS ('brms_model.RDS')

brms_model
