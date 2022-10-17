

library (brms)
library (bmmb)
options (contrasts = c('contr.sum','contr.sum'))

data (exp_ex)

# create our dependent variables
exp_ex$Female = ifelse(exp_ex$G == 'f', 1, 0)

# create veridical gender predictor 
exp_ex$F_v = ifelse (exp_ex$G_v=="f", 1,-1)

# new dependent variable
exp_ex$y = cbind(b = as.numeric(exp_ex$C=='b'),
                   g = as.numeric(exp_ex$C=='g'),
                   m = as.numeric(exp_ex$C=='m'),
                   w = as.numeric(exp_ex$C=='w'))

# variable representing the size (n) of each observation. They are all 1.
exp_ex$size = 1


# center vtl
exp_ex$vtl_original = exp_ex$vtl
exp_ex$vtl = exp_ex$vtl - mean (exp_ex$vtl)

# center and scale f0
exp_ex$f0_original = exp_ex$f0 
exp_ex$f0 = exp_ex$f0 - mean(exp_ex$f0)
exp_ex$f0 = exp_ex$f0 / 100

# new dependent variable: Size Group
SG = 0
SG[exp_ex$C=='g'] = 1
SG[exp_ex$C=='b'] = 1
SG[exp_ex$C=='w'] = 2
SG[exp_ex$C=='m'] = 3
exp_ex$SG = SG



###################################################################
###################################################################
### Chapter 3
###################################################################

men = exp_ex[exp_ex$C_v=='m',]

model_priors_ex =  
  brms::brm (height ~ 1, data = men, chains = 4, cores = 4,
             warmup = 1000, iter = 3500, thin = 2,
             prior = c(brms::set_prior("normal(176, 15)", class = "Intercept"),
                       brms::set_prior("normal(0, 15)", class = "sigma")))

model_priors_ex = brms::add_criterion(model_priors_ex, "loo")

# saveRDS (model_priors_ex, '3_model_priors_ex.RDS')


###################################################################
###################################################################
### Chapter 4
###################################################################

men = exp_ex[exp_ex$C_v=='m',]

model_multilevel_L_S_ex =  brms::brm (
  height ~ 1 + (1|L) + (1|S), data = men, chains = 4, cores = 4,
  warmup = 1000, iter = 3500, thin = 2,
  prior = c(brms::set_prior("normal(176, 15)", class = "Intercept"),
            brms::set_prior("normal(0, 15)", class = "sd"),
            brms::set_prior("normal(0, 15)", class = "sigma")))

model_multilevel_L_S_ex = brms::add_criterion(model_multilevel_L_S_ex, "loo")

# saveRDS (model_multilevel_L_S_ex, '../models/4_model_multilevel_L_S_ex.RDS')



###################################################################
###################################################################
### Chapter 5
###################################################################

options (contrasts = c('contr.sum','contr.sum'))
notmen = exp_ex[exp_ex$C_v!='m' & exp_ex$C!='m',]

model_sum_coding_t_ex =  brms::brm (
  height ~ A + (1|L) + (1|S), data = notmen, chains = 4, 
  cores = 4, warmup = 1000, iter = 3500, thin = 2, family="student",
  prior = c(brms::set_prior("student_t(3, 156, 12)", class = "Intercept"),
            brms::set_prior("student_t(3, 0, 12)", class = "b"),
            brms::set_prior("student_t(3, 0, 12)", class = "sd"),
            brms::set_prior("gamma(2, 0.1)", class = "nu"),
            brms::set_prior("student_t(3, 0, 12)", class = "sigma")))

model_sum_coding_t_ex = brms::add_criterion(model_sum_coding_t_ex, "loo")

# saveRDS (model_sum_coding_t_ex, '../models/5_model_sum_coding_t_ex.RDS')



###################################################################
###################################################################
### Chapter 6
###################################################################

options (contrasts = c('contr.sum','contr.sum'))
notmen = exp_ex[exp_ex$C_v!='m' & exp_ex$C!='m',]

priors = c(brms::set_prior("student_t(3,156, 12)", class = "Intercept"),
           brms::set_prior("student_t(3,0, 12)", class = "b"),
           brms::set_prior("student_t(3,0, 12)", class = "sd"),
           brms::set_prior("lkj_corr_cholesky (2)", class = "cor"), 
           brms::set_prior("gamma(2, 0.1)", class = "nu"),
           brms::set_prior("student_t(3,0, 12)", class = "sigma"))

model_re_t_ex =  
  brms::brm (height ~ A + (A|L) + (1|S), data = notmen, chains = 4, 
             cores = 4, warmup = 1000, iter = 5000, thin = 4, 
             prior = priors, family = "student")

model_re_t_ex = brms::add_criterion(model_re_t_ex, "loo")

# saveRDS (model_re_t_ex, '../models/6_model_re_t_ex.RDS')


###################################################################
###################################################################
### Chapter 7
###################################################################

options (contrasts = c('contr.sum','contr.sum'))

priors = c(brms::set_prior("student_t(3,156, 12)", class = "Intercept"),
           brms::set_prior("student_t(3,0, 12)", class = "b"),
           brms::set_prior("student_t(3,0, 12)", class = "sd"),
           brms::set_prior("lkj_corr_cholesky (2)", class = "cor"), 
           brms::set_prior("gamma(2, 0.1)", class = "nu"),
           brms::set_prior("student_t(3,0, 12)", class = "sigma"))

model_interaction_ex =  
  brms::brm (height ~ A + G + A:G + (A + G + A:G|L) + (1|S), 
             data = exp_ex, chains = 4, cores = 4, warmup = 1000, 
             iter = 5000, thin = 4, prior = priors, family = "student")

model_interaction_ex = brms::add_criterion(model_interaction_ex, "loo")

# saveRDS (model_interaction_ex, '../models/7_model_interaction_ex.RDS')



###################################################################
###################################################################
### Chapter 8
###################################################################

options (contrasts = c('contr.sum','contr.sum'))

model_formula = brms::bf(height ~ A*G + (A*G|x|L) + (1|S),
                         sigma ~ A + (A|x|L))
priors = 
  c(set_prior("student_t(3, 156, 12)", class = "Intercept"),
    set_prior("student_t(3, 0, 12)", class = "b"),
    set_prior("student_t(3, 0, 12)", class = "sd"),
    set_prior("gamma(2, 0.1)", class = "nu"),
    set_prior("normal(0, 1.5)", class = "Intercept", dpar = "sigma"),
    set_prior("normal(0, 1.5)", class = "b", dpar = "sigma"),
    set_prior("normal(0, 1.5)", class = "sd", dpar = "sigma"),
    set_prior("lkj_corr_cholesky (2)", class = "cor"))

model_A_L_sigma_ex = 
  brms::brm (model_formula, data = exp_ex, chains = 4, cores = 4,
             warmup = 1000, iter = 3500, thin = 2, family="student",
             prior = priors)

model_A_L_sigma_ex = brms::add_criterion(model_A_L_sigma_ex, "loo")

# saveRDS (model_A_L_sigma_ex, '../models/8_model_A_L_sigma_ex.RDS')


###################################################################
###################################################################
### Chapter 9
###################################################################


options (contrasts = c('contr.sum','contr.sum'))

priors = c(brms::set_prior("student_t(3,160, 12)", class = "Intercept"),
           brms::set_prior("student_t(3,0, 12)", class = "b"),
           brms::set_prior("student_t(3,0, 12)", class = "sd"),
           brms::set_prior("lkj_corr_cholesky (2)", class = "cor"), 
           brms::set_prior("gamma(2, 0.1)", class = "nu"),
           brms::set_prior("student_t(3,0, 12)", class = "sigma"))

model_random_slopes_complex_ex =  
  brms::brm (height ~ vtl*A*G + (vtl*A*G|L) + (1|S), data = exp_ex, 
             chains = 4, cores = 4, warmup = 1000, iter = 5000, thin = 4, 
             prior = priors, family = "student")#, control = list(adapt_delta = 0.95))

model_random_slopes_complex_ex = brms::add_criterion(model_random_slopes_complex_ex, "loo")

##  saveRDS (model_random_slopes_complex_ex, '../models/9_model_random_slopes_complex_ex.RDS')



###################################################################
###################################################################
### Chapter 10
###################################################################

options (contrasts = c('contr.sum','contr.sum'))

priors = c(brms::set_prior("student_t(3, 0, 3)", class = "Intercept"),
           brms::set_prior("student_t(3, 0, 3)", class = "b"),
           brms::set_prior("student_t(3, 0, 3)", class = "sd"),
           brms::set_prior("lkj_corr_cholesky (2)", class = "cor"))

model_gender_vtl_ex =
  brm (Female ~ vtl*A + (vtl*A|L) + (1|S), data=exp_ex, chains=4, cores=4, 
       family="bernoulli", warmup=1000, iter= 5000, thin = 4,prior=priors)

model_gender_vtl_ex = brms::add_criterion(model_gender_vtl_ex, "loo")

# saveRDS (model_gender_vtl_ex, '../models/10_model_gender_vtl_ex.RDS')


model_gender_dt_ex =
  brm (Female ~ F_v*A_v + (F_v*A_v|L) + (1|S), data=exp_ex, 
       chains=4, cores=4, family="bernoulli", 
       warmup=1000, iter = 5000, thin = 4,  
       prior = c(set_prior("student_t(3, 0, 3)", class = "Intercept"),
                 set_prior("student_t(3, 0, 3)", class = "b"),
                 set_prior("student_t(3, 0, 3)", class = "sd"),
                 set_prior("lkj_corr_cholesky (2)", class = "cor")))

model_gender_dt_ex = brms::add_criterion(model_gender_dt_ex, "loo")

# saveRDS (model_gender_dt_ex, '../models/10_model_gender_dt_ex.RDS')




###################################################################
###################################################################
### Chapter 11
###################################################################


options (contrasts = c('contr.sum','contr.sum'))

priors = c(brms::set_prior("student_t(3,0, 12)", class = "Intercept"),
           brms::set_prior("student_t(3,0, 12)", class = "b"),
           brms::set_prior("student_t(3,0, 12)", class = "sd"),
           brms::set_prior("lkj_corr_cholesky (2)", class = "cor"), 
           brms::set_prior("gamma(2, 0.1)", class = "nu"),
           brms::set_prior("student_t(3,0, 12)", class = "sigma"))

model_height_vtl_f0_ex =  
  brms::brm (height ~ vtl*f0*A*G + (vtl*f0*A*G|L) + (1|S), data = exp_ex, 
             chains = 4, cores = 4, warmup = 1000, iter = 5000, thin = 4, 
             prior = priors, family = "student")

model_height_vtl_f0_ex = brms::add_criterion(model_height_vtl_f0_ex, "loo")

# saveRDS (model_height_vtl_f0_ex, '../models/11_model_height_vtl_f0_ex.RDS')


model_gender_vtl_f0_reduced_ex =
  brm (Female ~ (vtl+f0)*A + ((vtl+f0)*A|L) + (1|S), data=exp_ex, 
       chains=4, cores=4, family="bernoulli", 
       warmup=1000, iter = 5000, thin = 4,  
       prior = c(set_prior("student_t(3, 0, 3)", class = "Intercept"),
                 set_prior("student_t(3, 0, 3)", class = "b"),
                 set_prior("student_t(3, 0, 3)", class = "sd"),
                 set_prior("lkj_corr_cholesky (2)", class = "cor")))

model_gender_vtl_f0_reduced_ex = brms::add_criterion(model_gender_vtl_f0_reduced_ex, "loo")

# saveRDS (model_gender_vtl_f0_reduced_ex, '../models/11_model_gender_vtl_f0_reduced_ex.RDS')




###################################################################
###################################################################
### Chapter 12
###################################################################


options (contrasts = c('contr.sum','contr.sum'))

multinomial_prior = 
  c(set_prior("student_t(3, 0, 3)", class = "Intercept",dpar="mug"),
    set_prior("student_t(3, 0, 3)", class = "b",dpar="mug"),
    set_prior("student_t(3, 0, 3)", class = "sd",dpar="mug"),
    set_prior("student_t(3, 0, 3)", class = "Intercept",dpar="mum"),
    set_prior("student_t(3, 0, 3)", class = "b",dpar="mum"),
    set_prior("student_t(3, 0, 3)", class = "sd",dpar="mum"),
    set_prior("student_t(3, 0, 3)", class = "Intercept",dpar="muw"),
    set_prior("student_t(3, 0, 3)", class = "b",dpar="muw"),
    set_prior("student_t(3, 0, 3)", class = "sd",dpar="muw"),
    set_prior("lkj_corr_cholesky (2)", class = "cor"))

model_multinomial_ex = 
  brms::brm (y|trials(size) ~ vtl+f0 + (vtl+f0|x|L) + (1|y|S), 
             data=exp_ex, family="multinomial", chains=4, cores=4, 
             warmup=1000, iter = 5000, thin = 4, prior = multinomial_prior)

model_multinomial_ex = brms::add_criterion(model_multinomial_ex, "loo")

#  saveRDS (model_multinomial_ex, "../models/12_model_multinomial_ex.RDS")



model_ordinal_ex = 
  brms::brm (SG ~ vtl+f0 + (vtl+f0|L) + (1|S), data=exp_ex, 
             family="cumulative", chains=4, cores=4, warmup=1000, 
             iter = 5000, thin = 4,
             prior = c(set_prior("student_t(3, 0, 3)", class = "Intercept"),
                       set_prior("student_t(3, 0, 3)", class = "b"),
                       set_prior("student_t(3, 0, 3)", class = "sd"),
                       set_prior("lkj_corr_cholesky (2)", class = "cor")))

model_ordinal_ex = brms::add_criterion(model_ordinal_ex, "loo")

#  saveRDS (model_ordinal_ex, "../models/12_model_ordinal_ex.RDS")



