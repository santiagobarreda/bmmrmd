
find_likelihood = function (x, from = 0, to = 100){

  mus = seq (from, to, length.out = 500)
  
  # easy way to make zero vector of same length as above
  log_likelihood = mus*0
  
  # add the log-density of all observations. Notice only the 
  # mean changes across iterations of the for loop.
  for (i in 1:length(mus)) log_likelihood[i] = 
    sum (dnorm (x, mus[i], sd(x), log = TRUE))
  
  log_likelihood = log_likelihood - max (log_likelihood)
  likelihood = exp (log_likelihood)
  return (cbind (mus, likelihood))
}

ll = find_likelihood (fake_data, 5,12)

fake_data = rnorm (500, 7.34, 3.11)
fake_data = data.frame (fake_data = fake_data)
mean (fake_data$fake_data)
sd (fake_data$fake_data)

fake_model = brms::brm (fake_data ~ 1, data = fake_data, 
                       family = gaussian, cores = 4, chains = 4)
fake_model

hist (fake_data$fake_data, col = "lightblue", freq=FALSE,
      main = "Fake Data", xlab = "", ylim = c(0, 0.15))
curve (dnorm (x, mean = 7.27, sd = 2.84), add = TRUE, 
       col = "orange", lwd=2)

samples = bmmb::get_samples(fake_model)
hist(samples[,1])

fake_model
mean (samples[,1])
sd (samples[,1])
quantile (samples[,1], c(0.025, 0.975))



nba = read.csv ("nbaplayerdata.csv")

head (nba)
nba$height_cm = nba$height * 2.54

hist (nba$height_cm)


nba_model = brms::brm (height_cm ~ 1, data = nba, 
                       family = gaussian, cores = 4, chains = 4)
nba_model

hist (nba$height_cm, col = "lightblue", freq=FALSE,
      main = "NBA Player Heights", xlab = "Height (cm)", ylim = c(0, 0.05))
curve (dnorm (x, mean = 200.47, sd = 8.54), add = TRUE, 
       col = "orange", lwd=2)

centers = nba[nba$pos == "C",]
center_model = brms::brm (height_cm ~ 1, data = centers, 
                          family = gaussian, cores = 4, chains = 4)
center_model

hist (centers$height_cm, col = "lightblue", freq=FALSE,
      main = "Center Heights", xlab = "Height (cm)", ylim = c(0, 0.18))
curve (dnorm (x, mean =  211.13, sd = 3.46), add = TRUE, 
       col = "orange", lwd=2)

