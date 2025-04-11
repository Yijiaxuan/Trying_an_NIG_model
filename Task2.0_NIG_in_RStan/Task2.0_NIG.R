# Here modeling, I canceled y & mu's limited interval [0,1] in Task2_NIG, 
# and my simulating dada y follows normal(mu, sigma2)

# I get psoterior value mu & sigma2 samiliar to Task1.

library(rstan)

#prepare data
set.seed(123456)
N <- 10000
B0 <- 0.5 # same as Task1
alpha0 <- 6 # choose a value greater than 4
beta0 <- 25
mu0 <- 3

g <- rgamma(1, shape=alpha0/2, rate=delta0/2) 
sigma2_true <- 1/g 
mu_true <- rnorm(1, mean = mu0, sd = sqrt(sigma2 * B0))
y <- rnorm(N, mean = mu_true, sd = sqrt(sigma2_true))

#package data
data_list<- list(
  N=N,
  y=y,
  B0=B0,
  alpha0=alpha0,
  beta0=beta0,
  mu0=mu0
)

#Run stan
fit <- stan(
  file = "Task2.0_NIG_in_RStan/Task2.0_NIG.stan",
  data = data_list,
  iter = 6500,   #same as paper
  warmup = 1500,
  chains = 4,
  seed = 123456
)



#check result
print(fit)
plot(fit)
posterior <- extract(fit)
traceplot(fit, pars = c("mu", "sigma2"))
mean(posterior$sigma2)
mean(posterior$mu)
#hist(posterior$mu, main = "Posterior of mu")
#hist(posterior$sigma2, main = "Posterior of sigma2")
summary(fit, pars = c("mu", "sigma2"))$summaryS