library(rstan)

#prepare data
set.seed(123456)
N <- 10000
y <- runif(N, 0, 1)  # generate 10000 random numbers between [0,1]

B0 <- 0.5 # same as Task1
alpha0 <- 6 # choose a value greater than 4
beta0 <- 25
mu0 <- 3

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
  file = "Task2.1_NIG_y[0,1]/Task2.1_NIG.stan",
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