// saved as Task2.stan
data {
  int<lower=0> N;          
  vector<lower=0,upper=1>[N] y;
  real<lower=0> B0;
  real<lower=4> alpha0;          
  real<lower=0> beta0;
}

parameters{
  real<lower=0,upper=1> mu;
  real<lower=0> sigma2;
  real<lower=0.4,upper=0.6> mu0;
}



model{
  mu0 ~ uniform(0.4, 0.6);  //prior,can be obmit
  mu ~ normal(mu0,sqrt(sigma2*B0));  //prior
  sigma2 ~ inv_gamma(alpha0/2, beta0/2); //prior
  y ~ normal(mu,sqrt(sigma2));  //Likelihood
}
