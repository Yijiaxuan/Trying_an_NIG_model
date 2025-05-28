// saved as Task5.stan
data {
  int<lower=0> NPOLLS;  
  vector<lower=0,upper=1>[NPOLLS] y;
  real<lower=0> B0;
  real<lower=4> alpha0;          
  real<lower=0> beta0;
  #real<lower=0> mu0;
  
  int<lower=1> NDAYS; // number of days
  int<lower=1> day[NPOLLS];
  // election result for start point
  real<lower=0,upper=1> prevElectionResult; // historical election results
  
  // upper bound of tau_upper
  real tau_upper;
}
  

parameters{
  
  real<lower=0> sigma2;
 
  vector[NDAYS - 1] omega;
  real<lower=0> tau;
}

transformed parameters {
  vector[NDAYS] xi;
  
  // dynamic model, gaussian RW
  xi[1] = prevElectionResult;
  for (i in 2:NDAYS) {
    xi[i] = xi[i - 1] + (tau * omega[i - 1]);
  }
 
}


model{
   // latent state innovations
  omega ~ normal(0.0, 1.0);
  // scale of innovations
  tau ~ uniform(0.0, tau_upper);
  
  sigma2 ~ inv_gamma(alpha0/2, beta0/2); //prior
  
  // Likelihood: NIG marginal
  for (i in 1:NPOLLS)
  y[i] ~ normal(xi[day[i]], sqrt(sigma2 * (1 + B0)));

} 
