data {
  int<lower=1> NPOLLS;                 // number of polls
  #int<lower=1> NDAYS;                  // number of days
  int<lower=1> NHOUSES;                // number of polling houses

  vector<lower=0, upper=1>[NPOLLS] y;  // vote shares
  #int<lower=1> day[NPOLLS];            // polling day index
  int<lower=1> j[NPOLLS];              // polling house index
  real<lower=0> mu0;
  real<lower=0> B0;
  #vector[NDAYS] u0;   // prior mean of true values (e.g. rep(prevElectionResult, NDAYS))
  #matrix[NDAYS, NDAYS] B0;             // prior covariance matrix (e.g. identity or RW)
  real<lower=4> alpha_0;               // IG shape
  real<lower=0> beta_0;                // IG scale
}

parameters {
  real alpha;
  vector[NHOUSES] delta_raw;          // raw house effects
  real<lower=0> sigma2;               // error variance
}

transformed parameters {
  vector[NHOUSES] delta;
  vector[NPOLLS] mu;

  // sum-to-zero constraint
  delta = delta_raw - mean(delta_raw);

  for (i in 1:NPOLLS)
    mu[i] = alpha + delta[j[i]];
}

model {
  // prior on latent true values
  alpha ~ normal(mu0, sigma2 * B0);

  // prior on house effects
  delta_raw ~ normal(0, 0.075);

  // prior on noise variance
  sigma2 ~ inv_gamma(alpha_0 / 2, beta_0 / 2);

  // likelihood
  y ~ normal(mu, sqrt(sigma2));
}
