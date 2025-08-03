data {
  int<lower=1> NPOLLS;             // number of polls
  int<lower=1> NDAYS;              // number of days (time index)
  int<lower=1> NHOUSES;            // number of polling houses

  vector<lower=0, upper=1>[NPOLLS] y;  // vote share observations
  int<lower=1, upper=NDAYS> day[NPOLLS];  // time index
  int<lower=1, upper=NHOUSES> j[NPOLLS];  // pollster index

  real<lower=0> B0;
  real<lower=4> alpha0;            // IG shape
  real<lower=0> beta0;             // IG scale
  real<lower=0, upper=1> prevElectionResult; // xi[1] prior
  real<lower=0> tau_upper;         // RW step size upper bound
}

parameters {
  real<lower=0> sigma2;            // variance

  vector[NDAYS - 1] omega;         // innovations
  real<lower=0> tau;               // step size for trend

  vector[NHOUSES] delta_raw;       // raw house effects (will be centered)
}

transformed parameters {
  vector[NDAYS] xi;                // time trend
  vector[NHOUSES] delta;           // house effects
  vector[NPOLLS] mu;               // latent mean

  // construct time trend
  xi[1] = prevElectionResult;
  for (t in 2:NDAYS) {
    xi[t] = xi[t - 1] + tau * omega[t - 1];
  }

  // enforce sum-to-zero constraint
  delta = delta_raw - mean(delta_raw);

  // construct latent mean for each poll
  for (i in 1:NPOLLS) {
    mu[i] = xi[day[i]] + delta[j[i]];
  }
}

model {
  // priors
  omega ~ normal(0, 1);
  tau ~ uniform(0, tau_upper);
  sigma2 ~ inv_gamma(alpha0 / 2, beta0 / 2);
  delta_raw ~ normal(0, 0.075);

  // likelihood
  y ~ normal(mu, sqrt(sigma2 * (1 + B0)));
}
