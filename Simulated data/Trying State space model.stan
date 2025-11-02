data {
  // sizes
  int<lower=1> NPOLLS;                // number of polls
  int<lower=1> NDAYS;                 // number of days
  int<lower=1> NHOUSES;               // number of polling houses
  int<lower=0> NBREAKS;               // number of discontinuity types

  // per-poll known standard deviation (sampling error etc.)
  vector<lower=0>[NPOLLS] sigma;

  // poll data
  vector<lower=0, upper=1>[NPOLLS] y; // vote share (proportions)
  int<lower=1, upper=NHOUSES> j[NPOLLS];
  int<lower=1, upper=NDAYS>   day[NPOLLS];

  // discontinuity index per day:
  // 0 = no break on that day; k in {1..NBREAKS} selects gamma[k]
  int<lower=0, upper=NBREAKS> discontinuity[NDAYS];

  // anchor for start point
  real<lower=0, upper=1> prevElectionResult;

  // bound for random-walk step size
  real<lower=0> tau_upper;
}

parameters {
  vector[NHOUSES] delta_raw;          // raw house effects, to be centered
  vector[NDAYS - 1] omega;            // RW innovations
  vector[NBREAKS] gamma;              // discontinuity shocks
  real<lower=0, upper=tau_upper> tau; // RW step size
}

transformed parameters {
  vector[NDAYS]   xi;                 // latent trend
  vector[NHOUSES] delta;              // centered house effects
  vector[NPOLLS]  mu;                 // latent mean for each poll

  // enforce sum-to-zero on house effects
  delta = delta_raw - mean(delta_raw);

  // state evolution: gaussian random walk with possible discontinuities
  xi[1] = prevElectionResult;
  for (i in 2:NDAYS)  {
    xi[i] = xi[i - 1] + tau * omega[i - 1];
    if (discontinuity[i] != 0) {
      xi[i] = xi[i] + gamma[discontinuity[i]];
    }
  }

  // observation equation
  for (i in 1:NPOLLS) {
    mu[i] = xi[ day[i] ] + delta[ j[i] ];
  }
}

model {
  // priors
  omega     ~ normal(0, 1);     // RW innovations
  delta_raw ~ normal(0, 0.075); // house effects prior (~1.5pp)
  gamma     ~ normal(0, 0.05);  // break magnitudes (tight; tune as needed)

  // likelihood with known per-poll sigma
  y ~ normal(mu, sigma);
}
