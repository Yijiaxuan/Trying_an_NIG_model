real_data <- read.csv("comb.s.csv")

run_NIG_house_effect <- function(data, party, year, start_date, end_date, stan_file) {
  library(dplyr)
  library(lubridate)
  
  filtered_data <- data %>%
    filter(what == party) %>%
    mutate(mid_date = ymd(mid_date)) %>%
    filter(mid_date >= start_date & mid_date <= end_date) %>%
    filter(!is.na(y)) %>%
    mutate(y = y / 100) %>%
    mutate(j = as.integer(factor(pollster)))  # house index
  
  if (nrow(filtered_data) == 0) {
    stop("No valid polling data for given party and year.")
  }
  
  
  data_list <- list(
    NPOLLS = nrow(filtered_data),
    NHOUSES = length(unique(filtered_data$j)),
    y = filtered_data$y,
    j = filtered_data$j,
    mu0 = 0.5,      
    B0 = 0.5,           
    alpha_0 = 6,
    beta_0 = 25
  )
  
  
  fit <- stan(
    file = stan_file,
    data = data_list,
    iter = 6500,
    warmup = 1500,
    chains = 4,
    seed = 123456
  )
  
  return(fit)
}

house_summary <- function(fit, data) {
  samples <- rstan::extract(fit)
  
  # alpha
  alpha_summary <- tibble(
    mean = mean(samples$alpha),
    sd = sd(samples$alpha),
    q05 = quantile(samples$alpha, 0.05),
    q95 = quantile(samples$alpha, 0.95)
  )
  
  # delta
  delta_summary <- apply(samples$delta, 2, function(x) {
    c(mean = mean(x), sd = sd(x),
      q05 = quantile(x, 0.05), q95 = quantile(x, 0.95))
  }) %>%
    t() %>%
    as.data.frame() %>%
    mutate(pollster = levels(factor(data$pollster))[sort(unique(data$j))])
    #mutate(pollster = sort(unique(data$pollster)))  # restore name
  
  return(list(
    alpha = alpha_summary,
    delta = delta_summary
  ))
}

library(rstan)

fit <- run_NIG_house_effect(
  data = real_data,
  party = "ALP",
  year = 2019,
  start_date = as.Date("2016-07-02"),
  end_date = as.Date("2019-05-18"),
  stan_file = "/Users/16438/Desktop/Reaserch Project/Trying NIG model/Task6_house_effect/Task6.stan"
)

samples <- rstan::extract(fit)

# --- alpha  ---
alpha_mean <- mean(samples$alpha)
alpha_sd <- sd(samples$alpha)
alpha_var <- var(samples$alpha)
alpha_q05 <- quantile(samples$alpha, 0.05)
alpha_q95 <- quantile(samples$alpha, 0.95)

cat("==== ALP Posterior Summary ====\n")
cat(sprintf("Alpha:\n  Mean = %.4f\n  SD = %.4f\n  Variance = %.4f\n  5%% CI = %.4f\n  95%% CI = %.4f\n\n",
            alpha_mean, alpha_sd, alpha_var, alpha_q05, alpha_q95))

# --- delta (house effects) ---
pollsters <- levels(factor(filtered_data$pollster))[sort(unique(filtered_data$j))]
delta_summary <- data.frame(
  pollster = pollsters,
  mean = apply(samples$delta, 2, mean),
  sd = apply(samples$delta, 2, sd),
  q05 = apply(samples$delta, 2, function(x) quantile(x, 0.05)),
  q95 = apply(samples$delta, 2, function(x) quantile(x, 0.95))
)

delta_summary_rounded <- delta_summary %>%
  mutate(across(c(mean, sd, q05, q95), ~ round(.x, 4)))

cat("==== House Effects (delta) Summary ====\n")
print(delta_summary_rounded)
