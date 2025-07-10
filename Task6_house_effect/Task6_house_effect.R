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
  
  return(list(fit = fit, filtered_data = filtered_data))
}

house_summary <- function(fit, filtered_data) {
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
    as.data.frame()
  
  colnames(delta_summary) <- c("mean", "sd", "q05", "q95")
  
  
  pollster_levels <- levels(factor(filtered_data$pollster))
  pollster_index <- sort(unique(filtered_data$j))
  delta_summary <- delta_summary %>%
    mutate(pollster = pollster_levels[pollster_index])
  
    #mutate(pollster = levels(factor(data$pollster))[sort(unique(data$j))])
    #mutate(pollster = sort(unique(data$pollster)))  # restore name
  
  return(list(
    alpha = alpha_summary,
    delta = delta_summary
  ))
}

library(rstan)

result <- run_NIG_house_effect(
  data = real_data,
  party = "ALP",
  year = 2019,
  start_date = as.Date("2016-07-02"),
  end_date = as.Date("2019-05-18"),
  stan_file = "/Users/16438/Desktop/Reaserch Project/Trying NIG model/Task6_house_effect/Task6.stan"
)

fit <- result$fit
filtered_data <- result$filtered_data
summary_list <- house_summary(fit, filtered_data)


library(ggplot2)

actual_ALP <- 0.3334
alpha_mean <- summary_list$alpha$mean
delta_summary <- summary_list$delta

# calculate each pollster's estimate for ALP
delta_summary <- delta_summary %>%
  mutate(
    est_support = alpha_mean + mean,
    q05_support = alpha_mean + q05,
    q95_support = alpha_mean + q95
  ) %>%
  arrange(est_support) %>%
  mutate(pollster = factor(pollster, levels = pollster)) 


ggplot(delta_summary, aes(x = pollster, y = est_support, color = pollster)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = q05_support, ymax = q95_support), width = 0.2) +
  geom_hline(yintercept = actual_ALP, linetype = "dashed", color = "black") +
  annotate("text", x = 0.5, y = actual_ALP + 0.01, label = "Actual ALP vote", hjust = 0, size = 3.5) +
  theme_minimal() +
  labs(
    title = "Estimated ALP Support by Pollster (with House Effects)",
    y = "Estimated ALP Support",
    x = "Pollster"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = "none")
