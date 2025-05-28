real_data <- read.csv("comb.s.csv")

run_NIG_time_trend <- function(data, party, year, start_date, end_date, stan_file) {
  library(dplyr)
  library(lubridate)
  
  
  filtered_data <- data %>%
    filter(what == party) %>%
    mutate(mid_date = ymd(mid_date)) %>%
    filter(mid_date >= start_date & mid_date <= end_date) %>%
    filter(!is.na(y)) %>%
    mutate(y = y / 100)
  
  if (nrow(filtered_data) == 0) {
    stop("No valid polling data for given party and year.")
  }
  
  # 2. daily index
  filtered_data <- filtered_data %>%
    mutate(day = as.numeric(difftime(mid_date, start_date, units = "days")) + 1)
  
  NDAYS <- as.numeric(difftime(end_date, start_date, units = "days")) + 1
  NPOLLS <- nrow(filtered_data)
  
  # 3. stan data list
  data_list <- list(
    NPOLLS = NPOLLS,
    y = filtered_data$y,
    day = filtered_data$day,
    NDAYS = NDAYS,
    B0 = 0.5,
    alpha0 = 6,
    beta0 = 25,
    prevElectionResult = 0.3473, 
    tau_upper = 0.3 # Jackan's 0.003 is not work here
  )
  
  # 4. fitted model
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


# summary xi of ALP
xi_summary <- function(fit, start_date) {
  library(rstan)
  library(dplyr)
  library(lubridate)
  
  xi_samples <- rstan::extract(fit)$xi  # [iterations, NDAYS]
  
  xi_df <- apply(xi_samples, 2, function(x) {
    tibble(
      mean = mean(x),
      sd = sd(x),
      q05 = quantile(x, 0.05),
      q95 = quantile(x, 0.95)
    )
  }) %>%
    bind_rows() %>%
    mutate(
      day = 1:n(),
      date = start_date + days(day - 1)
    )
  
  return(xi_df)
}

library(rstan)
fit <- run_NIG_time_trend(
  data = real_data,
  party = "ALP",
  year = 2019,
  start_date = as.Date("2016-07-02"),
  end_date = as.Date("2019-05-18"),
  stan_file = "Task5.stan"
)

# summary(fit)

sum_xi<- xi_summary(fit, start_date = as.Date("2016-07-02"))
head(sum_xi)
tail(sum_xi)
#View(sum_xi)


# plot xi of ALP
plot_xi<- function(fit, start_date) {
  library(rstan)
  library(ggplot2)
  library(dplyr)
  
  xi_samples <- rstan::extract(fit)$xi
  
  xi_mean <- apply(xi_samples, 2, mean)
  
  xi_df <- tibble(
    day = 1:length(xi_mean),
    date = start_date + days(day - 1),
    mean = xi_mean
  )
  
  ggplot(xi_df, aes(x = date, y = mean)) +
    geom_line(color = "black", size = 1) +
    labs(
      title = "Poll aggregation of ALP over Time",
      x = "Date", y = "Estimated xi"
    ) +
    theme_minimal()
}

plot_xi(fit, start_date = as.Date("2016-07-02"))


# plot xi of ALP & polls data
plot_xi_trend <- function(fit, start_date, real_data, party) {
  library(rstan)
  library(ggplot2)
  library(dplyr)
  library(lubridate)
  
  xi_samples <- rstan::extract(fit)$xi
  xi_mean <- apply(xi_samples, 2, mean)
  
  xi_df <- tibble(
    day = 1:length(xi_mean),
    date = start_date + days(day - 1),
    mean = xi_mean
  )
  
  poll_data <- real_data %>%
    filter(what == party) %>%
    mutate(mid_date = ymd(mid_date),
           y = y / 100) %>%
    filter(mid_date >= start_date,
           mid_date <= max(xi_df$date))
  
  ggplot(xi_df, aes(x = date, y = mean)) +
    geom_line(color = "black", size = 1) +
    geom_point(data = poll_data, aes(x = mid_date, y = y),
               inherit.aes = FALSE, color = "red", size = 1.5, alpha = 0.6) +
    labs(
      title = paste("Poll aggregation of", party, "over Time"),
      x = "Date", y = "Estimated xi"
    ) +
    theme_minimal()
}

plot_xi_trend(
  fit = fit,
  start_date = as.Date("2016-07-02"),
  real_data = real_data,
  party = "ALP"
)

