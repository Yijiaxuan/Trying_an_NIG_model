real_data <- read.csv("C:/Users/16438/Desktop/Reaserch Project/Trying NIG model/Task4.2_NIG_real_in_Rstan_u0~U(0.4, 0.6)/comb.s.csv")

# define a function
run_NIG_real <- function(data, party, year, stan_file) {
  filtered_data <- data %>%
    filter(what == party) %>%
    mutate(mid_date = ymd(mid_date)) %>%
    mutate(year = year(mid_date)) %>%
    filter(year == year)
  
  y_clean <- filtered_data$y[!is.na(filtered_data$y)] / 100
  NPOLLS <- length(y_clean)
  
  data_list <- list(
    NPOLLS = NPOLLS,
    y = y_clean,
    mu0 <- 3, # If change to 0.5, mu is similar, sigma2 will decrease.
    B0 = 0.5,
    alpha0 = 6,
    delta0 = 25
  )
  
  fit <- stan(file = stan_file, 
              data = data_list,
              iter = 6500, 
              warmup = 1500, 
              chains = 4, 
              seed = 123456)
  
  return(fit)
}


# run stan
library(rstan)
fit <- run_NIG_real (real_data, "ALP", 2019, "/Users/16438/Desktop/Reaserch Project/Trying NIG model/Task4.0_NIG_real_in_Rstan/Task4.0.stan")
print(fit)

fit2 <- run_NIG_real (real_data, "LNP", 2016, "/Users/16438/Desktop/Reaserch Project/Trying NIG model/Task4.0_NIG_real_in_Rstan/Task4.0.stan")
print(fit2)

