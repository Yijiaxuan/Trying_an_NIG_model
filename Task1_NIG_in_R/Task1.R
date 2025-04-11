library(tidyverse) 
library(ggExtra)
#library(mvtnorm)
set.seed(123456)

# Simulate NIG data
mu0 <- 3 # any value
B0 <- 0.5 # any positive value
alpha0 <- 6 # choose a value greater than 4 (Why? What happens if you don't)
delta0 <- 25 # any positive value
R <- 6500
N <- 10000

g <- rgamma(1, shape=alpha0/2, rate=delta0/2) # easy to sample
sigma2 <- 1/g # an IG(shape=alpha0/2, scale=delta0/2) draw!
mu <- rnorm(1, mean = mu0, sd = sqrt(sigma2 * B0))

y <- rnorm(N, mean = mu, sd = sqrt(sigma2))

#posterior
ybar <- mean(y)
y2<-sum(y^2)
B1 <- (N + (B0)^(-1))^(-1)
mu1 <- B1*((N*ybar)+(mu0*(B0)^(-1)))
alpha1 <- alpha0 + N
delta1 <- delta0 + y2 + (B0^(-1))*(mu0^2) - (B1^(-1))*(mu1^2)


g_post <- rgamma(R, shape=alpha1/2, rate=delta1/2) 
sigma2_post <- 1/g_post

mu_post <- rnorm(R, mean = mu1, sd = sqrt(sigma2_post*B1))  

posterior <- tibble(mu = mu_post, sigma2 = sigma2_post)
summary(posterior)


cat("True mu:", mu, "\n")
cat("Posterior mean of mu:", mean(mu_post), "\n\n")

cat("True sigma^2:", sigma2, "\n")
cat("Posterior mean of sigma^2:", mean(sigma2_post), "\n")


# p <- ggplot(posterior, aes(x = mu, y = sigma2)) +
#  geom_point(alpha = 0.3, color = "steelblue") +
#  labs(title = "Posterior Samples from NIG Model", x = "mu", y = "sigma²") +
#  theme_minimal()

#ggMarginal(p, type = "histogram")