library(tidyverse)
library(here)
library(tibble)
library(dplyr)
library(readr)

#Clean & combined data
makeCombined <- function(){
  polls_16_19 <- read.csv("/Users/16438/Desktop/Reaserch Project/Paper data and code/2019/polls_2016_2019_good_v2.csv")
  
  polls_13_16 <- read.csv("/Users/16438/Desktop/Reaserch Project/Paper data and code/2016/polls_2013_2016_good_v2.csv")
  
  polls_04_13 <- read.csv("/Users/16438/Desktop/Reaserch Project/Paper data and code/2013/polls_2004_2013_good_v2.csv")
  
  require(dplyr)
  
  polls_16_19$sample[polls_16_19$pollster=="Essential"] <- polls_16_19$sample[polls_16_19$pollster=="Essential"]/2
  
  polls_16_19$responses <- 9 - rowSums(is.na(polls_16_19[c( "Coalition.total.primary",
                                                            "Labor.primary","Green.primary",
                                                            "OneNation.primary","Other.primary",
                                                            "UAP.PUP.primary","KattersAustraliaParty.primary",
                                                            "XenophonCentreAlliance.primary","CON")]))
  
  p.16_19 <- polls_16_19 %>% select(fielding.start,fielding.end,pollster,sample,
                                    Coalition.total.primary,Labor.primary,Green.primary,OneNation.primary,Other.primary,UAP.PUP.primary,KattersAustraliaParty.primary,XenophonCentreAlliance.primary,
                                    DKrefused.combined,responses,
                                    Coalition.TPP.reported)
  
  
  polls_13_16$responses <- 7 - rowSums(is.na(polls_13_16[c( "Coalition.total.primary",
                                                            "Labor.primary","Green.primary",
                                                            "Other.primary",
                                                            "UAP.PUP.primary","KattersAustraliaParty.primary",
                                                            "XenophonCentreAlliance.primary")]))
  
  p.13_16 <- polls_13_16 %>% select(fielding.start,fielding.end,pollster,sample,
                                    Coalition.total.primary,Labor.primary,Green.primary,Other.primary,UAP.PUP.primary,KattersAustraliaParty.primary,XenophonCentreAlliance.primary,
                                    DKrefused.combined,responses,
                                    Coalition.TPP.reported)
  
  
  polls_04_13$responses <- 12 - rowSums(is.na(polls_04_13[c( "Coalition.total.primary","Liberal.primary","National.primary",
                                                             "FamilyFirst.primary","Democrats.primary","OneNation.primary",
                                                             "Labor.primary","Green.primary",
                                                             "Other.primary",
                                                             "UAP.PUP.primary","KattersAustraliaParty.primary", 
                                                             "Independent.primary",      
                                                             "XenophonCentreAlliance.primary")]))
  
  
  p.04_13 <- polls_04_13 %>% select(fielding.start,fielding.end,pollster,sample,
                                    Coalition.total.primary,Labor.primary,Green.primary,
                                    Other.primary,
                                    UAP.PUP.primary,KattersAustraliaParty.primary,XenophonCentreAlliance.primary,FamilyFirst.primary,Democrats.primary,OneNation.primary,Independent.primary,
                                    DKrefused.combined,responses,
                                    Coalition.TPP.reported)
  
  comb <- bind_rows(p.16_19,p.13_16,p.04_13)
  
  comb$LNP <- comb$Coalition.total.primary
  comb$ALP <- comb$Labor.primary
  comb$GRN <- comb$Green.primary
  comb$OTH <- 100 - comb$Labor.primary- comb$Green.primary - comb$Coalition.total.primary
  comb$LNP2PP <- comb$Coalition.TPP.reported
  
  comb$fielding.start <- as.Date(comb$fielding.start,format="%d/%m/%y")
  comb$fielding.end <- as.Date(comb$fielding.end,format="%d/%m/%y")
  
  comb <- comb %>%
    mutate(mid_date = as.Date(floor((as.numeric(fielding.start)+as.numeric(fielding.end))/2),
                              origin="1970/01/01")) 
  
  
  comb$pollster <- recode(comb$pollster,
                          "Galaxy MM"="Newspoll (online & robo)",
                          "Essential (one-week)"="Essential")
  
  
  comb.s <- comb %>% 
    select(fielding.start,fielding.end,mid_date,pollster,sample,LNP,ALP,GRN,OTH,LNP2PP) %>% 
    gather(`LNP`,`ALP`,`GRN`,`OTH`,`LNP2PP`,key="what",value="y")
  comb.s$pollster <- as.factor(comb.s$pollster)
  
  return(as_tibble(comb.s))
  
}

comb.s <- makeCombined()
write.csv(comb.s, "C:/Users/16438/Desktop/Reaserch Project/Trying NIG model/Task3_NIG_real_data_in_R/comb.s.csv", row.names = FALSE)
real_data <- read.csv("C:/Users/16438/Desktop/Reaserch Project/Trying NIG model/Task3_NIG_real_data_in_R/comb.s.csv")

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
fit <- run_NIG_real (real_data, "ALP", 2019, "/Users/16438/Desktop/Reaserch Project/Trying NIG model/Task3_NIG_real_data_in_R/Task3.stan")
print(fit)

fit2 <- run_NIG_real (real_data, "LNP", 2016, "/Users/16438/Desktop/Reaserch Project/Trying NIG model/Task3_NIG_real_data_in_R/Task3.stan")
print(fit2)

