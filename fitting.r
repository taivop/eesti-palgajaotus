library(dplyr)
library(ggplot2)
library(reshape2)
library(rriskDistributions)


# Log-normal
get_lnorm_params <- function(deciles) {
  probabilities <- seq(0.1, 0.9, 0.1)
  distr_lnorm <- get.lnorm.par(p=probabilities, q=deciles, plot=FALSE)
  
  res <- list()
  res$meanlog <- distr_lnorm[["meanlog"]]
  res$sdlog <- distr_lnorm[["sdlog"]]
  return(res)
}

get_salary_dist <- function(deciles, salary) {
  fit <- get_lnorm_params(deciles)
  probs <- dlnorm(salary, meanlog=fit$meanlog, sdlog=fit$sdlog)
  return(probs)
}

predict_quantiles <- function(true_deciles, probabilities) {
  fit <- get_lnorm_params(true_deciles)
  return(qlnorm(probabilities, meanlog=fit$meanlog, sdlog=fit$sdlog))
}

# Data from Stat:PA621 http://pub.stat.ee/px-web.2001/Dialog/varval.asp?ma=PA621&ti=T%C4IST%D6%D6AJAGA+T%D6%D6TAJATE+KESKMINE+BRUTOKUUT%D6%D6TASU+JA+T%D6%D6TASU+DETSIILID+SOO+JA+AMETIALA+PEAR%DCHMA++J%C4RGI%2C+OKTOOBER&path=../Database/Majandus/12Palk_ja_toojeukulu/05Tootasu/&lang=2
probabilities <- seq(0.1, 0.9, 0.1)
deciles_overall <- c(413, 540, 649, 770, 890, 1011, 1174, 1387, 1784)
deciles_men <- c(428, 607, 752, 892, 1018, 1170, 1337, 1580, 2000)
deciles_women <- c(400, 500, 596, 680, 785, 888, 1000, 1180, 1484)

salary <- seq(0, 2500, 10)
overall <- get_salary_dist(deciles_overall, salary)
men <- get_salary_dist(deciles_men, salary)
women <- get_salary_dist(deciles_women, salary)

#predicted_deciles <- qlnorm(probabilities, meanlog=fit$meanlog, sdlog=fit$sdlog)
#true_deciles <- data.frame(salary=deciles)
probs <- seq(0.1, 0.9, 0.1)
predicted_deciles <- data.frame(
  overall = predict_quantiles(deciles_overall, probs),
  men = predict_quantiles(deciles_men, probs),
  women = predict_quantiles(deciles_women, probs)
)
true_deciles <- data.frame(
  overall = deciles_overall,
  men = deciles_men,
  women = deciles_women
)


# ---- Plotting ----
data2 <- data.frame(salary, overall, men, women) %>%
  melt(id.vars=c("salary"))
ggplot(data2 %>% filter(variable=="men"), aes(x=salary, y=value)) +
  geom_vline(data=predicted_deciles, aes(xintercept=men)) +
  geom_vline(data=true_deciles, aes(xintercept=men), color="red") +
  geom_area(fill="blue", alpha=0.4) +
  geom_line(color="blue") +
  facet_wrap(~variable, ncol=1) +
  theme_bw()

ggplot(data2 %>% filter(variable=="women"), aes(x=salary, y=value)) +
  geom_vline(data=predicted_deciles, aes(xintercept=women)) +
  geom_vline(data=true_deciles, aes(xintercept=men), color="blue") +
  geom_area(fill="red", alpha=0.4) +
  geom_line(color="red") +
  facet_wrap(~variable, ncol=1) +
  theme_bw()


# Plotting
data1 <- data.frame(salary, probability)
ggplot(data1, aes(x=salary, y=probability)) +
  geom_line(size=2, color="blue") +
  #geom_vline(data=predicted_deciles, aes(xintercept=salary)) +
  #geom_vline(data=true_deciles, aes(xintercept=salary), color="red") +
  theme_bw()