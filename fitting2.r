library(dplyr)
library(ggplot2)
library(reshape2)
library(readxl)
library(rriskDistributions)
library(plotly)

source("helpers.r")
source("/Users/taivo/Google Drive/blog/ggplot-template/template.R")

pa629 <- read_excel("data/pa629.xlsx") %>%
  mutate(alampiir=sapply(Palgavahemik, function(x) strsplit(x, "-")[[1]][1]),
         ülempiir=sapply(Palgavahemik, function(x) strsplit(x, "-")[[1]][2])) %>%
  mutate(alampiir=as.numeric(alampiir), ülempiir=as.numeric(ülempiir)) %>%
  filter(!is.na(ülempiir)) %>%
  mutate(palk=ülempiir, Mehed=cumsum(Mehed)/100, Naised=cumsum(Naised)/100,
         Kokku=cumsum(Kokku)/100) %>%
  select(-Palgavahemik, -alampiir, -ülempiir) %>%
  melt(id.vars=c("palk")) %>%
  rename(prob=value)

pa621 <- read.csv("data/pa621.csv") %>%
  melt(id.vars=c("prob")) %>%
  rename(palk=value)

data <- pa621 %>%
  rbind(pa629)

# ------------------
# ---- Plotting ----
# ------------------

men_women_colors <- c("#09B251", "#9700FF")

# Cumulative prob
data1 <- data %>%
  rename(Grupp=variable, Palk=palk, Osakaal=prob)
ggplot(data1 %>% filter(Grupp != "Kokku"), aes(x=Palk, y=Osakaal, color=Grupp)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values=men_women_colors) +
  xlim(0, NA) +
  xlab("Brutopalk [€]") +
  theme_bw() +
  theme(legend.position="top")
makeFootnote(footnoteText=paste0("Taivo Pungas | pungas.ee"))
ggsave("graphs/palgajaotus-mn.png", width=7, height=5)
ggplotly(width=700, height=500)
plotly_POST(filename="post-60-palgajaotus-mn", fileopt="overwrite")

ggplot(data1 %>% filter(Grupp=="Kokku"), aes(x=Palk, y=Osakaal)) +
  geom_point(color="#2cabb7") +
  geom_line(color="#2cabb7") +
  scale_color_manual(values=men_women_colors) +
  xlim(0, NA) +
  xlab("Brutopalk [€]") +
  theme_bw() +
  theme(legend.position="none")
makeFootnote(footnoteText=paste0("Taivo Pungas | pungas.ee"))
ggsave("graphs/palgajaotus.png", width=7, height=5)
ggplotly(width=700, height=500)
plotly_POST(filename="post-60-palgajaotus", fileopt="overwrite")

# ---- Fitting lognormal distribution ----
salary <- seq(0, 3000, 10)
men <- data %>% filter(variable=="Mehed") %>% arrange(prob)
palk_ennustus <- predict_quantiles_lnorm(men$palk, men$prob, men$prob)
men <- men %>%
  mutate(EnnustatudPalk=round(palk_ennustus)) %>%
  rename(Palk=palk)

probs <- get_salary_dist_lnorm(men$Palk, men$prob, salary)
kumulatiivne <- 100 * get_salary_dist_cum_lnorm(men$Palk, men$prob, salary)
men_dist <- data.frame(salary, probs) %>%
  mutate(type="Mehed",
         Kumulatiivne=kumulatiivne)

women <- data %>% filter(variable=="Naised") %>% arrange(prob)
palk_ennustus <- predict_quantiles_lnorm(women$palk, women$prob, women$prob)
women <- women %>%
  mutate(EnnustatudPalk=round(palk_ennustus)) %>%
  rename(Palk=palk)

probs2 <- get_salary_dist_lnorm(women$Palk, women$prob, salary)
kumulatiivne2 <- 100 * get_salary_dist_cum_lnorm(women$Palk, women$prob, salary)
women_dist <- data.frame(salary, probs=probs2) %>%
  mutate(type="Naised",
         Kumulatiivne=kumulatiivne2)

# ---- Checking fit: salary vs predicted salary ----
ggplot(men %>% rename(Grupp=variable), aes(x=Palk, y=EnnustatudPalk, text=paste0("Kumulatiivne osakaal: ", round(prob*100, digits=1), "%"), group=Grupp)) +
  geom_point() +
  geom_line() +
  geom_abline(intercept=0, slope=1, color="red") +
  xlim(0, NA) + ylim(0, NA) +
  xlab("Brutopalk [€]") + ylab("Mudeli ennustatud brutopalk [€]") +
  ggtitle("Tegelik vs ennustatud palk, mehed") +
  theme_bw()
makeFootnote(footnoteText=paste0("Taivo Pungas | pungas.ee"))
ggsave("graphs/checkingmodel-men.png", width=7, height=5)
ggplotly(width=700, height=300)
plotly_POST(filename="post-60-checkingmodel-men", fileopt="overwrite")

ggplot(women %>% rename(Grupp=variable), aes(x=Palk, y=EnnustatudPalk, text=paste0("Kumulatiivne osakaal: ", round(prob*100, digits=1), "%"), group=1)) +
  geom_point() +
  geom_line() +
  geom_abline(intercept=0, slope=1, color="red") +
  xlim(0, NA) + ylim(0, NA) +
  xlab("Brutopalk [€]") + ylab("Mudeli ennustatud brutopalk [€]") +
  ggtitle("Tegelik vs ennustatud palk, naised") +
  theme_bw()
makeFootnote(footnoteText=paste0("Taivo Pungas | pungas.ee"))
ggsave("graphs/checkingmodel-women.png", width=7, height=5)
ggplotly(width=700, height=300)
plotly_POST(filename="post-60-checkingmodel-women", fileopt="overwrite")

# ---- Checking fit: percentiles ----
men_colors <- c("#09B251", "#888888")
men_melted <- men %>%
  select(-variable) %>%
  melt(id.vars=c("prob")) %>%
  rename(Allikas=variable, Palk=value, KumulatiivneOsakaal=prob)
ggplot(men_melted, aes(x=Palk, y=KumulatiivneOsakaal, color=Allikas)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values=men_colors) +
  xlim(0, NA) + ylim(0, NA) +
  xlab("Brutopalk [€]") + ylab("Osakaal") +
  ggtitle("Tegelik ja mudeli ennustatud palk, mehed") +
  theme_bw()
makeFootnote(footnoteText=paste0("Taivo Pungas | pungas.ee"))
ggsave("graphs/checkingmodel-men-percentiles.png", width=7, height=5)
ggplotly(width=700, height=500)
plotly_POST(filename="post-60-checkingmodel-men-percentiles", fileopt="overwrite")

women_colors <- c("#9700FF", "#888888")
women_melted <- women %>%
  select(-variable) %>%
  melt(id.vars=c("prob")) %>%
  rename(Allikas=variable, Palk=value, KumulatiivneOsakaal=prob)
ggplot(women_melted, aes(x=Palk, y=KumulatiivneOsakaal, color=Allikas)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values=women_colors) +
  xlim(0, NA) + ylim(0, NA) +
  xlab("Brutopalk [€]") + ylab("Osakaal") +
  ggtitle("Tegelik ja mudeli ennustatud palk, naised") +
  theme_bw()
makeFootnote(footnoteText=paste0("Taivo Pungas | pungas.ee"))
ggsave("graphs/checkingmodel-women-percentiles.png", width=7, height=5)
ggplotly(width=700, height=500)
plotly_POST(filename="post-60-checkingmodel-women-percentiles", fileopt="overwrite")

# ---- Plotting distribution predicted by model ----
both <- rbind(women_dist, men_dist) %>%
  rename(Palk=salary, Tihedus=probs, Grupp=type) %>%
  mutate(Kumulatiivne=round(Kumulatiivne, digits=1))
ggplot(both, aes(x=Palk, y=Tihedus, text=paste0("Kumulatiivne osakaal: ", Kumulatiivne, "%"), group=Grupp)) +
  geom_line(aes(color=Grupp)) +
  geom_area(aes(fill=Grupp), alpha=0.2, position="identity") +
  scale_color_manual(values=men_women_colors) +
  scale_fill_manual(values=men_women_colors) +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
makeFootnote(footnoteText=paste0("Taivo Pungas | pungas.ee"))
ggsave("graphs/palgajaotus-modelboth.png", width=7, height=5)
ggplotly(width=700, height=500)
plotly_POST(filename="post-60-palgajaotus-modelboth", fileopt="overwrite")

ggplot(both, aes(x=Palk, y=Kumulatiivne)) +
  geom_line(aes(color=Grupp)) +
  geom_area(aes(fill=Grupp), alpha=0.2, position="identity") +
  scale_color_manual(values=men_women_colors) +
  scale_fill_manual(values=men_women_colors) +
  ylab("Kumulatiivne osakaal, %") +
  theme_bw()
makeFootnote(footnoteText=paste0("Taivo Pungas | pungas.ee"))
ggsave("graphs/palgajaotus-modelboth-cum.png", width=7, height=5)
ggplotly(width=700, height=500)
plotly_POST(filename="post-60-palgajaotus-modelboth-cum", fileopt="overwrite")

# ---- Checking statistics ----
true_mean_men <- 1192
true_mean_women <- 896

meanlog_men <- get_params_lnorm(men$Palk, men$prob)$meanlog
sdlog_men <- get_params_lnorm(men$Palk, men$prob)$sdlog
meanlog_women <- get_params_lnorm(women$Palk, women$prob)$meanlog
sdlog_women <- get_params_lnorm(women$Palk, women$prob)$sdlog

predicted_mean_men <- round(exp(meanlog_men + sdlog_men * sdlog_men / 2))
predicted_mean_women <- round(exp(meanlog_women + sdlog_women * sdlog_women / 2))

print(paste0("Naised -- ennustatud keskmine ", predicted_mean_women,
             ", tegelik ", true_mean_women, " (",
             round(100*(true_mean_women - predicted_mean_women)/true_mean_women,digits=1),
             "% mööda)."))

print(paste0("Mehed -- ennustatud keskmine ", predicted_mean_men,
             ", tegelik ", true_mean_men, " (",
             round(100*(true_mean_men - predicted_mean_men)/true_mean_men,digits=1),
             "% mööda)."))

