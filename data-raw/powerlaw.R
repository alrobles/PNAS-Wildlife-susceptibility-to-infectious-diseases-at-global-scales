
library(poweRlaw)

library("grid")
library("ggplotify")
library(tidyverse)
malaria_pl = displ$new(ecointeraction::birdsplasmodiumrelictum$incidence)
malaria_est = estimate_xmin(malaria_pl)
malaria_pl$setXmin(malaria_est)
malaria_plot.data <- plot(malaria_pl, draw = F)
malaria_fit.data <- lines(malaria_pl, draw = F)
malaria_bs_pl <- bootstrap_p(malaria_pl, no_of_sims=1000, threads=7, seed = 123)


# ggplot(malaria_plot.data) + geom_point(aes(x=log(x), y=log(y))) + labs(x="log(k)", y="log(CDF)") + theme_bw() +
#   geom_line(data = malaria_fit.data, aes(x=log(x), y=log(y)), colour="red")
plot_malaria <- ggplot(malaria_plot.data) +
  geom_point(aes(x=x, y=y), size = 5 )  +
  labs(x="incidence", y="CDF") + scale_y_log10() + scale_x_log10() + theme_bw() +
  geom_line(data = malaria_fit.data, aes(x=(x), y=(y)), colour="red")  +
  annotate("text", label  = paste("p = ", malaria_bs_pl$p), x = 5, y = 0.01, size = 8 ) +
  ggtitle(expression(paste(bolditalic("Plasmodium relictum"), bold("-birds") )) ) +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        legend.title=element_text(size = 30, face = "bold"),
        legend.text=element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 30, face = "bold"),
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.text.y = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"))+
  theme(plot.margin = unit(c(0.3,0.3, 0.3, 0.3), "cm"))


wnv_pl = displ$new(ecointeraction::birdswnv$incidence)
wnv_est = estimate_xmin(wnv_pl)
wnv_pl$setXmin(wnv_est)
wnv_plot.data <- plot(wnv_pl, draw = F)
wnv_fit.data <- lines(wnv_pl, draw = F)
wnv_bs_pl <- bootstrap_p(wnv_pl, no_of_sims=1000, threads=7, seed = 123)


plot_wnv <- ggplot(wnv_plot.data) +
  geom_point(aes(x=x, y=y), size = 5 )  +
  labs(x="incidence", y="CDF") + scale_y_log10() + scale_x_log10() + theme_bw() +
  geom_line(data = wnv_fit.data, aes(x=(x), y=(y)), colour="red")  +
  annotate("text", label  = paste("p = ", wnv_bs_pl$p), x = 5, y = 0.005, size = 8 ) +
  ggtitle("West Nile virus-birds") +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        legend.title=element_text(size = 30, face = "bold"),
        legend.text=element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 30, face = "bold"),
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.text.y = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"))+
  theme(plot.margin = unit(c(0.3,0.3, 0.3, 0.3), "cm"))



coronavirus_pl = displ$new(ecointeraction::batscoronavirus$incidence)
est = estimate_xmin(coronavirus_pl)
coronavirus_pl$setXmin(est)
coronavirus_plot.data <- plot(coronavirus_pl, draw = F)
coronavirus_fit.data <- lines(coronavirus_pl, draw = F)
coronavirus_bs_pl <- bootstrap_p(coronavirus_pl, no_of_sims=1000, threads=7, seed = 123)


plot_coronavirus <- ggplot(coronavirus_plot.data) +
  geom_point(aes(x=x, y=y), size = 5 )  +
  labs(x="incidence", y="CDF") + scale_y_log10() + scale_x_log10() + theme_bw() +
  geom_line(data = coronavirus_fit.data, aes(x=(x), y=(y)), colour="red")  +
  annotate("text", label  = paste("p = ", coronavirus_bs_pl$p), x = 5, y = 0.01, size = 8 ) +
  ggtitle("Coronavirus-bats") +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        legend.title=element_text(size = 30, face = "bold"),
        legend.text=element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 30, face = "bold"),
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.text.y = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"))+
  theme(plot.margin = unit(c(0.3,0.3, 0.3, 0.3), "cm"))


library(cowplot)
png("/home/arobles/ecointeraction/data-raw/fig_powerlaw.png", width = 480*3.5, height = 480)
plot_grid(plot_malaria
          ,plot_wnv
          ,plot_coronavirus
          ,nrow = 1
          #,scale = c( 1, 0.05, 1, 0.05, 1)
)
dev.off()

png("/home/arobles/ecointeraction/data-raw/fig_powerlaw_malaria.png", width = 480, height = 480)
plot_malaria
dev.off()

png("/home/arobles/ecointeraction/data-raw/fig_powerlaw_wnv.png", width = 480, height = 480)
plot_wnv
dev.off()

png("/home/arobles/ecointeraction/data-raw/fig_powerlaw_coronavirus.png", width = 480, height = 480)
plot_coronavirus
dev.off()
