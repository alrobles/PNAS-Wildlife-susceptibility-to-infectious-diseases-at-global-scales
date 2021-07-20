
library(poweRlaw)
library(cowplot)
library(grid)
library(ggplotify)
library(tidyverse)

#install ecointeraction package from github
install.packages("remotes")
remotes::install_github("alrobles/natcommSubmit_26853")

#get power law
malaria_pl = get_powerlaw(ecointeraction::birdsplasmodiumrelictum$incidence, threads = 7)
wnv_pl = get_powerlaw(ecointeraction::birdswnv$incidence, threads = 7)
coronavirus_pl = get_powerlaw(ecointeraction::batscoronavirus$incidence, threads = 7)

malaria_plot.data <- plot(malaria_pl$pl, draw = F)
malaria_fit.data <- lines(malaria_pl$pl, draw = F)
wnv_plot.data <- plot(wnv_pl$pl, draw = F)
wnv_fit.data <- lines(wnv_pl$pl, draw = F)
coronavirus_plot.data <- plot(coronavirus_pl$pl, draw = F)
coronavirus_fit.data <- lines(coronavirus_pl$pl, draw = F)

mytheme <- theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
                 legend.title=element_text(size = 30, face = "bold"),
                 legend.text=element_text(size = 30, face = "bold"),
                 axis.text.x = element_text(size = 30, face = "bold"),
                 axis.title.x = element_text(size = 30, face = "bold"),
                 axis.text.y = element_text(size = 30, face = "bold"),
                 axis.title.y = element_text(size = 30, face = "bold"),
                 plot.margin = unit(c(0.3,0.3, 0.3, 0.3), "cm"))


plot_malaria <- ggplot(malaria_plot.data) +
  geom_point(aes(x=x, y=y), size = 5 )  +
  geom_line(data = malaria_fit.data, aes(x=(x), y=(y)), colour="red")  +
  annotate("text", label  = paste("p = ", malaria_pl$bs_pl$p), x = 5, y = 0.01, size = 8 ) +
  ggtitle(expression(paste(bolditalic("Plasmodium relictum"), bold("-birds") )) ) +
  mytheme +
  labs(x="incidence", y="CDF") +
  scale_y_log10() + scale_x_log10() + theme_bw()


plot_wnv <- ggplot(wnv_plot.data) +
  geom_point(aes(x=x, y=y), size = 5 )  +
  labs(x="incidence", y="CDF") + scale_y_log10() + scale_x_log10() + theme_bw() +
  geom_line(data = wnv_fit.data, aes(x=(x), y=(y)), colour="red")  +
  annotate("text", label  = paste("p = ", wnv_pl$bs_pl$p), x = 5, y = 0.005, size = 8 ) +
  ggtitle("West Nile virus-birds")  +
  mytheme

plot_coronavirus <- ggplot(coronavirus_plot.data) +
  geom_point(aes(x=x, y=y), size = 5 )  +
  labs(x="incidence", y="CDF") + scale_y_log10() + scale_x_log10() + theme_bw() +
  geom_line(data = coronavirus_fit.data, aes(x=(x), y=(y)), colour="red")  +
  annotate("text", label  = paste("p = ", coronavirus_pl$bs_pl$p), x = 5, y = 0.01, size = 8 ) +
  ggtitle("Coronavirus-bats")  +
  mytheme



png("/home/arobles/ecointeraction/data-raw/01_fig_powerlaw.png", width = 480*3.5, height = 480)
plot_grid(plot_malaria
          ,plot_wnv
          ,plot_coronavirus
          ,nrow = 1
          #,scale = c( 1, 0.05, 1, 0.05, 1)
)
dev.off()
