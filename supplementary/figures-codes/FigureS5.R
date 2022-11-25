# Figure S5

library(ggplot2)
library(RColorBrewer)
library(wesanderson)
library(viridis)
library(paletteer)
library(patchwork)
library(cowplot)
library(dplyr)
library(ggcorrplot)
library(latex2exp)
library(multimode)
source(file = "utils.R")

theme_set(theme_bw())
pal <- wes_palette("Darjeeling1", 3, type = "continuous")
pal2 <- c(wes_palette("GrandBudapest2", 2, type = "continuous"), "#cad3fa", pal[2])

p <- c(2:10, 25, 50)
nsimu <- 500

# Results over 500 simulations of the data 
time_dip_2cl <- read.csv(file = "supplementary/simulations-results/results_figureS5/time_dip_2cl.csv")
time_dip_4cl <- read.csv(file = "supplementary/simulations-results/results_figureS5/time_dip_4cl.csv")

time_SI_2cl <- read.csv(file = "supplementary/simulations-results/results_figureS5/time_SI_2cl.csv")
time_SI_4cl <- read.csv(file = "supplementary/simulations-results/results_figureS5/time_SI_4cl.csv")

time_merge_2cl <- read.csv(file = "supplementary/simulations-results/results_figureS5/time_merge_2cl.csv")
time_merge_4cl <- read.csv(file = "supplementary/simulations-results/results_figureS5/time_merge_4cl.csv")
time_merge_4cl$V11 <- 60*time_merge_4cl$V11 #Min -> sec


time_dip_2cl$Method <- time_dip_4cl$Method <- "Multimodality test"
time_SI_2cl$Method <- time_SI_4cl$Method <- "Selective inference (Direct)"
time_merge_2cl$Method <- time_merge_4cl$Method <- "Selective inference (Harmonic mean)"

time_dip_2cl$NbCluster <- time_SI_2cl$NbCluster <- time_merge_2cl$NbCluster <- "2 clusters"
time_dip_4cl$NbCluster <- time_SI_4cl$NbCluster <- time_merge_4cl$NbCluster <- "4 clusters"

p.df <- c()

for (i in 1:length(p)){
  p.df <- c(p.df, rep(p[i], 6*nsimu))
}
time_res <- rbind(time_dip_2cl,
                  time_SI_2cl,
                  time_merge_2cl,
                  time_dip_4cl,
                  time_SI_4cl,
                  time_merge_4cl)

time.df <- data.frame(Time = as.numeric(as.matrix(time_res[,1:11])),
                      Method = rep(time_res$Method, length(p)),
                      NbCluster = rep(time_res$NbCluster, length(p)), 
                      p = as.factor(rep(p.df, 6)))


p_time <- ggplot(time.df) + aes(x=p, y=Time, colour = Method) + geom_boxplot() + facet_wrap(~NbCluster) +
  scale_colour_manual(values = pal2, 
                      labels = c("Multimodality test",
                                 "Selective test (direct)", 
                                 "Merging selective test"))  +
  scale_y_continuous(trans = "log10") +
  xlab("Number of dimensions (p)") +
  ylab("Time for one test (sec)
       log10 scale") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14))

p_mean_time <- time.df %>% group_by(Method, NbCluster, p) %>% 
  summarise(Mean_Time = mean(Time)) %>%
  ggplot() + 
  aes(x=as.numeric(p), y=Mean_Time, colour = Method, linetype = Method) + 
  geom_point(size = 2.5) + 
  geom_line(size = 1.2) +
  scale_y_continuous(trans = "log10") + 
  annotation_logticks(sides = "l") +
  scale_colour_manual(values = pal2, 
                      labels = c("Multimodality test",
                                 "Selective test (direct)", 
                                 "Merging selective test"))  +
  scale_linetype_manual(values = c("dotted", "solid", "dashed"),
                        labels = c("Multimodality test",
                                   "Selective test (direct)", 
                                   "Merging selective test")) +
  facet_wrap(~NbCluster) +
  xlab("Number of dimensions (p)") +
  ylab("Mean time to perform one test (sec)
       log10 scale") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=14),
        legend.box="vertical") 

p_mean_time

p_res_time <- p_time / p_mean_time +
  plot_layout(heights = c(1, 1.5),
              widths = c(2,1)) +
  plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold", size = 14))

p_res_time

ggsave(p_res_time, filename = "supplementary/figures/FigureS5.pdf", dpi = 600, width = 150, height = 225, units = "mm")
ggsave(p_mean_time, filename = "supplementary/figures/FigureS5V2.pdf", dpi = 600, width = 225, height = 125, units = "mm")
  