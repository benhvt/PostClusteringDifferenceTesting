rm(list = ls())
library(ggplot2)
library(RColorBrewer)
library(wesanderson)
library(paletteer)
library(patchwork)
library(cowplot)
library(dplyr)
library(kableExtra)
library(latex2exp)
library(ggpattern)
theme_set(theme_bw())

#-- Simulations parameters
nsimu <- 1000
alpha <- 0.05


################################################################################
#--------------- Impact of the sample size on Type I and Power ----------------#
################################################################################

pal <- wesanderson::wes_palette("Darjeeling1", 3, type = "continuous")
pal2 <- c(wesanderson::wes_palette("GrandBudapest2", 2, type = "continuous"), "#cad3fa", pal[2])

pal_illu <- c("#399283", "#e1d936", "#e09c6c", "#dc3c07")


#1./ Impact on the type I error rate 

nocluster_csv <- list.files("supplementary/simulations-results/results_figureS5/", pattern = c("d=0",".csv"), full.names = TRUE)
nocluster <- lapply(nocluster_csv, readr::read_csv)

nocluster_df <- dplyr::bind_rows(nocluster)
nocluster_df <- nocluster_df[,-1]
nocluster_df$Test <- rep(c("Merging test", "Multimodality test", "Selective test (K=2)", "Selective test (K=4)", "t-test"), each = nsimu)

FP_df <-nocluster_df %>% group_by(Test) %>% summarise(across(colnames(nocluster_df)[1:9], function(x){mean(x<0.05, na.rm = T)}))
FP_df2 <- data.frame("False Positive rate"=c(FP_df$`n=10`, FP_df$`n=15`, FP_df$`n=20`, FP_df$`n=25`, FP_df$`n=30`, FP_df$`n=50`, FP_df$`n=75`, FP_df$`n=100`, FP_df$`n=500`),
                     Sample_size = rep(c("n=10", "n=15", "n=20","n=25", "n=30", "n=50", "n=75", 'n=100', "n=500"), each = 5),
                     Test = rep(FP_df$Test, 9), check.names = F)
plt2.FP <- FP_df2 %>% ggplot() +
  aes(x=factor(Sample_size, levels =c("n=10", "n=15", "n=20","n=25", "n=30", "n=50", "n=75", 'n=100', "n=500")), y= `False Positive rate`, colour = Test, 
      group = Test) +
  geom_point(size = 3)+
  geom_line(linewidth = .8) +
  scale_colour_manual(name = "Test",
                      values = c(pal2[3], pal2[1], pal2[2], "#2e3b55", pal2[4])) +
  ylim(c(0,1)) +
  ggnewscale::new_scale_colour() +
  geom_hline(aes(yintercept = 0.05, colour = "5% Nominal Level"), linetype = "dotted", linewidth = 1.3) +
  scale_colour_manual(name = "", values = NatParksPalettes::natparks.pals(name = "Volcanoes", n=6)[5]) +
  xlab("Sample Size") +
  ylab("False Positive Rate at the 5% level") +
  theme(axis.text.x = element_text(angle = 45, hjust =1))

#2./ Impact on the statistical power
cluster_csv <- list.files("supplementary/simulations-results/results_figureS5/", pattern = c("d=5",".csv"), full.names = TRUE)
cluster <- lapply(cluster_csv, readr::read_csv)

cluster_df <- dplyr::bind_rows(cluster)
cluster_df <- cluster_df[,-1]
cluster_df$Test <- rep(c("Merging test", "Multimodality test", "Selective test (K=2)", "Selective test (K=4)", "t-test"), each = nsimu)


power_df <-cluster_df %>% group_by(Test) %>% summarise(across(colnames(cluster_df)[1:9], function(x){mean(x<0.05, na.rm = T)}))
power_df2 <- data.frame("Power"=c(power_df$`n=10`, power_df$`n=15`, power_df$`n=20`, power_df$`n=25`, power_df$`n=30`, power_df$`n=50`, power_df$`n=75`, power_df$`n=100`, power_df$`n=500`),
                        Sample_size = rep(c("n=10", "n=15", "n=20","n=25", "n=30", "n=50", "n=75", 'n=100', "n=500"), each = 5),
                        Test = rep(power_df$Test, 9), check.names = F)
plt2.pow <- power_df2 %>% ggplot() +
  aes(x=factor(Sample_size, levels =c("n=10", "n=15", "n=20","n=25", "n=30", "n=50", "n=75", 'n=100', "n=500")), y= `Power`, colour = Test, 
      group = Test) +
  geom_point(size = 3)+
  geom_line(linewidth = .8) +
  scale_colour_manual(name = "Test",
                      values = c(pal2[3], pal2[1], pal2[2], "#2e3b55", pal2[4])) +
  ylim(c(0,1)) +
  ggnewscale::new_scale_colour() +
  geom_hline(aes(yintercept = 0.05, colour = "5% Nominal Level"), linetype = "dotted", linewidth = 1.3) +
  scale_colour_manual(name = "", values = NatParksPalettes::natparks.pals(name = "Volcanoes", n=6)[5]) +
  xlab("Sample Size") +
  ylab("Statistical power at the 5% level") +
  theme(axis.text.x = element_text(angle = 45, hjust =1))

plt.res <- plt2.FP + plt2.pow + plot_layout(guides = "collect") &
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = 'bold'),
        legend.position = "bottom",
        text = element_text(size = 14))


ggsave(plt.res, filename = "supplementary/figures/FigureS5.pdf",
       width = 300,
       height = 125,
       units = "mm",
       dpi = 600)



