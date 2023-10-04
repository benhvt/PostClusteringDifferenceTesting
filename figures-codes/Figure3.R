# Figure 2

library(ggplot2)
library(RColorBrewer)
library(wesanderson)
library(paletteer)
library(patchwork)
library(cowplot)
library(dplyr)
library(latex2exp)
theme_set(theme_bw())
source(file = "utils.R")

pal <- wes_palette("Darjeeling1", 3, type = "continuous")
pal2 <- c(wes_palette("GrandBudapest2", 2, type = "continuous"), "#cad3fa", pal[2])

pal_illu <- c("#399283", "#e1d936", "#e09c6c", "#dc3c07")

# Illustration
# Parameter 
n <- 200
mu <- c(0,5)

set.seed(27092021)
X_4cl <- data.frame(X1= c(rnorm(n/2, mean = mu[1]), rnorm(n/2, mean = mu[2])))
X_4cl$Cluster <- hcl4(X_4cl$X1)
X_4cl$NbClust <- "4 clusters"
X_4cl$Cluster <- forcats::fct_recode(X_4cl$Cluster, "1"="1", "2"="4", "3"="2", "4"="3")

X_2cl <- data.frame(X1=X_4cl$X1)
X_2cl$Cluster <- hcl2(X_2cl$X1)
X_2cl$NbClust <- "2 clusters"

X <- rbind(X_4cl, X_2cl)
ppower_illu <- ggplot(X) +  aes(x=X1, fill = Cluster, colour = Cluster) + 
  geom_histogram(aes(y=..density..), colour = "white", bins = 25, alpha = 0.9) +
  geom_vline(xintercept = mu, colour = "orange", linetype = "dashed", size = 0.9) +
  facet_grid(~NbClust, scale = "free", drop = T) +
  scale_fill_manual(name = "Cluster", values = c(pal_illu[c(1,4,2,3)]), breaks = c("1", "2", "3", "4")) +
  #+ ggnewscale::new_scale_colour() +
  geom_density(data = X, aes(x=X1, fill = Cluster, colour = Cluster), alpha = 0.4, size=.8) +
  scale_colour_manual(name = "Cluster", values = c("#22574e", "#9a2a04", "#b4ad2b", "#b37c56"), breaks = c("1", "2", "3", "4")) +
  ylab("Density") +
  xlab(expression(X[1])) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        strip.text = element_text(size=16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))
ppower_illu+ ggtitle("Experiment")

# Results over 2 000 simulations of the data

delta <- seq(0,8, 0.2)
alpha <- 0.05
nsimu <- 2000

pval_dip <- read.csv(file = "simulations-results/results_figure3/power_dip_delta.csv")
pval_SI_2cl <- read.csv(file = "simulations-results/results_figure3/power_SI_2cl_delta.csv")
pval_SI_4cl <- read.csv(file = "simulations-results/results_figure3/power_SI_4cl_delta.csv")
pval_merge <- read.csv(file = "simulations-results/results_figure3/power_merge_delta.csv")


# Estimation of the statistical power using monte-carlo approach over the 2000 simulations

power_dip <- apply(pval_dip,2, function(x){sum(x<alpha)/nsimu})
power_SI_2cl <- apply(pval_SI_2cl,2, function(x){sum(x<alpha, na.rm = T)/sum(is.na(x)==F)})
power_SI_4cl <- apply(pval_SI_4cl,2, function(x){sum(x<alpha, na.rm = T)/sum(is.na(x)==F)})
power_merge <- apply(pval_merge,2, function(x){sum(x<alpha, na.rm = T)/sum(is.na(x)==F)})

powerfig <- data.frame(power = c(rep(as.numeric(power_dip),2),
                                 as.numeric(power_SI_2cl),
                                 as.numeric(power_SI_4cl),
                                 as.numeric(power_SI_2cl),
                                 as.numeric(power_merge)),
                       Method = c(rep("Multimodality test",2*length(delta)),
                                  rep("Selective inference (Direct)", 2*length(delta)),
                                  rep("Selective inference (Harmonic Mean)", 2*length(delta))),
                       NbClust = rep(c(rep("2 clusters", length(delta)),
                                       rep("4 clusters", length(delta))), 3),
                       delta = rep(delta, 6))

ppower_res <- ggplot(powerfig) + 
  geom_hline(aes(yintercept = alpha, colour ="5% level"), linetype = "dashed") +
  scale_colour_manual(name = " ", values = "black")  +
  ggnewscale::new_scale_colour() +
  aes(x=delta, y = power, colour = Method)  + 
  geom_line(size = 0.8) +
  geom_point() + 
  geom_point(data=powerfig,
             aes(x=delta, y = power, colour = Method)) +
  geom_point(data=powerfig[which(powerfig$Method == "Selective inference (Direct)"),],
             aes(x=delta, y=power, colour = Method)) +
  scale_colour_manual(values = c(pal2, "#cad3fa"), 
                      labels = c("Multimodality test",
                                 "Selective test (direct)",
                                 "Merging selective test"))  +
  facet_wrap(~NbClust) +
  xlab(TeX(r'($\delta$)')) +
  ylab("Statistical power") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        strip.text = element_text(size=16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))

ppower_res

# Make figure

ppower <- ppower_illu / ppower_res + 
  plot_layout(heights = c(1, 2), widths = c(2,1)) + 
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold', size = 20)) 

ggsave(ppower, file = "figures/figure3.pdf", dpi = 600, width = 250, height = 262.5, units = "mm")

