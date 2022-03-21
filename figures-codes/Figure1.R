# Figure 1 

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

pal_illu <- c("#399283", "#e1d936", "#e09c6c", "#dc3c07")
pal_pb <- wes_palette("Darjeeling1", 3, type = "continuous")


# Illustration 

# Parameters
mu <- 0
n <- 200
nsimu <- 2000

set.seed(270921)
X <- data.frame(X1=rnorm(n, mean = mu))
X$Cluster <- as.factor(hcl2(X$X1))

ppb_illu_0cl <- ggplot(X) + aes(x=X1) + 
  geom_histogram(aes(y=..density..), colour = "white", bins = 25, alpha = 0.9) +
  geom_density(fill = "grey", colour = "black", alpha = 0.2) +
  ylab("Density") +
  theme(axis.title = element_text(size = 14))
ppb_illu_0cl 

ppb_illu_2cl <- ggplot(X) + aes(x=X1, fill = Cluster, colour = Cluster) + 
  geom_histogram(aes(y=..density..), colour = "white", bins = 25, alpha = 0.9) +
  scale_colour_manual(values = pal_illu) +  
  scale_fill_manual(values = pal_illu) +  
  ggnewscale::new_scale_colour() +
  geom_density(data = X, aes(x=X1, fill = Cluster, colour = Cluster), alpha = 0.2) +
  scale_colour_manual(values = c("#1c4941","#706c1b")) +
  ylab("Density") +
  theme(legend.position = c(0.85, 0.6), 
        axis.title = element_text(size = 14))

# Results over 2 000 simulations of the data 
pval_pb <- read.csv(file = "simulations-results/pval_pb.csv")
pval_pb <- as.matrix(pval_pb)

pval_pb.df <- data.frame(pvalues = c(as.numeric(pval_pb),
                                     runif(nsimu)), 
                         Test = c(rep("t-Test", nsimu), 
                                  rep("Gao et al.", nsimu),
                                  rep("Uniform", nsimu)))
ppb_ECDF <- ggplot(pval_pb.df) + aes(x=-log(pvalues), colour = Test, size = Test) + 
  stat_ecdf() + 
  scale_colour_manual(values = pal_pb) +
  scale_size_manual(values = c(.8,.8, .5)) +
  xlab("-log(p-values)") +
  ylab("ECDF") +
  theme(legend.title=element_blank(),
        legend.position = "bottom",
        axis.title = element_text(size = 14)) 

ppb_box<- ggplot(pval_pb.df) +  aes(x=Test, y=pvalues, colour = Test) +
  geom_boxplot() +
  scale_colour_manual(values = pal_pb) +
  theme(legend.title=element_blank(),
        legend.position = "bottom", 
        axis.title = element_text(size = 14)) +
  xlab("Test") +
  ylab("p-values")
ppb_res <- plot_grid(ppb_ECDF + theme(legend.position="none", plot.margin = margin(7, 7, 7, 17)), 
                     ppb_box + theme(legend.position="none", plot.margin = margin(7, 7, 7, 17)),
                     ncol = 2,
                     hjust = -1)
# add legend
legend <- get_legend(
  # create some space to the left of the legend
  ppb_ECDF + theme(legend.box.margin = margin(0, 0, 0, 12))
)

ppb_res_grid<- plot_grid(ppb_res, legend, nrow = 2, rel_heights = c(5, .9))


# Make figure 
ppb_illu <- plot_grid(ppb_illu_0cl, ppb_illu_2cl, nrow = 1, labels = c("A","B"))
ppb_illu_res<- plot_grid(ppb_illu, ppb_res_grid, ncol = 1, labels = c("", "C"))
ppb_illu_res
ggsave(ppb_illu_res, filename = "figures/figure1.pdf", dpi = 600, width = 150, height = 100, units = "mm")
