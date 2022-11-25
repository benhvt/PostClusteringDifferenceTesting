# Figure S6

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


n <- 200
nsimu <- 500
pal <- c("#399283", "#dc3c07")
pal2 <- c(wes_palette("GrandBudapest2", 2, type = "continuous"), "#cad3fa", pal[2])


set.seed(170822)
X <- matrix(NA, nrow = n, ncol=7)
X[,1] <- rnorm(n, sd=2)
X[,2] <- runif(n, -10, 10)
X[,3] <- rchisq(n, df=4)
X[,4] <- rexp(n, rate = 0.5)
X[,5] <- rgamma(n, shape = 4)
X[,6] <- rpois(n, lambda = 4)
X[,7] <- rpois(n, lambda = 50)

X.illu.distri <- data.frame(X1=as.numeric(X),
                            Distribution = as.factor(c(rep("N(0,2)", n),
                                                       rep("U(-10,10)",n),
                                                       rep("chi^{2}~(4)", n),
                                                       rep("E(0.5)",n),
                                                       rep("Gamma(4,1)", n),
                                                       rep("Poi(4)", n),
                                                       rep("Poi(50)",n))))

X.illu.distri$Cluster <- as.factor(apply(X,2,hcl2))

p_illu_distri <- ggplot(X.illu.distri) + 
  aes(x=X1, y=..count.., fill = Cluster, colour = Cluster) + 
  geom_histogram(colour = "white", bins = 25, alpha = 0.9) +
  geom_density(alpha = 0.2) +
  scale_colour_manual(values =pal) +
  scale_fill_manual(values = pal) +
  facet_wrap(~Distribution, scale = "free", labeller= label_parsed) +
  ylab("Density") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        legend.position = "bottom") 

pval_distri_H0_dip <- read.csv(file = "supplementary/simulations-results/results_figureS6/pval_distri_H0_dip.csv")
pval_distri_H0_SI <- read.csv(file="supplementary/simulations-results/results_figureS6/pval_distri_H0_SI.csv")

pval_distri_H0 <- data.frame(pvalues = c(as.numeric(as.matrix(pval_distri_H0_dip[,-1])),
                                         as.numeric(as.matrix(pval_distri_H0_SI[,-1]))),
                             Distribution = as.factor(c(rep(c(rep("N(0,2)", nsimu),
                                                              rep("U(-10,10)",nsimu),
                                                              rep("chi^{2}~(4)", nsimu),
                                                              rep("E(0.5)",nsimu),
                                                              rep("Gamma(4,1)", nsimu),
                                                              rep("Poi(4)", nsimu),
                                                              rep("Poi(50)", nsimu)),2))),
                             Test = c(rep("Multimodality test", nsimu*ncol(pval_distri_H0_dip[,-1])),
                                      rep("Selective Test", nsimu*ncol(pval_distri_H0_SI[,-1]))))

p_distri_H0_box <- ggplot(pval_distri_H0) + 
  geom_hline(aes(yintercept = 0.05, colour = "5% level"), linetype = "dashed") +
  scale_colour_manual(name = "",values = "red") +
  ggnewscale::new_scale_colour() +
  aes(x=Distribution, y = pvalues, colour = Test) + 
  geom_boxplot() +
  scale_colour_manual(label =c("Multimodality test",
                               "Selective test (direct)", 
                               "Merging selective test"),
                      values = pal2) +
  scale_x_discrete(labels=c("chi2(4)"=expression(chi(4)),
                            "Gamma(4,1)"=expression(Gamma(4,1)),
                            "E(0.5)" = "E(0.5)",
                            "U(-10,10)"="U(-10,10)",
                            "N(0,2)"="N(0,2)",
                            "Poi(4)"="Poi(4)",
                            "Poi(50)"="Poi(50)"))+
  ylab("p-values") +
  theme_classic() +
  theme(strip.text = element_text(size = 12),
        legend.position = "bottom",
        axis.title = element_text(size = 14))

p_distri_H0_qq <- ggplot(pval_distri_H0)+ stat_qq(aes(sample=pvalues, colour=Test), distribution=qunif) + 
  geom_abline(slope=1, intercept=0, col="red") + xlab("Theoretical Quantiles") + 
  ylab("Empirical Quantiles") + 
  facet_wrap(~Distribution, labeller= label_parsed) +
  xlim(c(0, 1)) + ylim(c(0, 1)) + theme_classic(base_size=17) +
  
  scale_colour_manual(label = c("Multimodality test",
                                "Selective test (direct)"),
                      values = pal2)+
  theme(strip.text = element_text(size = 12),
        legend.position = "bottom",
        axis.title = element_text(size = 14), 
        axis.text.x = element_text(size = 10))


p_distri <- p_illu_distri + p_distri_H0_qq + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold", size = 14)) & theme(legend.position = "bottom",
                                                                                     legend.text = element_text(size = 12), 
                                                                                     legend.title = element_text(size = 14),
                                                                                     strip.text = element_text(size = 12),
                                                                                     axis.title = element_text(size = 14),
                                                                                     axis.text.x = element_text(size = 10),
                                                                                     axis.text.y = element_text(size = 10))
# plot_grid(p_illu_distri, p_distri_H0_qq, nrow = 1,
#                       rel_heights = c(1.5,1),
#                       labels = "AUTO") 
p_distri

ggsave(p_distri, filename = "supplementary/figures/FigureS6.pdf", 
       dpi = 600, 
       width = 300, 
       height = 200, 
       units = "mm")
