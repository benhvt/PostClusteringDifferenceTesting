#Figure S4

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
source(file = "utils.R")

theme_set(theme_bw())

pal_illu <- c("#399283", "#e1d936", "#e09c6c", "#dc3c07")
pal <- wes_palette("Darjeeling1", 3, type = "continuous")
pal2 <- c(wes_palette("GrandBudapest2", 2, type = "continuous"), "#cad3fa", pal[2])
pal8 <- c(pal_illu[1], pal_illu[4], pal_illu[3], pal_illu[2], "#D0B49F", "#AB6B51", "#2F435A", "#A47786")


# Results over 2 000 simulations of the data 

## Under the null
K <- 3:7
nsimu <- 2000
pval_bonf_H0 <- read.csv(file = "supplementary/simulations-results/results_figureS4/pval_merge_bonf_H0.csv")
pval_geo_H0 <- read.csv(file = "supplementary/simulations-results/results_figureS4/pval_merge_geo_H0.csv")
pval_harm_H0 <- read.csv(file = "supplementary/simulations-results/results_figureS4/pval_merge_harm_H0.csv")

K.df <- c()
for (i in 1:length(K)){
  K.df <- c(K.df, rep(K[i], nsimu))
}
pval_merge_comp_H0 <- data.frame(pvalues = c(as.numeric(as.matrix(pval_bonf_H0)),
                                             as.numeric(as.matrix(pval_geo_H0)),
                                             as.numeric(as.matrix(pval_harm_H0))),
                                 Method = c(rep("Bonferoni (min)", nrow(pval_bonf_H0)*length(K)),
                                            rep("Geometric Mean", nrow(pval_geo_H0)*length(K)),
                                            rep("Harmonic Mean", nrow(pval_harm_H0)*length(K))),
                                 K = as.factor(paste0("K=",K.df)))
pval_merge_comp_H0$pvalues <- sapply(pval_merge_comp_H0$pvalues, FUN = function(x){min(x, 1, na.rm=T)})


p_distri_pval_merge <- ggplot(pval_merge_comp_H0)+ 
  geom_hline(aes(yintercept = 0.05, colour ="5% level"), linetype = "dashed") +
  scale_colour_manual(name = " ", values = "red")  +
  ggnewscale::new_scale_colour() + aes(x=K, y = pvalues, colour = Method) +
  geom_boxplot() +
  scale_discrete_manual(name = "Merging function", aesthetics = c("colour"),
                        values = c("#899D78","#8A1C7C","#735CDD")) + 
  theme_classic() +
  xlab("Number of estimated clusters (K)") +
  ylab("p-values (under H0)") +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

fun_FP <- function(x){sum(x<0.05, na.rm = T)/length(na.omit(x))}
FP_merge_comp <- data.frame(K = rep(K,3), 
                            FP = c(apply(pval_bonf_H0, 2, 
                                         fun_FP),
                                   apply(pval_geo_H0, 2, 
                                         fun_FP),
                                   apply(pval_harm_H0, 2, 
                                         fun_FP)),
                            Method = c(rep("Bonferoni (min)", length(K)),
                                       rep("Geometric Mean", length(K)),
                                       rep("Harmonic Mean", length(K)))) 

p_FP_merge <- ggplot(FP_merge_comp) +
  geom_hline(aes(yintercept = 0.05, colour ="5% level"), linetype = "dashed") +
  scale_colour_manual(name = " ", values = "red")  +
  ggnewscale::new_scale_colour() +
  aes(x=K, y= FP, colour = Method) + 
  geom_point() + 
  geom_line() +
  xlab("Number of estimated clusters (K)") +
  ylab("False positive rate") +
  ylim(c(0,1)) +
  scale_colour_manual(name = "Merging function", values = c("#899D78","#8A1C7C","#735CDD")) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

## Under the alternative 
pval_bonf_H1 <- read.csv(file = "supplementary/simulations-results/results_figureS4/pval_merge_bonf_H1.csv")
pval_geo_H1 <- read.csv(file = "supplementary/simulations-results/results_figureS4/pval_merge_geo_H1.csv")
pval_harm_H1 <- read.csv(file = "supplementary/simulations-results/results_figureS4/pval_merge_harm_H1.csv")


pval_merge_comp_H1 <- data.frame(pvalues = c(as.numeric(as.matrix(pval_bonf_H1)),
                                             as.numeric(as.matrix(pval_geo_H1)),
                                             as.numeric(as.matrix(pval_harm_H1))),
                                 Method = c(rep("Bonferoni (min)", nrow(pval_bonf_H1)*length(K)),
                                            rep("Geometric Mean", nrow(pval_geo_H1)*length(K)),
                                            rep("Harmonic Mean", nrow(pval_harm_H1)*length(K))),
                                 K = as.factor(paste0("K=",K.df)))
pval_merge_comp_H1$pvalues <- sapply(pval_merge_comp_H1$pvalues, FUN = function(x){min(x, 1, na.rm=T)})

p_distri_pval_merge_H1 <- ggplot(pval_merge_comp_H1) + 
  geom_hline(aes(yintercept = 0.05, colour ="5% level"), linetype = "dashed") +
  scale_colour_manual(name = " ", values = "red")  +
  ggnewscale::new_scale_colour() +
  aes(x=K, y = pvalues, colour = Method) + 
  geom_boxplot() +
  xlab("Number of estimated clusters (K)") +
  ylab("p-values (under H1)") +
  scale_colour_manual(name = "Merging function", values = c("#899D78","#8A1C7C","#735CDD")) +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

## Statistical power
delta <- seq(0,8, 0.5)

pval_bonf_H1_pow <- read.csv(file = "supplementary/simulations-results/results_figureS4/power_comp_merge_bonf.csv")
pval_geo_H1_pow <- read.csv(file = "supplementary/simulations-results/results_figureS4/power_comp_merge_geo.csv")
pval_harm_H1_pow <- read.csv(file = "supplementary/simulations-results/results_figureS4/power_comp_merge_harm.csv")


pow_bonf <- apply(pval_bonf_H1_pow, 2, fun_FP)
pow_geo <- apply(pval_geo_H1_pow, 2, fun_FP)
pow_harm <- apply(pval_harm_H1_pow, 2, fun_FP)

pow_merge <- data.frame(Power = c(pow_bonf, pow_geo, pow_harm),
                        Method = c(rep("Bonferoni (min)", length(delta)),
                                   rep("Geometric Mean", length(delta)),
                                   rep("Harmonic Mean", length(delta))),
                        Delta = rep(delta,3))

p_power_merge <- ggplot(pow_merge) + 
  geom_hline(aes(yintercept = 0.05, colour ="5% level"), linetype = "dashed") +
  scale_colour_manual(name = " ", values = "red")  +
  ggnewscale::new_scale_colour() +
  aes(x=Delta, y = Power, colour = Method) + 
  geom_line() +
  geom_point() +
  scale_colour_manual(name = "Merging function", values = c("#899D78","#8A1C7C","#735CDD")) +
  xlab(TeX(r'(Value of the mean difference $\delta$)'))+
  ylab("Statistical power (5% levels)")  +
  theme(axis.title = element_text(size = 14),
        strip.text = element_text(size = 12)) 

# Make figure 
p_merging <- (p_distri_pval_merge |p_distri_pval_merge_H1) / (p_FP_merge|p_power_merge) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold", size = 14))

p_merging 

ggsave(p_merging, filename = "supplementary/figures/FigureS4.pdf", 
       dpi = 600, 
       width = 225, 
       height = 150, 
       units = "mm")
