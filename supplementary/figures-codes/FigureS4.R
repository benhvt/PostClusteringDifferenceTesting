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
library(multimode)
source(file = "utils.R")

theme_set(theme_bw())

# Results over 2000 simulations of the data 

## Under the null
nsimu <- 500
pval_null_U <- as.matrix(read.csv(file = "supplementary/simulations-results/multimode_test_unimodale_uniform.csv"))

pval_null_N <- as.matrix(read.csv(file = "supplementary/simulations-results/multimode_test_unimodale_gaussian.csv"))

pval_null_U.df <- data.frame(pvalues = as.numeric(pval_null_U), 
                             Test = c(rep("Silverman", nsimu), 
                                      rep("DipTest", nsimu),
                                      rep("Cheng and Hall", nsimu),
                                      rep("Ameijeiras", nsimu)))
pval_null_N.df <- data.frame(pvalues = as.numeric(pval_null_N), 
                             Test = pval_null_U.df$Test)
pval_null_U.df$Distribution <- "Uniform"
pval_null_N.df$Distribution <- "Gaussian"

pval_null <- rbind(pval_null_U.df, pval_null_N.df)

p_unimod <- ggplot(pval_null)+ aes(x=Distribution, y = pvalues, colour = Test) +
  geom_boxplot() +
  scale_colour_brewer(palette = "Dark2") +
  ylab("p-values (under H0)") + 
  ylim(c(0, 1)) + theme_classic(base_size=17) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        strip.text = element_text(size=12))

## Under the alternative
delta <- seq(0,8, 0.5)
power_multimod_delta <- read.csv(file = "supplementary/simulations-results/multimode_test_power.csv")
power_multimod_delta <- power_multimod_delta[,-1]


power_delta <- data.frame(Power = as.numeric(as.matrix(power_multimod_delta)),
                          Test = c(rep("Silverman", length(delta)), 
                                   rep("DipTest", length(delta)),
                                   rep("Cheng and Hall", length(delta)),
                                   rep("Ameijeiras", length(delta))),
                          delta = rep(delta, 4))

p_power_delta <- ggplot(power_delta) + aes(x=delta, y = Power, colour = Test) +
  geom_point() +
  geom_line() +
  scale_colour_brewer(palette = "Dark2") +
  xlab(TeX(r'(Value of the mean difference $\delta$)'))+
  ylab("Statistical power (5% levels)") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        strip.text = element_text(size=12))

nn <-  c(10,15, 25, 50, 75, 100, 200, 1000)


power_multimod_n <- as.matrix(read.csv(file="supplementary/simulations-results/multimode_test_power_n.csv",
                                       row.names = NULL))
time_multimod_n <- as.matrix(read.csv(file="supplementary/simulations-results/time_multimode.csv"))

power_multimod_n <- power_multimod_n[,-1]
time_multimod_n <- time_multimod_n[,-1]
multimod_n <- data.frame(N=rep(nn, 4), 
                         Power = as.numeric(power_multimod_n),
                         Time = as.numeric(time_multimod_n),
                         Test = c(rep("Silverman", length(nn)), 
                                  rep("DipTest", length(nn)),
                                  rep("Cheng and Hall", length(nn)),
                                  rep("Ameijeiras", length(nn))))

p_multimod_power_n <- ggplot(multimod_n) + aes(x=N, y=Power, colour = Test) + 
  geom_point() +
  geom_line() +
  scale_colour_brewer(palette = "Dark2") +
  xlab("Number of observations (n)") +
  ylab("Statistical power (5% levels)") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        strip.text = element_text(size=12))

p_multimod_time_n <- ggplot(multimod_n) + aes(x=N, y = Time, colour = Test) + 
  geom_point() +
  geom_line() +
  scale_colour_brewer(palette = "Dark2") +
  xlab("Number of observations (n)") +
  ylab("Mean computation time (sec)
       log10 scale") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        strip.text = element_text(size=12)) +
  scale_y_continuous(trans = 'log10')


# Make figure 

p_multimod<- (p_unimod+p_multimod_time_n )/(p_power_delta + p_multimod_power_n)  + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold", size = 14))

p_multimod
ggsave(p_multimod, filename = "supplementary/figures/FigureS4.pdf", 
       dpi = 600, 
       width = 225, 
       height = 150, 
       units = "mm")
