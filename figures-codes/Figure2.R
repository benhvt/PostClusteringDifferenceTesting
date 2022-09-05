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

# Parameters 
n <- 200
muH0 <- 0
muH1X1 <- c(-5, 5, 0)
muH1X2 <- c(0,0,10)

set.seed(15112021)
XH0 <- data.frame(X1 = rnorm(n, muH0), 
                  X2 = rnorm(n, muH0))
XH1 <- data.frame(X1 = c(rnorm(n/3, mean = muH1X1[1]),
                         rnorm(n/3, mean = muH1X1[2]),
                         rnorm(n/3, mean = muH1X1[3])),
                  X2 = c(rnorm(n/3, mean = muH1X2[1]),
                         rnorm(n/3, mean = muH1X2[2]),
                         rnorm(n/3, mean =muH1X2[3])))
X<- data.frame(rbind(XH0, XH1), 
               Cluster = as.factor(c(hcl3(XH0), hcl3(XH1))),
               Hypothesis = c(rep("(i)", nrow(XH0)), 
                              rep("(ii)", nrow(XH1))))

hypothesis.labs <- c("No cluster", "3 clusters")
names(hypothesis.labs) <- c("(i)", "(ii)")

pbehav_illu <- ggplot(X) + aes(x=X1, y = X2, colour = Cluster) +
  geom_point() +
  scale_colour_manual(values = c(pal_illu[1], pal_illu[4], pal_illu[3])) +
  facet_wrap(~Hypothesis, scale = "free",  labeller = labeller(Hypothesis = hypothesis.labs)) +
  labs(x=expression(X[1]), 
       y=expression(X[2])) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        strip.text = element_text(size=12))
pbehav_illu + ggtitle("Experience")

# Results over 2 000 simulations of the data
nsimu <- 2000

pval_H0 <- read.csv(file = "simulations-results/simuH0.csv")
pval_H1 <- read.csv(file = "simulations-results/simuH1.csv")

pval_H0.df <- data.frame(pvalues = c(as.numeric(as.matrix(pval_H0))),
                         Method = c(rep("Selective Test", 6*nsimu),
                                    rep("Mulitmodality Test", 6*nsimu),
                                    rep("t-test", 6*nsimu),
                                    rep("Selective Test 2", 6*nsimu)),
                         Variable = rep(c(rep("X[1]", 3*nsimu),
                                          rep("X[2]", 3*nsimu)),4),
                         Test = rep(c(rep("C1vsC2", nsimu),
                                      rep("C1vsC3", nsimu),
                                      rep("C2vsC3",nsimu)),8),
                         Hypothesis = "(i)")
pval_H1.df <- data.frame(pvalues = c(as.numeric(as.matrix(pval_H1))),
                         Method = c(rep("Selective Test", 6*nsimu),
                                    rep("Mulitmodality Test", 6*nsimu),
                                    rep("t-test", 6*nsimu),
                                    rep("Selective Test 2", 6*nsimu)),
                         Variable = rep(c(rep("X[1]", 3*nsimu),
                                          rep("X[2]", 3*nsimu)),4),
                         Test = rep(c(rep("C1vsC2", nsimu),
                                      rep("C1vsC3", nsimu),
                                      rep("C2vsC3",nsimu)),8),
                         Hypothesis = "(ii)")
pval_2D <- rbind(pval_H0.df, pval_H1.df)
variables.labs <- c('X1', "X2")
names(variables.labs) <- c("X[1]", "X[2]")
pbehav_res <- ggplot(pval_2D) + aes(x=Test, y = pvalues, colour = Method, fill = Method) +
  geom_boxplot(alpha=.4) +
  scale_discrete_manual(c("color", "fill"), values = pal2, labels = lapply(c("Multimodality test", 
                                                                             r'(Selective test : $p_g^{C_k, C_l}$)',
                                                                             r'(Selective test : $p_g^{C_k:C_l}$)',
                                                                             "t-test"), TeX)) + 
  facet_grid(Variable~Hypothesis, labeller = labeller(Hypothesis = hypothesis.labs, Variable = variables.labs) ) +
  ylab("p-values") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        strip.text = element_text(size=12))
pbehav_res +  ggtitle("Results for 2000 simualtions of the data")

# Make figures 
pbehav <- pbehav_illu / pbehav_res + 
  plot_layout(heights = c(1, 2), widths = c(2,1)) + 
  plot_annotation(tag_levels = 'A')  & theme(plot.tag = element_text(face = 'bold')) 
ggsave(pbehav, file = "figures/figure2.png", dpi = 600, width = 155, height = 232.5, units = "mm")
