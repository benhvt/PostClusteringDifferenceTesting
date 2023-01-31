#--- HIPC data analysis ---#

# install.packages("cytometree")
rm(list=ls())
library(ggplot2)
library(GGally)
library(patchwork)
library(rstatix)
library(ggpubr)
library(VALIDICLUST)
library(MetBrewer)
library(cytometree)
library(dplyr)
library(kableExtra)
source("utils.R")

# Load data from the cytometree package
data("HIPC")

# Convert data to data frame and recode labels modality
HIPC <- HIPC %>% data.frame() %>%
  mutate(label = as.factor(label))
HIPC$label <- forcats::fct_recode(HIPC$label,"CD8 Effector" = "1",
                                  "CD8 Naive" = "2",
                                  "CD8 Central Memory"= "3",
                                  "CD8 Effector Memory" = "4",
                                  "CD8 Activated" = "5",
                                  "CD4 Effector" = "6",
                                  "CD4 Naive" = "7",
                                  "CD4 Central Memory" = "8", 
                                  "CD4 Effector Memory" = "9", 
                                  "CD4 Activated" = "10")

# Keep only the cells in one of the following population: "CD8 Naive", "CD8 Effector Memory", "CD4 Naive", "CD4 Effector Memory")
HIPC_samp <- HIPC %>%
  filter(label %in% c("CD8 Naive", "CD8 Effector Memory", "CD4 Naive", "CD4 Effector Memory"))
HIPC_samp$label <- droplevels(HIPC_samp$label)


# Keep only 5% of cells of each population
set.seed(2102022)
prop <- 0.05
HIPC_sub <- HIPC_samp %>%
  group_by(label) %>% 
  slice_sample(prop = prop)

# Clustering step (on scaled data)
HIPC_scale <- HIPC_sub %>%
  ungroup() %>%
  dplyr:: select(-c(label)) %>% 
  scale()

## Define function for clustering 
cl_fun <- function(x){cutree(fastcluster::hclust(dist(x), method = "ward.D2"), k=4)}

clust <- as.factor(cl_fun(HIPC_scale))

## Comparaison with the ground truth 
ARI <- mclust::adjustedRandIndex(HIPC_sub$label, clust)

# Inference step:
## Test on cluster 1
C1C2 <- apply3test(HIPC_scale, cl_fun = cl_fun, cl = clust, k1=1, k2=2)
C1C3 <- apply3test(HIPC_scale, cl_fun = cl_fun, cl = clust, k1=1, k2=3)
C1C4 <- apply3test(HIPC_scale, cl_fun = cl_fun, cl = clust, k1=1, k2=4)

## Test on cluster 2
C2C3 <- apply3test(HIPC_scale, cl_fun = cl_fun, cl = clust, k1=2, k2=3)
C2C4 <- apply3test(HIPC_scale, cl_fun = cl_fun, cl = clust, k1=2, k2=4)

## Test on cluster 3  
C3C4 <- apply3test(HIPC_scale, cl_fun = cl_fun, cl = clust, k1=3, k2=4)


# Results 

## Main Results 

### Figures
plt_multimodality_test_res <- box_pval(HIPC_scale, test = "Multimod", var = "CCR7", clust = clust) + 
  box_pval(HIPC_scale, test = "Multimod", var = "CD4", clust = clust)+
  box_pval(HIPC_scale, test = "Multimod", var = "CD45RA", clust = clust) +
  box_pval(HIPC_scale, test = "Multimod", var = "HLADR", clust = clust) +
  box_pval(HIPC_scale, test = "Multimod", var = "CD38", clust = clust) +
  box_pval(HIPC_scale, test = "Multimod", var = "CD8", clust = clust) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 18),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))
ggsave(plt_multimodality_test_res, filename = "figures/figure5.pdf",
       width = 300, height = 200, units = "mm")

 plt_multimodality_test_res
### Table
rownames(C1C2$pval) <- rownames(C1C3$pval) <- rownames(C1C4$pval) <- rownames(C2C3$pval) <- rownames(C2C4$pval) <- rownames(C3C4$pval) <- colnames(HIPC_scale)

colnames(C1C2$pval) <- colnames(C1C3$pval) <- colnames(C1C4$pval) <- colnames(C2C3$pval) <- colnames(C2C4$pval) <- colnames(C3C4$pval) <- c("Selective test", "Merging test", "Mutlimodality test")
restab <- table(clust, HIPC_sub$label)
proptab <- prop.table(restab, margin = 1)
C2C4$pval %>% round(4) %>% 
  xtable::xtable(digits = 4,
                 caption = paste("Comparison between Cluster 2 (", 
                                 round(proptab[2,"CD8 Effector Memory"],2)*100, 
                                 "% of CD8 Effector Memory cells) and Cluster 4 (",
                                 round(proptab[4,"CD4 Effector Memory"],2)*100,"% of CD4 Effector Memory cells)", sep = ""), 
                 label = "tab:resC2C4HIPC")

## Supplementary results 

### Supplementary Figures 

pairplot <- data.frame(HIPC_scale) %>% 
  data.frame() %>%
  mutate(`Knwon cell types` = HIPC_sub$label)%>%
  ggpairs(aes(colour = `Knwon cell types`), 
          lower = list(continuous = "density", combo = wrap("autopoint", alpha = .5)),
          upper = list(continuous = wrap("points", alpha = 0.5))) +
  scale_colour_met_d("Archambault") +
  scale_fill_met_d("Archambault") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text  = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16)) 

ggsave(pairplot, filename = "supplementary/figures/FigureS6.pdf",
       width = 375, 
       height = 400, 
       units = "mm",
       dpi = 600)


### Supplementary table

pval_results <- data.frame(Markers = rep(colnames(HIPC_scale), 6))
pval_results$`Selective test` <- c(C1C2$pval[,1],
                                   C1C3$pval[,1],
                                   C1C4$pval[,1],
                                   C2C3$pval[,1],
                                   C2C4$pval[,1],
                                   C3C4$pval[,1])

pval_results$`Merging test` <- c(C1C2$pval[,2],
                                 C1C3$pval[,2],
                                 C1C4$pval[,2],
                                 C2C3$pval[,2],
                                 C2C4$pval[,2],
                                 C3C4$pval[,2])

pval_results$`Multimodality test` <- c(C1C2$pval[,3],
                                       C1C3$pval[,3],
                                       C1C4$pval[,3],
                                       C2C3$pval[,3],
                                       C2C4$pval[,3],
                                       C3C4$pval[,3])


add_percentage_ground_truth <- function(cluster, clust1, clust2, label, lab1, lab2){
  restab <- table(cluster, label)
  proptab <- prop.table(restab, margin = 1)
  return(paste("Cluster", clust1, " (", 
               round(proptab[clust1, lab1],2)*100, 
               "% of ", lab1, ") vs Cluster", clust2, " (",
               round(proptab[clust2,lab2],2)*100,"% of ", lab2, ")", sep = ""))
}


pval_results %>% 
  mutate(`Selective test` = round(`Selective test`,4)) %>%
  mutate(`Merging test` = round(`Merging test`, 4)) %>%
  mutate(`Multimodality test` = round(`Multimodality test`,4)) %>%
  mutate(`Selective test`, `Selective test` = ifelse(`Selective test`<0.05, paste(`Selective test`, "*", sep = ""),`Selective test`)) %>%
  mutate(`Merging test`, `Merging test` = ifelse(`Merging test`<0.05, paste(`Merging test`, "*", sep = ""), `Merging test`)) %>%
  mutate(`Multimodality test`, `Multimodality test` = ifelse(`Multimodality test`<0.05, paste(`Multimodality test`, "*", sep = ""), `Multimodality test`)) %>%
  kbl(digits = 4, booktabs = T, longtable = T, format = "latex") %>%
  kable_styling(font_size = 30) %>%
  kable_classic(full_width = T, html_font = "Cambria") %>%
  row_spec(0,bold=TRUE) %>%
  pack_rows(add_percentage_ground_truth(cluster = clust, clust1 = 1, clust2 = 2, label = HIPC_sub$label, lab1 = "CD8 Naive", lab2 = "CD8 Effector Memory"), 1, 6) %>%
  pack_rows(add_percentage_ground_truth(cluster = clust, clust1 = 1, clust2 = 3, label = HIPC_sub$label, lab1 = "CD8 Naive", lab2 = "CD4 Naive"), 7, 12) %>%
  pack_rows(add_percentage_ground_truth(cluster = clust, clust1 = 1, clust2 = 4, label = HIPC_sub$label, lab1 = "CD8 Naive", lab2 = "CD4 Effector Memory"), 13, 18) %>%
  pack_rows(add_percentage_ground_truth(cluster = clust, clust1 = 2, clust2 = 3, label = HIPC_sub$label, lab1 = "CD8 Effector Memory", lab2 = "CD4 Naive"), 19, 24) %>%
  pack_rows(add_percentage_ground_truth(cluster = clust, clust1 = 2, clust2 = 4, label = HIPC_sub$label, lab1 = "CD8 Effector Memory", lab2 = "CD4 Effector Memory"), 25, 30) %>%
  pack_rows(add_percentage_ground_truth(cluster = clust, clust1 = 3, clust2 = 4, label = HIPC_sub$label, lab1 = "CD4 Naive", lab2 = "CD4 Effector Memory"), 31, 36) %>%
  kable_styling()
