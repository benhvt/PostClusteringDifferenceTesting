# Figure 4 
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
library(palmerpenguins)
library(dendextend)
library("ggdendro")


data("penguins")
penguins_NA <- na.omit(penguins)
pal_appli <- c("#56423E", "#E09A8E", "#506CFC")
pal_clust3 <- wes_palette("BottleRocket2", n=3, type = c("discrete"))

penguins_scale <- penguins_NA
penguins_scale[,3:6] <- scale(penguins_scale[,3:6])

pspeciesbilllength <- ggplot(penguins_scale) + aes(x=bill_length_mm, colour = species, fill = species) + 
  geom_histogram(colour = "white", bins = 25, alpha = 0.9) +
  geom_density(aes(y=..count..), alpha = 0.4) +
  scale_colour_manual(name = "True species", values = pal_appli) +
  scale_fill_manual(name = "True species",values = pal_appli)  + 
  xlab("bill length (scaled)") +
  ylab("Count") +
  theme_classic() +
  theme(legend.position = "bottom", 
        axis.title = element_text(size = 14))


pspeciesbilldepth <- ggplot(penguins_scale) + aes(x=bill_depth_mm, colour = species, fill = species) + 
  geom_histogram(colour = "white", bins = 25, alpha = 0.9) +
  geom_density(aes(y=..count..), alpha = 0.4) +
  scale_colour_manual(values = pal_appli) +
  scale_fill_manual(values = pal_appli)  + 
  xlab("bill depth (scaled)") +
  ylab("Count") +
  theme_classic() +
  theme(axis.title = element_text(size = 14))


pspeciesflipperlength <- ggplot(penguins_scale) + aes(x=flipper_length_mm, colour = species, fill = species) +
  geom_histogram(colour = "white", bins = 25, alpha = 0.9) +
  geom_density(aes(y=..count..), alpha = 0.4) +
  scale_colour_manual(values = pal_appli) +
  scale_fill_manual(values = pal_appli)  + 
  xlab("flipper length (scaled)") +
  ylab("Count") +
  theme_classic() +
  theme(axis.title = element_text(size = 14))

pspeciesbodymass <- ggplot(penguins_scale) + aes(x=body_mass_g, colour = species, fill = species) + 
  geom_histogram(colour = "white", bins = 25, alpha = 0.9) +
  geom_density(aes(y=..count..), alpha = 0.4) +
  scale_colour_manual(values = pal_appli) +
  scale_fill_manual(values = pal_appli)  + 
  xlab("body mass (scaled)") +
  ylab("Count") +
  theme_classic() +
  theme(axis.title = element_text(size = 14))

phistspecies <- plot_grid(pspeciesbilllength + theme(legend.position="none", 
                                                     plot.margin = margin(7, 7, 7, 17)), 
                          pspeciesbilldepth + theme(legend.position="none", 
                                                    plot.margin = margin(7, 7, 7, 17)),
                          pspeciesflipperlength + theme(legend.position="none", 
                                                        plot.margin = margin(7, 7, 7, 17)),
                          pspeciesbodymass + theme(legend.position="none", 
                                                   plot.margin = margin(7, 7, 7, 17)),
                          ncol = 4,
                          hjust = -1)
# add legend
legend <- get_legend(
  # create some space to the left of the legend
  pspeciesbilllength + theme(legend.box.margin = margin(0, 0, 0, 12))
)

dend <- penguins_NA[,c(3:6)] %>% scale %>% dist %>% 
  hclust(method = "ward.D2") %>% as.dendrogram %>% 
  set("labels", "")%>%
  set("branches_k_color", k=3) %>% set("branches_lwd", 1)
# plot the dend in usual "base" plotting engine:
penguins_NA$Cluster <- cutree(dend,k = 3) 

penguins_NA$Cluster <- as.factor(penguins_NA$Cluster)
p_to_leg <- ggplot(penguins_NA) + aes(x=bill_length_mm, fill = Cluster) + geom_histogram() +
  scale_fill_manual(name = "Estimated cluster", values = pal_clust3) +
  theme(legend.position = "bottom")

legend_cluster <- get_legend(
  # create some space to the left of the legend
  p_to_leg + theme(legend.box.margin = margin(0, 0, 0, 12)))

legend_fin <- plot_grid(legend, legend_cluster, nrow = 2, vjust = 1)  
phistspecies_legend <- plot_grid(phistspecies, legend_fin, nrow = 2, rel_heights = c(5, .9))


pal_clust3 <- wes_palette("BottleRocket2", n=3, type = c("discrete"))
ggd1 <- as.ggdend(dend)
p_dend <- ggplot(ggd1, theme = theme_classic())+scale_color_manual(values = c(pal_clust3[1], pal_clust3[3], pal_clust3[2], "grey")) +
  xlab("") +
  ylab("Height") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size = 14))
p_perf <- ggplot(penguins_NA) + aes(x=Cluster, fill = species) + 
  geom_bar(position="fill") +
  scale_fill_manual(name = "True Species", values = pal_appli) +
  theme_classic() +
  theme(legend.position = "none", 
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size=12,colour = c(pal_clust3)),
        axis.title.x = element_blank())+
  ylab("Proportion") 


p_cluster <- plot_grid(p_dend, p_perf, ncol=2)

# Make figure
p_appli <- phistspecies_legend / p_cluster + 
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold')) 
p_appli
ggsave(p_appli, file = "figures/figure4.pdf", dpi = 600, width = 300, height = 200, units = "mm")
