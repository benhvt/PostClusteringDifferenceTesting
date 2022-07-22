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
library(MetBrewer)


data("penguins")
penguins_NA <- na.omit(penguins)
pal_clust3  <- MetBrewer::met.brewer("Egypt", n=3, type = "discrete")
pal_appli<- MetBrewer::met.brewer("Cross", n=3, type = "discrete")

penguins_scale <- penguins_NA
penguins_scale[,3:6] <- scale(penguins_scale[,3:6])

# pspeciesbilllength <- ggplot() + geom_histogram(data = subset(penguins_scale, species == "Adelie"), 
#                                                 aes(x=bill_length_mm, y=..density.., colour = species, fill = species),
#                                                 colour = "white", bins = 25, alpha = 0.6) + 
#   geom_density(data = subset(penguins_scale, species == "Adelie"), 
#                aes(x=bill_length_mm, y=..density.., colour = species, fill = species),
#                alpha = 0.1, size = .8) +
#   scale_colour_manual(name = "", values = pal_appli[1]) +
#   scale_fill_manual(name = "", values = pal_appli[1]) +
#   ggnewscale::new_scale_colour() + 
#   ggnewscale::new_scale_fill() +
#   geom_histogram(data = subset(penguins_scale, species == "Gentoo"), 
#                  aes(x=bill_length_mm, y=..density.., colour = species, fill = species),
#                  colour = "white", bins = 25, alpha = 0.6) + 
#   geom_density(data = subset(penguins_scale, species == "Gentoo"), 
#                aes(x=bill_length_mm, y=..density.., colour = species, fill = species),
#                alpha = 0.1, size = .8) +
#   scale_colour_manual(name = "", values = pal_appli[3]) +
#   scale_fill_manual(name = "", values = pal_appli[3]) +
#   ggnewscale::new_scale_colour() + 
#   ggnewscale::new_scale_fill() +
#   geom_histogram(data = subset(penguins_scale, species == "Chinstrap"), 
#                  aes(x=bill_length_mm, y=..density.., colour = species, fill = species),
#                  colour = "white", bins = 25, alpha = 0.6) + 
#   geom_density(data = subset(penguins_scale, species == "Chinstrap"), 
#                aes(x=bill_length_mm, y=..density.., colour = species, fill = species),
#                alpha = 0.1, size = .8) +
#   scale_colour_manual(name = "", values = pal_appli[2]) +
#   scale_fill_manual(name = "", values = pal_appli[2]) +
#   xlab("bill length (scaled)") +
#   ylab("Density") +
#   theme_classic() +
#   theme(legend.position = "bottom", 
#         axis.title = element_text(size = 16))
# 
# 
# pspeciesbilldepth <- ggplot() + geom_histogram(data = subset(penguins_scale, species == "Adelie"), 
#                                                aes(x=bill_depth_mm, y=..density.., colour = species, fill = species),
#                                                colour = "white", bins = 25, alpha = 0.6) + 
#   geom_density(data = subset(penguins_scale, species == "Adelie"), 
#                aes(x=bill_depth_mm, y=..density.., colour = species, fill = species),
#                alpha = 0.1, size = .8) +
#   scale_colour_manual(name = "", values = pal_appli[1]) +
#   scale_fill_manual(name = "", values = pal_appli[1]) +
#   ggnewscale::new_scale_colour() + 
#   ggnewscale::new_scale_fill() +
#   geom_histogram(data = subset(penguins_scale, species == "Gentoo"), 
#                  aes(x=bill_depth_mm, y=..density.., colour = species, fill = species),
#                  colour = "white", bins = 25, alpha = 0.6) + 
#   geom_density(data = subset(penguins_scale, species == "Gentoo"), 
#                aes(x=bill_depth_mm, y=..density.., colour = species, fill = species),
#                alpha = 0.1, size = .8) +
#   scale_colour_manual(name = "", values = pal_appli[3]) +
#   scale_fill_manual(name = "", values = pal_appli[3]) +
#   ggnewscale::new_scale_colour() + 
#   ggnewscale::new_scale_fill() +
#   geom_histogram(data = subset(penguins_scale, species == "Chinstrap"), 
#                  aes(x=bill_depth_mm, y=..density.., colour = species, fill = species),
#                  colour = "white", bins = 25, alpha = 0.6) + 
#   geom_density(data = subset(penguins_scale, species == "Chinstrap"), 
#                aes(x=bill_depth_mm, y=..density.., colour = species, fill = species),
#                alpha = 0.1, size = .8) +
#   scale_colour_manual(name = "", values = pal_appli[2]) +
#   scale_fill_manual(name = "", values = pal_appli[2]) +
#   xlab("bill depth (scaled)") +
#   ylab("Density") +
#   theme_classic() +
#   theme(legend.position = "bottom", 
#         axis.title = element_text(size = 16))
# 
# 
# pspeciesflipperlength <- ggplot() + geom_histogram(data = subset(penguins_scale, species == "Adelie"), 
#                                                    aes(x=flipper_length_mm, y=..density.., colour = species, fill = species),
#                                                    colour = "white", bins = 25, alpha = 0.6) + 
#   geom_density(data = subset(penguins_scale, species == "Adelie"), 
#                aes(x=flipper_length_mm, y=..density.., colour = species, fill = species),
#                alpha = 0.1, size = .8) +
#   scale_colour_manual(name = "", values = pal_appli[1]) +
#   scale_fill_manual(name = "", values = pal_appli[1]) +
#   ggnewscale::new_scale_colour() + 
#   ggnewscale::new_scale_fill() +
#   geom_histogram(data = subset(penguins_scale, species == "Gentoo"), 
#                  aes(x=flipper_length_mm, y=..density.., colour = species, fill = species),
#                  colour = "white", bins = 25, alpha = 0.6) + 
#   geom_density(data = subset(penguins_scale, species == "Gentoo"), 
#                aes(x=flipper_length_mm, y=..density.., colour = species, fill = species),
#                alpha = 0.1, size = .8) +
#   scale_colour_manual(name = "", values = pal_appli[3]) +
#   scale_fill_manual(name = "", values = pal_appli[3]) +
#   ggnewscale::new_scale_colour() + 
#   ggnewscale::new_scale_fill() +
#   geom_histogram(data = subset(penguins_scale, species == "Chinstrap"), 
#                  aes(x=flipper_length_mm, y=..density.., colour = species, fill = species),
#                  colour = "white", bins = 25, alpha = 0.6) + 
#   geom_density(data = subset(penguins_scale, species == "Chinstrap"), 
#                aes(x=flipper_length_mm, y=..density.., colour = species, fill = species),
#                alpha = 0.1, size = .8) +
#   scale_colour_manual(name = "", values = pal_appli[2]) +
#   scale_fill_manual(name = "", values = pal_appli[2]) +
#   xlab("flipper length (scaled)") +
#   ylab("Density") +
#   theme_classic() +
#   theme(legend.position = "bottom", 
#         axis.title = element_text(size = 16))
# 
# pspeciesbodymass <- ggplot() + geom_histogram(data = subset(penguins_scale, species == "Chinstrap"), 
#                                               aes(x=body_mass_g, y=..density.., colour = species, fill = species),
#                                               colour = "white", bins = 25, alpha = 0.6) + 
#   geom_density(data = subset(penguins_scale, species == "Chinstrap"), 
#                aes(x=body_mass_g, y=..density.., colour = species, fill = species),
#                alpha = 0.1, size = .8) +
#   scale_colour_manual(name = "", values = pal_appli[2]) +
#   scale_fill_manual(name = "", values = pal_appli[2]) +
#   ggnewscale::new_scale_colour() + 
#   ggnewscale::new_scale_fill() +
#   geom_histogram(data = subset(penguins_scale, species == "Adelie"), 
#                  aes(x=body_mass_g, y=..density.., colour = species, fill = species),
#                  colour = "white", bins = 25, alpha = 0.6) + 
#   geom_density(data = subset(penguins_scale, species == "Adelie"), 
#                aes(x=body_mass_g, y=..density.., colour = species, fill = species),
#                alpha = 0.1, size = .8) +
#   scale_colour_manual(name = "", values = pal_appli[1]) +
#   scale_fill_manual(name = "", values = pal_appli[1]) +
#   ggnewscale::new_scale_colour() + 
#   ggnewscale::new_scale_fill() +
#   geom_histogram(data = subset(penguins_scale, species == "Gentoo"), 
#                                               aes(x=body_mass_g, y=..density.., colour = species, fill = species),
#                                               colour = "white", bins = 25, alpha = 0.6) + 
#   geom_density(data = subset(penguins_scale, species == "Gentoo"), 
#                aes(x=body_mass_g, y=..density.., colour = species, fill = species),
#                alpha = 0.1, size = .8) +
#   scale_colour_manual(name = "", values = pal_appli[3]) +
#   scale_fill_manual(name = "", values = pal_appli[3]) +
#   xlab("body mass (scaled)") +
#   ylab("Density") +
#   theme_classic() +
#   theme(legend.position = "bottom", 
#         axis.title = element_text(size = 16))
# 
# phistspecies <- plot_grid(pspeciesbilllength + theme(legend.position="none", 
#                                                      plot.margin = margin(7, 7, 7, 17)), 
#                           pspeciesbilldepth + theme(legend.position="none", 
#                                                     plot.margin = margin(7, 7, 7, 17)),
#                           pspeciesflipperlength + theme(legend.position="none", 
#                                                         plot.margin = margin(7, 7, 7, 17)),
#                           pspeciesbodymass + theme(legend.position="none", 
#                                                    plot.margin = margin(7, 7, 7, 17)),
#                           ncol = 4,
#                           hjust = -1)
# # add legend
# legend <- get_legend(
#   # create some space to the left of the legend
#   pspeciesbilllength + theme(legend.box.margin = margin(0, 0, 0, 16))
# )

melt_penguins_scale <- reshape2:: melt(penguins_scale, measure.vars = 3:6)
melt_penguins_scale$variable <- stringr::str_replace(melt_penguins_scale$variable, pattern = "_", replacement = " ")
melt_penguins_scale$variable <- stringr::str_remove(melt_penguins_scale$variable, pattern = "_mm")
melt_penguins_scale$variable <- stringr::str_remove(melt_penguins_scale$variable, pattern = "_g")
melt_penguins_scale$variable <- paste(melt_penguins_scale$variable, "(scaled)", sep =" ")

phistspecies <- ggplot(melt_penguins_scale) + aes(x=value, fill = species, colour = species) +
  geom_histogram(aes(y=..density..), bins = 25, colour = "white", alpha = 0.8) +
  facet_wrap(~variable, nrow = 1) +
  scale_fill_manual(values = pal_appli) +
  ggnewscale::new_scale_colour()+
  geom_density(data = melt_penguins_scale, aes(x=value, colour = species, fill=species), alpha = .1, size = .8) +
  scale_colour_manual(values = c("#AD5748", "#C38A28", "#395E66")) +
  guides(fill= guide_legend("Species"), 
         colour = guide_legend("Species")) +
  theme_classic() +
  theme(axis.title = element_text(size = 16),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size=16)) +
  xlab("Scaled values") +
  ylab("Density") 

legend <- get_legend(
  # create some space to the left of the legend
  phistspecies + theme(legend.box.margin = margin(0, 0, 0, 16))
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
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

legend_cluster <- get_legend(
  # create some space to the left of the legend
  p_to_leg + theme(legend.box.margin = margin(0, 0, 0, 16)))

legend_fin <- plot_grid(legend, legend_cluster, nrow = 2, vjust = 5.2)
phistspecies_legend <- plot_grid(phistspecies+theme(legend.position = "none"), legend_fin, nrow = 2, rel_heights = c(5, .9))


#pal_clust3 <- wes_palette("BottleRocket2", n=3, type = c("discrete"))
ggd1 <- as.ggdend(dend)
p_dend <- ggplot(ggd1, theme = theme_classic())+
  scale_color_manual(values = c(pal_clust3[1], pal_clust3[3], pal_clust3[2], "grey")) +
  xlab("Observations") +
  ylab("Ward's criterion") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title = element_text(size = 16))
p_perf <- ggplot(penguins_NA) + aes(x=Cluster, fill = species) + 
  geom_bar(position="fill") +
  scale_fill_manual(name = "True Species", values = pal_appli) +
  theme_classic() +
  theme(legend.position = "none", 
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size=16,colour = c(pal_clust3)))+
  ylab("True species composition (Proportion)") + 
  xlab("Estimated clusters")


p_cluster <- plot_grid(p_dend, p_perf, ncol=2)

# Make figure

# p_appli <- phistspecies_legend / p_cluster + 
#   plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold')) 


p_appli <- phistspecies_legend / (p_dend + p_perf) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold')) 
p_appli
ggsave(p_appli, file = "figures/figure4.pdf", dpi = 600, width = 330, height = 230, units = "mm")
