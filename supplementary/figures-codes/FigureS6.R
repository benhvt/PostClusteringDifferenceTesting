################################################################################
#            Supplementary Figure 6                                            #
################################################################################

library(ggplot2)
library(MetBrewer)
library(dplyr)
library(latex2exp)
library(kableExtra)

pal <- wesanderson::wes_palette("Darjeeling1", 3, type = "continuous")
pal2 <- c(wesanderson::wes_palette("GrandBudapest2", 2, type = "continuous"), 
          "#cad3fa", pal[2])


#--------- Figure 6 : Comparison with clustvarsel  -------------------#

results_files <-  list.files("supplementary/simulations-results/results_figureS6/", 
                             pattern = c(".csv"), full.names = TRUE)
length(results_files)
results_read <- lapply(results_files, readr::read_csv)

results <- do.call("rbind.data.frame", results_read) %>%
  group_by(Variable, Scenario, FeatureSelection, Clustering, delta) %>%
  mutate(delta = paste0("delta==", delta)) %>%
  mutate(FeatureSelection = ifelse(FeatureSelection == "Mclust", "clustvarsel", 
                                   ifelse(FeatureSelection == "SI", "Selective test",
                                          "Multimodality test"))) %>%
  mutate(Clustering = ifelse(Clustering == "MClust", "Mclust", 
                             ifelse(Clustering == "CAH", 
                                    "Hierarchical Clustering", Clustering))) %>%
  mutate(Scenario = gsub("S", "Scenario~", Scenario)) %>%
  mutate(Clustering=ifelse(Clustering == "CAH", "Hierarchical clustering", Clustering)) %>%
  summarise(Indicator = mean(Selection, na.rm = T))


plt_indicator <- ggplot(results) +
  aes(x=Variable, y = Indicator, fill =FeatureSelection ) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Method",
                    values = c("#BF0A30",pal2[1:2]), 
                    labels = c("clustvarsel", 
                               "Multimodality test",
                               "Selective test")) +
  geom_hline(aes(yintercept = 0.05, colour = "5% nominal levels"), linetype = "dotted", size = 1.5) +
  scale_colour_manual(name = "", values = "#FF7F11") +
  ggh4x::facet_nested(factor(Clustering)~Scenario+delta, labeller = labeller(.cols = label_parsed)) +
  ylab("Proportion of selection") +
  theme_light() +
  theme(text = element_text(size = 18),
        legend.position = "bottom", 
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        panel.grid = element_blank())

plt_time <- do.call("rbind.data.frame", results_read) %>%
  filter(Variable == "X1") %>%
  mutate(FeatureSelection = ifelse(FeatureSelection == "Mclust", "clustvarsel", 
                                   ifelse(FeatureSelection == "SI", "Selective test",
                                          "Multimodality test"))) %>%
  mutate(Clustering = ifelse(Clustering == "MClust", "Mclust", 
                             ifelse(Clustering == "CAH", 
                                    "Hierarchical Clustering", Clustering))) %>%
  ggplot() + aes(x=Clustering, y = Time, colour = FeatureSelection, fill = FeatureSelection) +
  geom_boxplot(alpha = .2, size = .8) +
  scale_discrete_manual(name = "Method",
                        aesthetics = c("fill", "colour"),
                        values = c("#BF0A30",pal2[1:2]), 
                        labels = c("clustvarsel", 
                                   "Multimodality test",
                                   "Selective test")) +
  scale_y_log10() + 
  ylab("Computational time (in secondes)") +
  xlab("Clustering method") +
  annotation_logticks(side = "l") +
  theme_light() +
  theme(text = element_text(size = 18),
        legend.position = "bottom", 
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))


plt_indicator / plt_time + plot_layout(heights = c(2.5, 1.5)) +  
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face = 'bold'))


ggsave(filename = "supplementary/figures/FigureS6.pdf",
       width = 250, 
       height = 300, 
       units = "mm", 
       dpi = 600)
  

