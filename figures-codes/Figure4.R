################################################################################
#                               Figure 4                                       #
################################################################################

library(ggplot2)
library(MetBrewer)
library(dplyr)
library(latex2exp)
library(kableExtra)
library(patchwork)

pal <- wesanderson::wes_palette("Darjeeling1", 3, type = "continuous")
pal2 <- c(wesanderson::wes_palette("GrandBudapest2", 2, type = "continuous"), 
          "#cad3fa", pal[2])


#--------- Figure 4 : 2 Scenario under the Gaussian Setting -------------------#

gaussian_results_files <-  list.files("simulations-results/results_figure4/", 
                                      pattern = c(".csv"), full.names = TRUE)
length(gaussian_results_files)
gaussian_results_read <- lapply(gaussian_results_files, readr::read_csv)

gaussian_results <- do.call("rbind.data.frame", gaussian_results_read) %>%
  group_by(Variable, Scenario, FeatureSelection, Clustering, delta) %>%
  mutate(delta = paste0("delta==", delta)) %>%
  mutate(Scenario = gsub("S", "Scenario~", Scenario)) %>%
  mutate(Clustering=ifelse(Clustering == "CAH", "Hierarchical clustering", Clustering)) %>%
  summarise(Indicator = mean(Pvalues < 0.05, na.rm = T))


plt_indicator <- ggplot(gaussian_results) +
  aes(x=Variable, y = Indicator, fill =FeatureSelection ) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Method",
                    values = c(pal2[1:2]), 
                    labels = c("Multimodality test",
                               "Selective test")) +
  geom_hline(aes(yintercept = 0.05, colour = "5% nominal levels"), linetype = "dotted", size = 1.5) +
  scale_colour_manual(name = "", values = "#FF7F11") +
  ggh4x::facet_nested(factor(Clustering, levels = c("Hierarchical clustering",
                                                       "k-means", 
                                                       "MClust", 
                                                       "Fuzzy c-means"))~Scenario+delta, labeller = labeller(.cols = label_parsed)) +
  ylab("Variable selection") +
  theme_light() +
  theme(text = element_text(size = 22),
        legend.position = "bottom", 
        axis.text.x = element_text(size = 12, angle = 65, hjust = 1),
        axis.text.y = element_text(size = 14),
        panel.grid = element_blank())



plt_ari <- do.call("rbind.data.frame", gaussian_results_read) %>%
  group_by(Variable, Scenario, FeatureSelection, Clustering, delta) %>%
  summarise(ARI = mean(ARI, na.rm = T)) %>%
  mutate(Scenario = gsub("S", "Scenario ", Scenario)) %>%
  mutate(Scenario = gsub("7", "2", Scenario)) %>%
  mutate(delta = paste0("delta==", delta)) %>%
  ggplot() + aes(x=delta, y=ARI, colour= factor(Clustering, levels = c("CAH",
                                                                       "k-means",
                                                                       "MClust", 
                                                                       "Fuzzy c-means"))) +
  geom_point(size = 4, alpha = .8) +
  geom_line(aes(group = factor(Clustering, levels = c("CAH",
                                                      "k-means",
                                                      "MClust", 
                                                      "Fuzzy c-means"))), size = 2, alpha = .8) +
  facet_wrap(~Scenario) +
  scale_colour_manual(name = "Clustering method", 
                      values = c(met.brewer("Nizami", n=4)[1:2], 
                                 met.brewer("Nizami", n=4)[4], 
                                 met.brewer("Nizami", n=4)[3]),
                      labels = c("Hierarchical clustering",
                                 "k-means",
                                 "MClust", 
                                 "Fuzzy c-means")) +
  theme_light() +
  xlab(TeX(r'($\delta$)')) +
  scale_x_discrete(labels = c("0", "2.5", "4", "8")) +
  guides(colour = guide_legend(override.aes = list(size = 2.5, linewidth = 1, alpha = 1))) +
  theme(text = element_text(size = 22),
         legend.position = "bottom", 
         axis.text.x = element_text(size = 18),
         axis.text.y = element_text(size = 18),
         panel.grid = element_blank())

plt_ari / plt_indicator + plot_layout(heights = c(2, 7)) +  
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face = 'bold'))


ggsave("figures/Figure4.pdf",
       width = 400, 
       height = 460, 
       units = "mm",
       dpi = 600)
