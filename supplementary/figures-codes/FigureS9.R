################################################################################
#            Supplementary Figure 5 associated to New Figure 4                 #
################################################################################

library(ggplot2)
library(MetBrewer)
library(dplyr)
library(latex2exp)
library(kableExtra)

pal <- wesanderson::wes_palette("Darjeeling1", 3, type = "continuous")
pal2 <- c(wesanderson::wes_palette("GrandBudapest2", 2, type = "continuous"), 
          "#cad3fa", pal[2])


#--------- Figure 4 : 2 Scenario under the Gaussian Setting -------------------#

results_files <-  list.files("supplementary/simulations-results/results_figureS9/", 
                             pattern = c(".csv"), full.names = TRUE)
length(results_files)
results_read <- lapply(results_files, readr::read_csv)

results <- do.call("rbind.data.frame", results_read) %>%
  group_by(Variable, Scenario, FeatureSelection, Clustering, delta) %>%
  mutate(delta = paste0("delta==", delta)) %>%
  mutate(Scenario = gsub("S", "Scenario~", Scenario)) %>%
  mutate(Scenario = ifelse(Scenario == "Scenario~1", paste0(Scenario, ": alpha == 1~nu == 100"), 
                           ifelse(Scenario == "Scenario~2", paste0(Scenario, ": alpha == 10~nu == 4"),
                                  ifelse(Scenario == "Scenario~3", paste0(Scenario, ": alpha == 10~nu == 100"), 
                                         paste0(Scenario, ": alpha == 1~nu == 4"))))) %>%
  mutate(Clustering=ifelse(Clustering == "CAH", "Hierarchical clustering", Clustering)) %>%
  summarise(Indicator = mean(Pvalues < 0.05, na.rm = T))


plt_indicator <- ggplot(results) +
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
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        panel.grid = element_blank())




ggsave(plt_indicator, 
       filename = "supplementary/figures/FigureS9.pdf",
       width = 350, 
       height = 350, 
       units = "mm", 
       dpi = 600)
  