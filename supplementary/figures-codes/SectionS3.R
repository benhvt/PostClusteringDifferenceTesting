################################################################################
#-------------------------- Supplementary Section S3 --------------------------#
#                                 31/01/2023                                   #
################################################################################
rm(list = ls())
library(ggplot2)
library(RColorBrewer)
library(wesanderson)
library(paletteer)
library(patchwork)
library(cowplot)
library(dplyr)
library(latex2exp)
library(ggpattern)
theme_set(theme_bw())
source(file = "utils.R")


################################################################################
#---------- Impact of the correlations between variables on Type I  -----------#
################################################################################

#-- Simulations parameters
nsimu <- 1000
alpha <- 0.05


#.1/ Impact of correlations and proportion of correlated variables on the type I

nbvar10_csv <- list.files("supplementary/simulations-results/results_sectionS3/p=10/", pattern = c("delta=0",".csv"), full.names = TRUE)
nbvar10 <- lapply(nbvar10_csv, readr::read_csv)
nbvar10_df <- dplyr::bind_rows(nbvar10)
nbvar10_df <- nbvar10_df[,-1]
nbvar10_df$Test <- rep(c("Merging test", "Multimodality test", "Selective test (K=2)", "Selective test (K=4)", "t-test"), each = 3*nsimu)
nbvar10_df$Correlation <- rep(c(rep("0.5", nsimu), rep("0", nsimu), rep("0.5", nsimu)), 5)
nbvar10_df$Prop_var_cor <- rep(c(rep("50%", nsimu), rep("0%", nsimu), rep("100%", nsimu)), 5)
FP_df <-nbvar10_df %>% group_by(Test, Correlation, Prop_var_cor) %>% 
  summarise(across(colnames(nbvar10_df)[1:10], function(x){mean(x<0.05, na.rm = T)})) 
FP_df2 <- data.frame("False Positive rate"=as.numeric(unlist(FP_df[,4:13, drop = T])),
                     Correlation = rep(paste("Cor(Xi,Xj)=",FP_df$Correlation, sep = ""), 10),
                     Prop_var_cor = rep(c("0% of correlated variables", "100% of correlated variables", "50% of correlated variables"), 5*10),
                     Test = rep(FP_df$Test, 10), check.names = F,
                     Variable = rep(paste("X", 1:10, sep = ""), each = 3*5))


plt2 <- FP_df2 %>% group_by(Prop_var_cor, Test) %>% summarise(Mean_FP = mean(`False Positive rate`)) %>%
  ggplot() + aes(x=Test, y = Mean_FP, fill = factor(Prop_var_cor, levels = paste(c("0%", "50%", "100%"), "of correlated variables"))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = TeX(r'($\kappa$)'),
                    values = NatParksPalettes::natparks.pals(name = "SmokyMtns", n=7)[c(3,6,4)],
                    labels = c(TeX(r'($\kappa=0\%$ of correlated variables)'), 
                               TeX(r'($\kappa=50\%$ of correlated variables)'),
                               TeX(r'($\kappa=100\%$ of correlated variables)'))) +
  geom_hline(aes(yintercept = 0.05, colour = "5% Nominal Level"), linetype = "dotted", linewidth = 1.3) +
  scale_colour_manual(name = "", values = NatParksPalettes::natparks.pals(name = "Volcanoes", n=6)[5]) +
  # scale_y_log10() +
  annotation_logticks(sides = "l") +
  xlab("Test") +
  ylab("Overall False Positive Rate") +
  theme(legend.position = "right", 
        legend.text.align = 0,
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust =1))

ggsave(plt2, filename = "supplementary/figures/FigureSectionS3.1.pdf",
       width = 300,
       height = 150,
       units = "mm",
       dpi = 600)


#2./ Impact of the numbers of variables (p=4, 10) on the Type 1 and Power

##A./ On the type I 
nbvar4_delta0_csv <- list.files("supplementary/simulations-results/results_sectionS3/p=4/", pattern = c("delta=0_prop_var_cor=0.csv"), full.names = TRUE)
nbvar10_delta0_csv <- list.files("supplementary/simulations-results/results_sectionS3/p=10/", pattern = c("delta=0_prop_var_cor=0.csv"), full.names = TRUE)
nbvar4_delta0 <- lapply(nbvar4_delta0_csv, readr::read_csv)
nbvar10_delta0 <- lapply(nbvar10_delta0_csv, readr::read_csv)

nbvar4_delta0_df <- dplyr::bind_rows(nbvar4_delta0)
nbvar10_delta0_df <- dplyr::bind_rows(nbvar10_delta0)


nbvar4_delta0_df <- nbvar4_delta0_df[,-1]
nbvar10_delta0_df <- nbvar10_delta0_df[,-1]


nbvar4_delta0_df$Test <- rep(c("Merging test", "Multimodality test", "Selective test (K=2)", "Selective test (K=4)", "t-test"), each = nsimu)
nbvar10_delta0_df$Test <- rep(c("Merging test", "Multimodality test", "Selective test (K=2)", "Selective test (K=4)", "t-test"), each = nsimu)
nbvar4_delta0_df$p <- paste0("p=",4) 
nbvar10_delta0_df$p <- paste0("p=",10) 


FP_nbvar4_df <-nbvar4_delta0_df %>% group_by(Test) %>% 
  summarise(across(colnames(nbvar4_delta0_df)[1:4], function(x){mean(x<0.05, na.rm = T)})) 

FP_nbvar4_df2 <- data.frame("False Positive rate"=as.numeric(unlist(FP_nbvar4_df[,2:5, drop = T])),
                            Test = rep(FP_nbvar4_df$Test, 4), check.names = F,
                            Variable = rep(paste("X", 1:4, sep = ""), each = 5), 
                            p=paste0("p=",4))

FP_nbvar10_df <-nbvar10_delta0_df %>% group_by(Test) %>% 
  summarise(across(colnames(nbvar10_delta0_df)[1:10], function(x){mean(x<0.05, na.rm = T)})) 
FP_nbvar10_df2 <- data.frame("False Positive rate"=as.numeric(unlist(FP_nbvar10_df[,2:11, drop = T])),
                             Test = rep(FP_nbvar10_df$Test, 10), check.names = F,
                             Variable = rep(paste("X", 1:10, sep = ""), each = 5), 
                             p=paste0("p=",10))

FP_df <- rbind(FP_nbvar4_df2, FP_nbvar10_df2)

plt2_p_FP <- FP_df %>% group_by(p, Test) %>% summarise(Mean_FP = mean(`False Positive rate`)) %>%
  ggplot() + aes(x=Test, y = Mean_FP, fill = factor(p, levels = c("p=4", "p=10"))) + 
  geom_bar(stat = "identity", position = "dodge",
           color = "grey40") +
  scale_fill_manual(name = "Number of variables",
                    values = colorRampPalette(c("#0066CC","#FFFFFF","#FF8C00"))(4)[c(2,3)])+
  geom_hline(aes(yintercept = 0.05, colour = "5% Nominal Level"), linetype = "dotted", linewidth = 1.3) +
  scale_colour_manual(name = "", values = NatParksPalettes::natparks.pals(name = "Volcanoes", n=6)[5]) +
  # scale_y_log10() +
  annotation_logticks(sides = "l") +
  xlab("Test") +
  ylab("Overall False Positive Rate") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust =1))


##B./ On the power

### --- d=5
nbvar4_delta5_csv <- list.files("supplementary/simulations-results/results_sectionS3/p=4/", pattern = c("delta=5_prop_var_cor=0.csv"), full.names = TRUE)
nbvar10_delta5_csv <- list.files("supplementary/simulations-results/results_sectionS3/p=10/", pattern = c("delta=5_prop_var_cor=0.csv"), full.names = TRUE)
nbvar4_delta5 <- lapply(nbvar4_delta5_csv, readr::read_csv)
nbvar10_delta5 <- lapply(nbvar10_delta5_csv, readr::read_csv)

nbvar4_delta5_df <- dplyr::bind_rows(nbvar4_delta5)
nbvar10_delta5_df <- dplyr::bind_rows(nbvar10_delta5)


nbvar4_delta5_df <- nbvar4_delta5_df[,-1]
nbvar10_delta5_df <- nbvar10_delta5_df[,-1]

nbvar4_delta5_df$df <- "ẟ=5"
nbvar10_delta5_df$df <- "ẟ=5"


nbvar4_delta5_df$p <- paste0("p=",4) 
nbvar10_delta5_df$p <- paste0("p=",10) 



nbvar4_delta5_df$Test <- rep(c("Merging test", "Multimodality test", "Selective test (K=2)", "Selective test (K=4)", "t-test"), each = nsimu)
nbvar10_delta5_df$Test <- rep(c("Merging test", "Multimodality test", "Selective test (K=2)", "Selective test (K=4)", "t-test"), each = nsimu)

power_nbvar4_delta5_df <-nbvar4_delta5_df %>% group_by(Test) %>% 
  summarise(across(colnames(nbvar4_delta5_df)[1:4], function(x){mean(x<0.05, na.rm = T)})) 

power_nbvar4_detla5_df2 <- data.frame("Power"=as.numeric(unlist(power_nbvar4_delta5_df[,2:5, drop = T])),
                                      Test = rep(power_nbvar4_delta5_df$Test, 4), check.names = F,
                                      Variable = rep(paste("X", 1:4, sep = ""), each = 5), 
                                      p=paste0("p=",4),
                                      d = paste0("ẟ=", 5))

power_nbvar10_delta5_df <-nbvar10_delta5_df %>% group_by(Test) %>% 
  summarise(across(colnames(nbvar10_delta5_df)[1:10], function(x){mean(x<0.05, na.rm = T)})) 
power_nbvar10_delta5_df2 <- data.frame("Power"=as.numeric(unlist(power_nbvar10_delta5_df[,2:11, drop = T])),
                                       Test = rep(power_nbvar10_delta5_df$Test, 10), check.names = F,
                                       Variable = rep(paste("X", 1:10, sep = ""), each = 5), 
                                       p=paste0("p=",10),
                                       d = paste0("ẟ=", 5))



### --- d=3
nbvar4_delta3_csv <- list.files("supplementary/simulations-results/results_sectionS3/p=4/", pattern = c("delta=3_prop_var_cor=0.csv"), full.names = TRUE)
nbvar10_delta3_csv <- list.files("supplementary/simulations-results/results_sectionS3/p=10/", pattern = c("delta=3_prop_var_cor=0.csv"), full.names = TRUE)
nbvar4_delta3 <- lapply(nbvar4_delta3_csv, readr::read_csv)
nbvar10_delta3 <- lapply(nbvar10_delta3_csv, readr::read_csv)

nbvar4_delta3_df <- dplyr::bind_rows(nbvar4_delta3)
nbvar10_delta3_df <- dplyr::bind_rows(nbvar10_delta3)


nbvar4_delta3_df <- nbvar4_delta3_df[,-1]
nbvar10_delta3_df <- nbvar10_delta3_df[,-1]


nbvar4_delta3_df$Test <- rep(c("Merging test", "Multimodality test", "Selective test (K=2)", "Selective test (K=4)", "t-test"), each = nsimu)
nbvar10_delta3_df$Test <- rep(c("Merging test", "Multimodality test", "Selective test (K=2)", "Selective test (K=4)", "t-test"), each = nsimu)

nbvar4_delta3_df$df <- "ẟ=3"
nbvar10_delta3_df$df <- "ẟ=3"
nbvar4_delta3_df$p <- paste0("p=",4) 
nbvar10_delta3_df$p <- paste0("p=",10) 


power_nbvar4_delta3_df <-nbvar4_delta3_df %>% group_by(Test) %>% 
  summarise(across(colnames(nbvar4_delta3_df)[1:4], function(x){mean(x<0.05, na.rm = T)})) 

power_nbvar4_delta3_df2 <- data.frame("Power"=as.numeric(unlist(power_nbvar4_delta3_df[,2:5, drop = T])),
                                      Test = rep(power_nbvar4_delta3_df$Test, 4), check.names = F,
                                      Variable = rep(paste("X", 1:4, sep = ""), each = 5), 
                                      p=paste0("p=",4),
                                      d = paste0("ẟ=", 3))

power_nbvar10_delta3_df <-nbvar10_delta3_df %>% group_by(Test) %>% 
  summarise(across(colnames(nbvar10_delta5_df)[1:10], function(x){mean(x<0.05, na.rm = T)})) 
power_nbvar10_delta3_df2 <- data.frame("Power"=as.numeric(unlist(power_nbvar10_delta3_df[,2:11, drop = T])),
                                       Test = rep(power_nbvar10_delta3_df$Test, 10), check.names = F,
                                       Variable = rep(paste("X", 1:10, sep = ""), each = 5), 
                                       p=paste0("p=",10),
                                       d = paste0("ẟ=", 3))

power_df <- rbind(power_nbvar4_delta3_df2, power_nbvar4_detla5_df2,
                  power_nbvar10_delta3_df2, power_nbvar10_delta5_df2)

# plt2_p_pow <- power_df %>% group_by(p, Test, d) %>% summarise(Mean_power = mean(`Power`)) %>%
#   ggplot() + aes(x=Test, y = Mean_power, fill = factor(p, levels = c("p=4", "p=10")), pattern = d) + 
#   geom_bar_pattern(position = position_dodge(preserve = "single"), stat="identity",
#                    color = "black", 
#                    pattern_fill = "grey40",
#                    pattern_color = "grey40",
#                    pattern_angle = 45,
#                    pattern_density = 0.1,
#                    pattern_spacing = 0.025,
#                    pattern_key_scale_factor = 0.6) +
#   scale_fill_manual(name = "Number of variables",
#                     values =  colorRampPalette(c("#0066CC","#FFFFFF","#FF8C00"))(4)[c(2,3)]) +
#   scale_pattern_manual(name = TeX(r'(Mean difference $\delta$)'),
#                        values = c(`ẟ=3` = "stripe", `ẟ=5` = "none"),
#                        labels = c(TeX(r'($\delta=3$)'), TeX(r'($\delta=5$)'))) +
#   geom_hline(aes(yintercept = 0.05, colour = "5% Nominal Level"), linetype = "dotted", linewidth = 1.3) +
#   scale_colour_manual(name = "", values = NatParksPalettes::natparks.pals(name = "Volcanoes", n=6)[5]) +
#   # scale_y_log10() +
#   annotation_logticks(sides = "l") +
#   xlab("Test") +
#   ylab("Overall Power") +
#   guides(pattern = guide_legend(override.aes = list(fill = "white")),
#          fill = guide_legend(override.aes = list(pattern = "none"))) +
#   theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust =1))
# 


plt2_p_pow <-power_df %>% group_by(p, Test, d) %>% summarise(Mean_power = mean(`Power`)) %>%
  mutate(d = factor(d, labels = c(TeX(r'($delta=3$)'), TeX(r'($\delta=5$)')))) %>%
  ggplot() + aes(x=Test, y = Mean_power, fill = factor(p, levels = c("p=4", "p=10"))) + 
  geom_bar(stat="identity", position = "dodge", color = "grey40") +
  scale_fill_manual(name = "Number of variables",
                    values =  colorRampPalette(c("#0066CC","#FFFFFF","#FF8C00"))(4)[c(2,3)]) +
  geom_hline(aes(yintercept = 0.05, colour = "5% Nominal Level"), linetype = "dotted", linewidth = 1.3) +
  scale_colour_manual(name = "", values = NatParksPalettes::natparks.pals(name = "Volcanoes", n=6)[5]) +
  # scale_y_log10() +
  annotation_logticks(sides = "l") +
  xlab("Test") +
  ylab("Overall Power") +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  theme_light() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust =1)) + 
  facet_wrap(~d, labeller = label_parsed)

plt.res_p <-(plt2_p_FP | plt2_p_pow) +
  plot_layout(guides = "collect") &
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = 'bold'),
        text = element_text(size = 15),
        legend.position = "right") 

ggsave(plt.res_p, filename = "supplementary/figures/FigureSectionS3.2_V2.pdf",
       width = 350,
       height = 125,
       units = "mm",
       dpi = 600)


   #3./ Impact of the numbers of variables on the computational time 

theme_set(theme_bw())
pal <- wes_palette("Darjeeling1", 3, type = "continuous")
pal2 <- c(wes_palette("GrandBudapest2", 2, type = "continuous"), "#cad3fa", pal[2])

p <- c(2:10, 25, 50)
nsimu <- 500

# Results over 500 simulations of the data 
time_dip_2cl <- read.csv(file = "supplementary/simulations-results/results_sectionS3/time/time_dip_2cl.csv")
time_dip_4cl <- read.csv(file = "supplementary/simulations-results/results_sectionS3/time/time_dip_4cl.csv")

time_SI_2cl <- read.csv(file = "supplementary/simulations-results/results_sectionS3/time/time_SI_2cl.csv")
time_SI_4cl <- read.csv(file = "supplementary/simulations-results/results_sectionS3/time/time_SI_4cl.csv")

time_merge_2cl <- read.csv(file = "supplementary/simulations-results/results_sectionS3/time/time_merge_2cl.csv")
time_merge_4cl <- read.csv(file = "supplementary/simulations-results/results_sectionS3/time/time_merge_4cl.csv")
time_merge_4cl$V11 <- 60*time_merge_4cl$V11 #Min -> sec


time_dip_2cl$Method <- time_dip_4cl$Method <- "Multimodality test"
time_SI_2cl$Method <- time_SI_4cl$Method <- "Selective inference (Direct)"
time_merge_2cl$Method <- time_merge_4cl$Method <- "Selective inference (Harmonic mean)"

time_dip_2cl$NbCluster <- time_SI_2cl$NbCluster <- time_merge_2cl$NbCluster <- "2 clusters"
time_dip_4cl$NbCluster <- time_SI_4cl$NbCluster <- time_merge_4cl$NbCluster <- "4 clusters"

p.df <- c()

for (i in 1:length(p)){
  p.df <- c(p.df, rep(p[i], 6*nsimu))
}
time_res <- rbind(time_dip_2cl,
                  time_SI_2cl,
                  time_merge_2cl,
                  time_dip_4cl,
                  time_SI_4cl,
                  time_merge_4cl)

time.df <- data.frame(Time = as.numeric(as.matrix(time_res[,1:11])),
                      Method = rep(time_res$Method, length(p)),
                      NbCluster = rep(time_res$NbCluster, length(p)), 
                      p = as.factor(rep(p.df, 6)))


p_mean_time <- time.df %>% group_by(Method, NbCluster, p) %>% 
  summarise(Mean_Time = mean(Time)) %>%
  mutate(p=as.numeric(as.character(p))) %>%
  ggplot() + 
  aes(x=p, y=Mean_Time, colour = Method, linetype = Method, shape = Method) + 
  geom_point(size = 2.5) + 
  geom_line(size = 1.2) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  annotation_logticks(sides = "lb") +
  scale_colour_manual(values = pal2, 
                      labels = c("Multimodality test",
                                 "Selective test (direct)", 
                                 "Merging selective test"))  +
  scale_linetype_manual(values = c("dotted", "solid", "dashed"),
                        labels = c("Multimodality test",
                                   "Selective test (direct)", 
                                   "Merging selective test")) +
  scale_shape_manual(name = "Method",
                     labels = c("Multimodality test",
                                "Selective test (direct)", 
                                "Merging selective test"),
                     values = c(15,16, 17)) +
  facet_wrap(~NbCluster) +
  xlab("Number of dimensions (p)
       log10 scale") +
  ylab("Mean time to perform one test (sec)
       log10 scale") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=14),
        legend.box="vertical") 

p_mean_time

ggsave(p_mean_time, filename = "supplementary/figures/FigureSectionS3.3.pdf", dpi = 600, width = 225, height = 125, units = "mm")

