# Libraries
library(tidyverse)
library(metafor)
library(ggpubr)
library(naniar) # to resolve NA/<NA> issue
library(orchaRd)
library(patchwork)
 
# Load meta-analysis results
meta_results_int <- read.csv("../data/CNPmeta_logr_results_int.csv") %>%
  mutate(myc_nas = ifelse(myc_assoc == "NM" | myc_assoc == "AM" |
                            myc_assoc == "NM-AM", 
                          "scavenging",
                          ifelse(myc_assoc == "EcM" | myc_assoc == "ErM" | 
                                   myc_assoc == "EcM-AM", "mining", NA)))

# Check data structure
head(meta_results_int)

# Load pft moderator results
pft_mods_int <- read.csv("../tables/CNPmeta_pft_moderators_int.csv")

# Check data structure
head(pft_mods_int)

##############################################################################
# Marea climate moderators
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "lma" & !is.na(gs_mat) & gs_ai < 3 & dNPi < 2) %>%
  ggplot(aes(x = gs_mat, y = dNPi)) +
  geom_point()

# Model
int_marea_clim <- rma.mv(yi = dNPi,
                         V = vNPi,
                         method = "REML", 
                         random = ~ 1 | exp, 
                         mods = ~ gs_mat + gs_ai + gs_par,
                         slab = exp, 
                         control = list(stepadj = 0.3), 
                         data = meta_results_int %>% 
                           filter(response == "lma" & 
                                    !is.na(gs_mat) & gs_ai < 3 & dNPi < 2))

# Summary
int_marea_clim_summary <- data.frame(trait = "marea",
                                     nut_add = "int",
                                     k = 78,
                                     mod = c("intrcpt", "gs_mat",
                                             "gs_ai", "gs_par"),
                                     coef(summary(int_marea_clim)),
                                     row.names = NULL)

# Temperature plot
int_marea_tg_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "lma" & 
                             !is.na(gs_mat) & gs_ai < 3 & dNPi < 2),
             aes(x = gs_mat, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(5, 27), breaks = seq(5, 25, 5)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bolditalic("M")[bold("area")]*bold(" - interaction resp. to ")*bolditalic("T")[bold("g")]),
       x = "",
       y = expression(bold("Int. effect size (")*bar(bolditalic("d")[bold("NP")])*bold(")")),
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_marea_tg_plot

# Aridity plot
int_marea_ai_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "lma" & 
                             !is.na(gs_mat) & gs_ai < 3 & dNPi < 2),
             aes(x = gs_ai, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bolditalic("M")[bold("area")]*bold(" - interaction resp. to ")*bolditalic("MI")[bold("g")]),
       x = "",
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_marea_ai_plot 

# PAR plot
int_marea_par_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "lma" & 
                             !is.na(gs_mat) & gs_ai < 3 & dNPi < 2),
             aes(x = gs_par, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(500, 1000), breaks = seq(500, 1000, 100)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bolditalic("M")[bold("area")]*bold(" - interaction resp. to ")*bolditalic("PAR")[bold("g")]),
       x = "",
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_marea_par_plot

##############################################################################
# Nmass climate moderators
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "leaf_n_mass" & !is.na(gs_mat) & gs_ai < 3 & dNPi > -1.9) %>%
  ggplot(aes(x = gs_mat, y = dNPi)) +
  geom_point()

# Model
int_nmass_clim <- rma.mv(yi = dNPi,
                         V = vNPi,
                         method = "REML", 
                         random = ~ 1 | exp, 
                         mods = ~ gs_mat + gs_ai + gs_par,
                         slab = exp, 
                         control = list(stepadj = 0.3), 
                         data = meta_results_int %>% 
                           filter(response == "leaf_n_mass" & 
                                    !is.na(gs_mat) & gs_ai < 3 & dNPi > -1.9))

# Summary
int_nmass_clim_summary <- data.frame(trait = "nmass",
                                     nut_add = "int",
                                     k = 101,
                                     mod = c("intrcpt", "gs_mat",
                                             "gs_ai", "gs_par"),
                                     coef(summary(int_nmass_clim)),
                                     row.names = NULL)

# Temperature plot
int_nmass_tg_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "leaf_n_mass" & 
                             !is.na(gs_mat) & gs_ai < 3 & dNPi > -1.9),
             aes(x = gs_mat, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(5, 27), breaks = seq(5, 25, 5)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bolditalic("N")[bold("mass")]*bold(" - interaction resp. to ")*bolditalic("T")[bold("g")]),
       x = "",
       y = expression(bold("Int. effect size (")*bar(bolditalic("d")[bold("NP")])*bold(")")),
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_nmass_tg_plot

# Aridity plot
int_nmass_ai_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "leaf_n_mass" & 
                             !is.na(gs_mat) & gs_ai < 3 & dNPi > -1.9),
             aes(x = gs_ai, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bolditalic("N")[bold("mass")]*bold(" - interaction resp. to ")*bolditalic("MI")[bold("g")]),
       x = "",
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_nmass_ai_plot 

# PAR plot
int_nmass_par_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "leaf_n_mass" & 
                             !is.na(gs_mat) & gs_ai < 3 & dNPi > -1.9),
             aes(x = gs_par, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(500, 1000), breaks = seq(500, 1000, 100)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bolditalic("N")[bold("mass")]*bold(" - interaction resp. to ")*bolditalic("PAR")[bold("g")]),
       x = "",
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_nmass_par_plot

##############################################################################
# Narea climate moderators
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "leaf_n_area" & !is.na(gs_mat) & gs_ai < 3) %>%
  ggplot(aes(x = gs_mat, y = dNPi)) +
  geom_point()

# Model
int_narea_clim <- rma.mv(yi = dNPi,
                         V = vNPi,
                         method = "REML", 
                         random = ~ 1 | exp, 
                         mods = ~ gs_mat + gs_ai + gs_par,
                         slab = exp, 
                         control = list(stepadj = 0.3), 
                         data = meta_results_int %>% 
                           filter(response == "leaf_n_area" & 
                                    !is.na(gs_mat) & gs_ai < 3))

# Summary
int_narea_clim_summary <- data.frame(trait = "narea",
                                     nut_add = "int",
                                     k = 50,
                                     mod = c("intrcpt", "gs_mat",
                                             "gs_ai", "gs_par"),
                                     coef(summary(int_narea_clim)),
                                     row.names = NULL)


# Temperature plot
int_narea_tg_plot <- mod_results(int_narea_clim, mod = "gs_mat",
                                 group = "exp", subset = TRUE)$mod_table %>%
  ggplot(aes(x = moderator, y = estimate)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "leaf_n_area" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_mat, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  geom_ribbon(aes(ymax = upperCL, ymin = lowerCL),
              alpha = 0.3) +
  geom_smooth(method = "loess", color = "black", linewidth = 2, 
              linetype = "dashed") +
  scale_x_continuous(limits = c(5, 27), breaks = seq(5, 25, 5)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bolditalic("N")[bold("area")]*bold(" - interaction resp. to ")*bolditalic("T")[bold("g")]),
       x = expression(bolditalic("T")[bold("g")]*bold(" ("*degree*"C)")),
       y = expression(bold("Int. effect size (")*bar(bolditalic("d")[bold("NP")])*bold(")")),
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_narea_tg_plot

# Aridity plot
int_narea_ai_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "leaf_n_area" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_ai, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bolditalic("N")[bold("area")]*bold(" - interaction resp. to ")*bolditalic("MI")[bold("g")]),
       x = expression(bolditalic("MI")[bold("g")]*bold(" (unitless)")),
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_narea_ai_plot 

# PAR plot
int_narea_par_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "leaf_n_area" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_par, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(500, 1000), breaks = seq(500, 1000, 100)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bolditalic("N")[bold("area")]*bold(" - interaction resp. to ")*bolditalic("PAR")[bold("g")]),
       x = expression(bolditalic("PAR")[bold("g")]*bold(" ("*mu*"mol"*" m"^"-2"*"s"^"-1"*")")),
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_narea_par_plot

##############################################################################
# Write figure S12
##############################################################################
png("../plots/supplement/CNP_figS12_leaf_clim_1.png", height = 14, width = 15,
    units = "in", res = 600)
ggarrange(int_marea_tg_plot, int_marea_ai_plot, int_marea_par_plot,
          int_nmass_tg_plot, int_nmass_ai_plot, int_nmass_par_plot,
          int_narea_tg_plot, int_narea_ai_plot, int_narea_par_plot,
          common.legend = TRUE, legend = "bottom",
          labels = c("(a)", "(b)", "(c)", 
                     "(d)", "(e)", "(f)", 
                     "(g)", "(h)", "(i)"),
          font.label = list(size = 22), align = "hv")
dev.off()

##############################################################################
# Pmass climate moderators
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "leaf_p_mass" & !is.na(gs_mat) & gs_ai < 3) %>%
  ggplot(aes(x = gs_mat, y = dNPi)) +
  geom_point()

# Model
int_pmass_clim <- rma.mv(yi = dNPi,
                         V = vNPi,
                         method = "REML", 
                         random = ~ 1 | exp, 
                         mods = ~ gs_mat + gs_ai + gs_par,
                         slab = exp, 
                         control = list(stepadj = 0.3), 
                         data = meta_results_int %>% 
                           filter(response == "leaf_p_mass" & 
                                    !is.na(gs_mat) & gs_ai < 3))

# Summary
int_pmass_clim_summary <- data.frame(trait = "pmass",
                                     nut_add = "int",
                                     k = 97,
                                     mod = c("intrcpt", "gs_mat",
                                             "gs_ai", "gs_par"),
                                     coef(summary(int_pmass_clim)),
                                     row.names = NULL)

# Temperature plot
int_pmass_tg_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "leaf_p_mass" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_mat, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(5, 27), breaks = seq(5, 25, 5)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bolditalic("P")[bold("mass")]*bold(" - interaction resp. to ")*bolditalic("T")[bold("g")]),
       x = "",
       y = expression(bold("Int. effect size (")*bar(bolditalic("d")[bold("NP")])*bold(")")),
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_pmass_tg_plot

# Aridity plot
int_pmass_ai_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "leaf_p_mass" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_ai, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bolditalic("P")[bold("mass")]*bold(" - interaction resp. to ")*bolditalic("MI")[bold("g")]),
       x = "",
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_pmass_ai_plot 

# PAR plot
int_pmass_par_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "leaf_p_mass" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_par, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(500, 1000), breaks = seq(500, 1000, 100)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bolditalic("P")[bold("mass")]*bold(" - interaction resp. to ")*bolditalic("PAR")[bold("g")]),
       x = "",
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_pmass_par_plot

##############################################################################
# Parea climate moderators
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "leaf_p_area" & !is.na(gs_mat) & gs_ai < 3 & dNPi < 2) %>%
  ggplot(aes(x = gs_mat, y = dNPi)) +
  geom_point()

# Model
int_parea_clim <- rma.mv(yi = dNPi,
                         V = vNPi,
                         method = "REML", 
                         random = ~ 1 | exp, 
                         mods = ~ gs_mat + gs_ai + gs_par,
                         slab = exp, 
                         control = list(stepadj = 0.3), 
                         data = meta_results_int %>% 
                           filter(response == "leaf_p_area" & 
                                    !is.na(gs_mat) & gs_ai < 3 & dNPi < 2))

# Summary
int_parea_clim_summary <- data.frame(trait = "parea",
                                     nut_add = "int",
                                     k = 44,
                                     mod = c("intrcpt", "gs_mat",
                                             "gs_ai", "gs_par"),
                                     coef(summary(int_parea_clim)),
                                     row.names = NULL)

# Temperature plot
int_parea_tg_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "leaf_p_area" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_mat, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(5, 27), breaks = seq(5, 25, 5)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bolditalic("P")[bold("area")]*bold(" - interaction resp. to ")*bolditalic("T")[bold("g")]),
       x = "",
       y = expression(bold("Int. effect size (")*bar(bolditalic("d")[bold("NP")])*bold(")")),
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_parea_tg_plot

# Aridity plot
int_parea_ai_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "leaf_p_area" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_ai, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bolditalic("P")[bold("area")]*bold(" - interaction resp. to ")*bolditalic("MI")[bold("g")]),
       x = "",
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_parea_ai_plot 

# PAR plot
int_parea_par_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "leaf_p_area" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_par, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(500, 1000), breaks = seq(500, 1000, 100)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bolditalic("P")[bold("area")]*bold(" - interaction resp. to ")*bolditalic("PAR")[bold("g")]),
       x = "",
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_parea_par_plot

##############################################################################
# Leaf N:P climate moderators
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "leaf_np" & !is.na(gs_mat) & gs_ai < 3) %>%
  ggplot(aes(x = gs_mat, y = dNPi)) +
  geom_point()

# Model
int_leafnp_clim <- rma.mv(yi = dNPi,
                          V = vNPi,
                          method = "REML", 
                          random = ~ 1 | exp, 
                          mods = ~ gs_mat + gs_ai + gs_par,
                          slab = exp, 
                          control = list(stepadj = 0.3), 
                          data = meta_results_int %>% 
                            filter(response == "leaf_np" & 
                                     !is.na(gs_mat) & gs_ai < 3 & dNPi > -2))

# Summary
int_leafnp_clim_summary <- data.frame(trait = "leaf_np",
                                      nut_add = "int",
                                      k = 82,
                                      mod = c("intrcpt", "gs_mat",
                                              "gs_ai", "gs_par"),
                                      coef(summary(int_leafnp_clim)),
                                      row.names = NULL)

# Temperature plot
int_leafnp_tg_plot <- mod_results(int_leafnp_clim, mod = "gs_mat",
                                 group = "exp", subset = TRUE)$mod_table %>%
  ggplot(aes(x = moderator, y = estimate)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "leaf_np" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_mat, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  geom_ribbon(aes(ymax = upperCL, ymin = lowerCL),
              alpha = 0.3) +
  geom_smooth(method = "loess", color = "black", linewidth = 2, 
              linetype = "dashed") +
  scale_x_continuous(limits = c(5, 27), breaks = seq(5, 25, 5)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("Leaf N:P - interaction resp. to ")*bolditalic("T")[bold("g")]),
       x = expression(bolditalic("T")[bold("g")]*bold(" ("*degree*"C)")),
       y = expression(bold("Int. effect size (")*bar(bolditalic("d")[bold("NP")])*bold(")")),
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_leafnp_tg_plot

# Aridity plot
int_leafnp_ai_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "leaf_np" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_ai, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("Leaf N:P - interaction resp. to ")*bolditalic("MI")[bold("g")]),
       x = expression(bolditalic("MI")[bold("g")]*bold(" (unitless)")),
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_leafnp_ai_plot 

# PAR plot
int_leafnp_par_plot <- mod_results(int_leafnp_clim, mod = "gs_par",
                                  group = "exp", subset = TRUE)$mod_table %>%
  ggplot(aes(x = moderator, y = estimate)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "leaf_np" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_par, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  geom_ribbon(aes(ymax = upperCL, ymin = lowerCL), alpha = 0.3) +
  geom_smooth(method = "loess", color = "black", linewidth = 2) +
  scale_x_continuous(limits = c(500, 1000), breaks = seq(500, 1000, 100)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("Leaf N:P - interaction resp. to ")*bolditalic("PAR")[bold("g")]),
       x = expression(bolditalic("PAR")[bold("g")]*bold(" ("*mu*"mol"*" m"^"-2"*"s"^"-1"*")")),
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_leafnp_par_plot


##############################################################################
# Write figure S13
##############################################################################
png("../plots/supplement/CNP_figS13_leaf_clim_2.png", height = 14, width = 15.5,
    units = "in", res = 600)
ggarrange(int_pmass_tg_plot, int_pmass_ai_plot, int_pmass_par_plot,
          int_parea_tg_plot, int_parea_ai_plot, int_parea_par_plot,
          int_leafnp_tg_plot, int_leafnp_ai_plot, int_leafnp_par_plot,
          common.legend = TRUE, legend = "bottom",
          labels = c("(a)", "(b)", "(c)", 
                     "(d)", "(e)", "(f)", 
                     "(g)", "(h)", "(i)"),
          font.label = list(size = 22), align = "hv")
dev.off()

##############################################################################
# Total biomass climate moderators
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "tbio_gm2" & !is.na(gs_mat) & gs_ai < 3 & dNPi < 1.25) %>%
  ggplot(aes(x = gs_mat, y = dNPi)) +
  geom_point()

# Model
int_tbio_clim <- rma.mv(yi = dNPi,
                        V = vNPi,
                        method = "REML", 
                        random = ~ 1 | exp, 
                        mods = ~ gs_mat + gs_ai + gs_par,
                        slab = exp, 
                        control = list(stepadj = 0.3), 
                        data = meta_results_int %>% 
                          filter(response == "tbio_gm2" & 
                                   !is.na(gs_mat) & gs_ai < 3 & dNPi < 1.25))

# Summary
int_tbio_clim_summary <- data.frame(trait = "tbio_gm2",
                                    nut_add = "int",
                                    k = 29,
                                    mod = c("intrcpt", "gs_mat",
                                            "gs_ai", "gs_par"),
                                    coef(summary(int_tbio_clim)),
                                    row.names = NULL)

# Temperature plot
int_tbio_tg_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "tbio_gm2" & 
                             !is.na(gs_mat) & gs_ai < 3 & dNPi < 1.25),
             aes(x = gs_mat, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(5, 27), breaks = seq(5, 25, 5)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("Biomass - interaction resp. to ")*bolditalic("T")[bold("g")]),
       x = "",
       y = expression(bold("Int. effect size (")*bar(bolditalic("d")[bold("NP")])*bold(")")),
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_tbio_tg_plot

# Aridity plot
int_tbio_ai_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "tbio_gm2" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_ai, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("Biomass - interaction resp. to ")*bolditalic("MI")[bold("g")]),
       x = "",
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_tbio_ai_plot 

# PAR plot
int_tbio_par_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "tbio_gm2" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_par, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(500, 1000), breaks = seq(500, 1000, 100)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("Biomass - interaction resp. to ")*bolditalic("PAR")[bold("g")]),
       x = "",
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_tbio_par_plot

##############################################################################
# Aboveground biomass climate moderators
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "anpp" & !is.na(gs_mat) & gs_ai < 3 & dNPi < 2 & dNPi > -1) %>%
  ggplot(aes(x = gs_mat, y = dNPi)) +
  geom_point()

# Model
int_anpp_clim <- rma.mv(yi = dNPi,
                        V = vNPi,
                        method = "REML", 
                        random = ~ 1 | exp, 
                        mods = ~ gs_mat + gs_ai + gs_par,
                        slab = exp, 
                        control = list(stepadj = 0.3), 
                        data = meta_results_int %>% 
                          filter(response == "anpp" & 
                                   !is.na(gs_mat) & gs_ai < 3 & dNPi < 2 & dNPi > -2))

# Summary
int_anpp_clim_summary <- data.frame(trait = "anpp",
                                    nut_add = "int",
                                    k = 104,
                                    mod = c("intrcpt", "gs_mat",
                                            "gs_ai", "gs_par"),
                                    coef(summary(int_anpp_clim)),
                                    row.names = NULL)

# Temperature plot
int_anpp_tg_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "anpp" & 
                             !is.na(gs_mat) & gs_ai < 3 & dNPi < 2 & dNPi > -2),
             aes(x = gs_mat, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(5, 27), breaks = seq(5, 25, 5)) +
  scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 1)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("ANPP - interaction resp. to ")*bolditalic("T")[bold("g")]),
       x = expression(bolditalic("T")[bold("g")]*bold(" ("*degree*"C)")),
       y = expression(bold("Int. effect size (")*bar(bolditalic("d")[bold("NP")])*bold(")")),
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_anpp_tg_plot

# Aridity plot
int_anpp_ai_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "anpp" & 
                             !is.na(gs_mat) & gs_ai < 3 & dNPi < 2 & dNPi > -2),
             aes(x = gs_ai, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
  scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 1)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("ANPP - interaction resp. to ")*bolditalic("MI")[bold("g")]),
       x = "",
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_anpp_ai_plot 

# PAR plot
int_anpp_par_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "anpp" & 
                             !is.na(gs_mat) & gs_ai < 3 & dNPi < 2 & dNPi > -2),
             aes(x = gs_par, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(500, 1000), breaks = seq(500, 1000, 100)) +
  scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 1)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("ANPP - interaction resp. to ")*bolditalic("PAR")[bold("g")]),
       x = "",
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_anpp_par_plot

##############################################################################
# Belowground biomass climate moderators
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "bnpp" & !is.na(gs_mat) & gs_ai < 3 & dNPi > -1) %>%
  ggplot(aes(x = gs_mat, y = dNPi)) +
  geom_point()

# Model
int_bnpp_clim <- rma.mv(yi = dNPi,
                        V = vNPi,
                        method = "REML", 
                        random = ~ 1 | exp, 
                        mods = ~ gs_mat + gs_ai + gs_par,
                        slab = exp, 
                        control = list(stepadj = 0.3), 
                        data = meta_results_int %>% 
                          filter(response == "bnpp" & 
                                   !is.na(gs_mat) & gs_ai < 3 & dNPi > -1))

# Summary
int_bnpp_clim_summary <- data.frame(trait = "bnpp",
                                    nut_add = "int",
                                    k = 51,
                                    mod = c("intrcpt", "gs_mat",
                                            "gs_ai", "gs_par"),
                                    coef(summary(int_bnpp_clim)),
                                    row.names = NULL)

# Temperature plot
int_bnpp_tg_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "bnpp" & 
                             !is.na(gs_mat) & gs_ai < 3  & dNPi > -1),
             aes(x = gs_mat, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(5, 27), breaks = seq(5, 25, 5)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("BNPP - interaction resp. to ")*bolditalic("T")[bold("g")]),
       x = expression(bolditalic("T")[bold("g")]*bold(" ("*degree*"C)")),
       y = expression(bold("Int. effect size (")*bar(bolditalic("d")[bold("NP")])*bold(")")),
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_bnpp_tg_plot

# Aridity plot
int_bnpp_ai_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "bnpp" & 
                             !is.na(gs_mat) & gs_ai < 3 & dNPi > -1),
             aes(x = gs_ai, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("BNPP - interaction resp. to ")*bolditalic("MI")[bold("g")]),
       x = expression(bolditalic("MI")[bold("g")]*bold(" (unitless)")),
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_bnpp_ai_plot 

# PAR plot
int_bnpp_par_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "bnpp" & 
                             !is.na(gs_mat) & gs_ai < 3  & dNPi > -1),
             aes(x = gs_par, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(500, 1000), breaks = seq(500, 1000, 100)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("BNPP - interaction resp. to ")*bolditalic("PAR")[bold("g")]),
       x = expression(bolditalic("PAR")[bold("g")]*bold(" ("*mu*"mol"*" m"^"-2"*"s"^"-1"*")")),
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_bnpp_par_plot

##############################################################################
# Write figure S13
##############################################################################
png("../plots/supplement/CNP_figS14_bio_clim1.png", height = 14, width = 16,
    units = "in", res = 600)
ggarrange(int_tbio_tg_plot, int_tbio_ai_plot, int_tbio_par_plot,
          int_anpp_tg_plot, int_anpp_ai_plot, int_anpp_par_plot,
          int_bnpp_tg_plot, int_bnpp_ai_plot, int_bnpp_par_plot,
          common.legend = TRUE, legend = "bottom",
          labels = c("(a)", "(b)", "(c)", 
                     "(d)", "(e)", "(f)", 
                     "(g)", "(h)", "(i)"),
          font.label = list(size = 22), align = "hv")
dev.off()

##############################################################################
# Root mass fraction climate moderators
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "rmf" & !is.na(gs_mat) & gs_ai < 3) %>%
  ggplot(aes(x = gs_mat, y = dNPi)) +
  geom_point()

# Model
int_rmf_clim <- rma.mv(yi = dNPi,
                       V = vNPi,
                       method = "REML", 
                       random = ~ 1 | exp, 
                       mods = ~ gs_mat + gs_ai + gs_par,
                       slab = exp, 
                       control = list(stepadj = 0.3), 
                       data = meta_results_int %>% 
                         filter(response == "rmf" & 
                                  !is.na(gs_mat) & gs_ai < 3))

# Summary
int_rmf_clim_summary <- data.frame(trait = "rmf",
                                   nut_add = "int",
                                   k = 34,
                                   mod = c("intrcpt", "gs_mat",
                                           "gs_ai", "gs_par"),
                                   coef(summary(int_rmf_clim)),
                                   row.names = NULL)

# Temperature plot
int_rmf_tg_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "rmf" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_mat, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(5, 27), breaks = seq(5, 25, 5)) +
  scale_y_continuous(limits = c(-1.4, 1.4), breaks = seq(-1.4, 1.4, 0.7)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("RMF - interaction resp. to ")*bolditalic("T")[bold("g")]),
       x = "",
       y = expression(bold("Int. effect size (")*bar(bolditalic("d")[bold("NP")])*bold(")")),
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_rmf_tg_plot

# Aridity plot
int_rmf_ai_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "rmf" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_ai, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
  scale_y_continuous(limits = c(-1.4, 1.4), breaks = seq(-1.4, 1.4, 0.7)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("RMF - interaction resp. to ")*bolditalic("MI")[bold("g")]),
       x = "",
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_rmf_ai_plot 

# PAR plot
int_rmf_par_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "rmf" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_par, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(500, 1000), breaks = seq(500, 1000, 100)) +
  scale_y_continuous(limits = c(-1.4, 1.4), breaks = seq(-1.4, 1.4, 0.7)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("RMF - interaction resp. to ")*bolditalic("PAR")[bold("g")]),
       x = "",
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_rmf_par_plot

##############################################################################
# Root:shoot climate moderators
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "rootshoot" & !is.na(gs_mat) & gs_ai < 3) %>%
  ggplot(aes(x = gs_mat, y = dNPi)) +
  geom_point()

# Model
int_rootshoot_clim <- rma.mv(yi = dNPi,
                             V = vNPi,
                             method = "REML", 
                             random = ~ 1 | exp, 
                             mods = ~ gs_mat + gs_ai + gs_par,
                             slab = exp, 
                             control = list(stepadj = 0.3), 
                             data = meta_results_int %>% 
                               filter(response == "rootshoot" & 
                                        !is.na(gs_mat) & gs_ai < 3))

# Summary
int_rootshoot_clim_summary <- data.frame(trait = "rootshoot",
                                         nut_add = "int",
                                         k = 36,
                                         mod = c("intrcpt", "gs_mat",
                                                 "gs_ai", "gs_par"),
                                         coef(summary(int_rootshoot_clim)),
                                         row.names = NULL)

# Temperature plot
int_rootshoot_tg_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "rootshoot" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_mat, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(5, 27), breaks = seq(5, 25, 5)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("R:S - interaction resp. to ")*bolditalic("T")[bold("g")]),
       x = expression(bolditalic("T")[bold("g")]*bold(" ("*degree*"C)")),
       y = expression(bold("Int. effect size (")*bar(bolditalic("d")[bold("NP")])*bold(")")),
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_rootshoot_tg_plot

# Aridity plot
int_rootshoot_ai_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "rootshoot" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_ai, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("R:S - interaction resp. to ")*bolditalic("MI")[bold("g")]),
       x = expression(bolditalic("MI")[bold("g")]*bold(" (unitless)")),
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_rootshoot_ai_plot 

# PAR plot
int_rootshoot_par_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_point(data = subset(meta_results_int, response == "rootshoot" & 
                             !is.na(gs_mat) & gs_ai < 3),
             aes(x = gs_par, y = dNPi, size = 1/dNPi_se), 
             alpha = 0.30) +
  scale_x_continuous(limits = c(500, 1000), breaks = seq(500, 1000, 100)) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  scale_size_continuous(limits = c(0, 30), range = c(1, 7)) +
  labs(title = expression(bold("R:S - interaction resp. to ")*bolditalic("PAR")[bold("g")]),
       x = expression(bolditalic("PAR")[bold("g")]*bold(" ("*mu*"mol"*" m"^"-2"*"s"^"-1"*")")),
       y = "",
       size = expression(bold("Error"^"-1"))) +
  guides(size = guide_legend(override.aes = list(fill = "grey", shape = 21))) +
  theme_classic(base_size = 20) +
  theme(title = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(color = "black", size = 20))
int_rootshoot_par_plot

##############################################################################
# Write figure S13
##############################################################################
png("../plots/supplement/CNP_figS15_bio_clim2.png", height = 10, width = 16,
    units = "in", res = 600)
ggarrange(int_rmf_tg_plot, int_rmf_ai_plot, int_rmf_par_plot,
          int_rootshoot_tg_plot, int_rootshoot_ai_plot, int_rootshoot_par_plot,
          common.legend = TRUE, legend = "bottom",
          labels = c("(a)", "(b)", "(c)", 
                     "(d)", "(e)", "(f)"),
          font.label = list(size = 22), align = "hv")
dev.off()


##############################################################################
# Merge climate moderators and write to .csv
##############################################################################
int_marea_clim_summary %>%
  full_join(int_nmass_clim_summary) %>%
  full_join(int_narea_clim_summary) %>%
  full_join(int_pmass_clim_summary) %>%
  full_join(int_parea_clim_summary) %>%
  full_join(int_leafnp_clim_summary) %>%
  full_join(int_tbio_clim_summary) %>%
  full_join(int_anpp_clim_summary) %>%
  full_join(int_bnpp_clim_summary) %>%
  full_join(int_rmf_clim_summary) %>%
  full_join(int_rootshoot_clim_summary) %>%
  mutate(across(estimate:ci.ub, ~round(.x, 3)),
         estimate_se = str_c(sprintf("%.3f", estimate), "±", sprintf("%.3f", se)),
         ci.range = str_c("[", sprintf("%.3f", ci.lb), ", ", sprintf("%.3f", ci.ub), "]")) %>%
  dplyr::select(trait:se, estimate_se, zval:ci.ub, ci.range) # %>%
  #write_excel_csv("../data/CNPmeta_clim_moderators_int.csv")

##############################################################################
# Marea photosynthetic pathway
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "lma" & !is.na(photo_path)) %>%
  ggplot(aes(x = photo_path, y = dNPi)) +
  geom_point()

# Model
int_marea_pft <- rma.mv(yi = dNPi,
                        V = vNPi,
                        method = "REML", 
                        random = ~ 1 | exp, 
                        mods = ~ photo_path + myc_nas + n_fixer,
                        slab = exp, 
                        control = list(stepadj = 0.3), 
                        data = meta_results_int %>% 
                          filter(response == "lma" & 
                                   !is.na(photo_path)))

# Extract photosynthetic pathway summary statistics
int_marea_photo <- data.frame(trait = "marea", 
                              nut_add = "int",
                              mod = "photo",
                              mod_results(int_marea_pft, 
                                          mod = "photo_path", 
                                          group = "exp")$mod_table,
                              z = coef(summary(int_marea_pft))[2,3],
                              p = coef(summary(int_marea_pft))[2, 4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract mycorrhizal acquisition strategy summary statistics
int_marea_myc <- data.frame(trait = "marea", 
                            nut_add = "int",
                            mod = "myc_nas",
                            mod_results(int_marea_pft, 
                                        mod = "myc_nas", 
                                        group = "exp")$mod_table,
                            z = coef(summary(int_marea_pft))[3,3],
                            p = coef(summary(int_marea_pft))[3,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract N-fixation ability summary statistics
int_marea_nfix <- data.frame(trait = "marea", 
                             nut_add = "int",
                             mod = "nfix",
                             mod_results(int_marea_pft, 
                                         mod = "n_fixer", 
                                         group = "exp")$mod_table,
                             z = coef(summary(int_marea_pft))[4,3],
                             p = coef(summary(int_marea_pft))[4,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

#############
# Merge summary statistics into single data frame
#############

int_marea_pft_results <- int_marea_photo %>% 
  rbind(int_marea_myc) %>% 
  rbind(int_marea_nfix) %>%
  mutate(k = 113,
         estimate = round(estimate, digits = 3),
         across(z:upperCL, ~ round(.x, digits = 3)),
         p = as.character(ifelse(p < 0.001, "<0.001", p))) %>%
  dplyr::select(trait:comp, k, estimate:upperCL)

##############################################################################
# Nmass
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "leaf_n_mass" & !is.na(photo_path)) %>%
  ggplot(aes(x = photo_path, y = dNPi)) +
  geom_point()

# Model
int_nmass_pft <- rma.mv(yi = dNPi,
                        V = vNPi,
                        method = "REML", 
                        random = ~ 1 | exp, 
                        mods = ~ photo_path + myc_nas + n_fixer,
                        slab = exp, 
                        control = list(stepadj = 0.3), 
                        data = meta_results_int %>% 
                          filter(response == "leaf_n_mass" & 
                                   !is.na(photo_path)))

# Extract photosynthetic pathway summary statistics
int_nmass_photo <- data.frame(trait = "nmass", 
                              nut_add = "int",
                              mod = "photo",
                              mod_results(int_nmass_pft, 
                                          mod = "photo_path", 
                                          group = "exp")$mod_table,
                              z = coef(summary(int_nmass_pft))[2,3],
                              p = coef(summary(int_nmass_pft))[2, 4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract mycorrhizal acquisition strategy summary statistics
int_nmass_myc <- data.frame(trait = "nmass", 
                            nut_add = "int",
                            mod = "myc_nas",
                            mod_results(int_nmass_pft, 
                                        mod = "myc_nas", 
                                        group = "exp")$mod_table,
                            z = coef(summary(int_nmass_pft))[3,3],
                            p = coef(summary(int_nmass_pft))[3,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract N-fixation ability summary statistics
int_nmass_nfix <- data.frame(trait = "nmass", 
                             nut_add = "int",
                             mod = "nfix",
                             mod_results(int_nmass_pft, 
                                         mod = "n_fixer", 
                                         group = "exp")$mod_table,
                             z = coef(summary(int_nmass_pft))[4,3],
                             p = coef(summary(int_nmass_pft))[4,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

#############
# Merge summary statistics into single data frame
#############

int_nmass_pft_results <- int_nmass_photo %>% 
  rbind(int_nmass_myc) %>% 
  rbind(int_nmass_nfix) %>%
  mutate(k = 136,
         estimate = round(estimate, digits = 3),
         across(z:upperCL, ~ round(.x, digits = 3)),
         p = as.character(ifelse(p < 0.001, "<0.001", p))) %>%
  dplyr::select(trait:comp, k, estimate:upperCL)


##############################################################################
# Narea - pft
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "leaf_n_area" & !is.na(photo_path)) %>%
  ggplot(aes(x = photo_path, y = dNPi)) +
  geom_point()

# Model
int_narea_pft <- rma.mv(yi = dNPi,
                        V = vNPi,
                        method = "REML", 
                        random = ~ 1 | exp, 
                        mods = ~ photo_path + myc_nas + n_fixer,
                        slab = exp, 
                        control = list(stepadj = 0.3), 
                        data = meta_results_int %>% 
                          filter(response == "leaf_n_area" & 
                                   !is.na(photo_path)))

# Extract photosynthetic pathway summary statistics
int_narea_photo <- data.frame(trait = "narea", 
                              nut_add = "int",
                              mod = "photo",
                              mod_results(int_narea_pft, 
                                          mod = "photo_path", 
                                          group = "exp")$mod_table,
                              z = coef(summary(int_narea_pft))[2,3],
                              p = coef(summary(int_narea_pft))[2, 4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract mycorrhizal acquisition strategy summary statistics
int_narea_myc <- data.frame(trait = "narea", 
                            nut_add = "int",
                            mod = "myc_nas",
                            mod_results(int_narea_pft, 
                                        mod = "myc_nas", 
                                        group = "exp")$mod_table,
                            z = coef(summary(int_narea_pft))[3,3],
                            p = coef(summary(int_narea_pft))[3,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract N-fixation ability summary statistics
int_narea_nfix <- data.frame(trait = "narea", 
                             nut_add = "int",
                             mod = "nfix",
                             mod_results(int_narea_pft, 
                                         mod = "n_fixer", 
                                         group = "exp")$mod_table,
                             z = coef(summary(int_narea_pft))[4,3],
                             p = coef(summary(int_narea_pft))[4,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

#############
# Merge summary statistics into single data frame
#############

int_narea_pft_results <- int_narea_photo %>% 
  rbind(int_narea_myc) %>% 
  rbind(int_narea_nfix) %>%
  mutate(k = 86,
         estimate = round(estimate, digits = 3),
         across(z:upperCL, ~ round(.x, digits = 3)),
         p = as.character(ifelse(p < 0.001, "<0.001", p))) %>%
  dplyr::select(trait:comp, k, estimate:upperCL)

##############################################################################
# Pmass - plant functional type
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "leaf_p_mass" & !is.na(photo_path)) %>%
  ggplot(aes(x = photo_path, y = dNPi)) +
  geom_point()

# Model
int_pmass_pft <- rma.mv(yi = dNPi,
                        V = vNPi,
                        method = "REML", 
                        random = ~ 1 | exp, 
                        mods = ~ photo_path + myc_nas + n_fixer,
                        slab = exp, 
                        control = list(stepadj = 0.3), 
                        data = meta_results_int %>% 
                          filter(response == "leaf_p_mass" & 
                                   !is.na(photo_path)))

# Extract photosynthetic pathway summary statistics
int_pmass_photo <- data.frame(trait = "pmass", 
                              nut_add = "int",
                              mod = "photo",
                              mod_results(int_pmass_pft, 
                                          mod = "photo_path", 
                                          group = "exp")$mod_table,
                              z = coef(summary(int_pmass_pft))[2,3],
                              p = coef(summary(int_pmass_pft))[2, 4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract mycorrhizal acquisition strategy summary statistics
int_pmass_myc <- data.frame(trait = "pmass", 
                            nut_add = "int",
                            mod = "myc_nas",
                            mod_results(int_pmass_pft, 
                                        mod = "myc_nas", 
                                        group = "exp")$mod_table,
                            z = coef(summary(int_pmass_pft))[3,3],
                            p = coef(summary(int_pmass_pft))[3,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract N-fixation ability summary statistics
int_pmass_nfix <- data.frame(trait = "pmass", 
                             nut_add = "int",
                             mod = "nfix",
                             mod_results(int_pmass_pft, 
                                         mod = "n_fixer", 
                                         group = "exp")$mod_table,
                             z = coef(summary(int_pmass_pft))[4,3],
                             p = coef(summary(int_pmass_pft))[4,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

#############
# Merge summary statistics into single data frame
#############

int_pmass_pft_results <- int_pmass_photo %>% 
  rbind(int_pmass_myc) %>% 
  rbind(int_pmass_nfix) %>%
  mutate(k = 128,
         estimate = round(estimate, digits = 3),
         across(z:upperCL, ~ round(.x, digits = 3)),
         p = as.character(ifelse(p < 0.001, "<0.001", p))) %>%
  dplyr::select(trait:comp, k, estimate:upperCL)

##############################################################################
# Parea - plant functional type
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "leaf_p_area" & !is.na(photo_path)) %>%
  ggplot(aes(x = photo_path, y = dNPi)) +
  geom_point()

# Model
int_parea_pft <- rma.mv(yi = dNPi,
                        V = vNPi,
                        method = "REML", 
                        random = ~ 1 | exp, 
                        mods = ~ photo_path + myc_nas + n_fixer,
                        slab = exp, 
                        control = list(stepadj = 0.3), 
                        data = meta_results_int %>% 
                          filter(response == "leaf_p_area" & 
                                   !is.na(photo_path)))

# Extract photosynthetic pathway summary statistics
int_parea_photo <- data.frame(trait = "parea", 
                              nut_add = "int",
                              mod = "photo",
                              mod_results(int_parea_pft, 
                                          mod = "photo_path", 
                                          group = "exp")$mod_table,
                              z = coef(summary(int_parea_pft))[2,3],
                              p = coef(summary(int_parea_pft))[2, 4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract mycorrhizal acquisition strategy summary statistics
int_parea_myc <- data.frame(trait = "parea", 
                            nut_add = "int",
                            mod = "myc_nas",
                            mod_results(int_parea_pft, 
                                        mod = "myc_nas", 
                                        group = "exp")$mod_table,
                            z = coef(summary(int_parea_pft))[3,3],
                            p = coef(summary(int_parea_pft))[3,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract N-fixation ability summary statistics
int_parea_nfix <- data.frame(trait = "parea", 
                             nut_add = "int",
                             mod = "nfix",
                             mod_results(int_parea_pft, 
                                         mod = "n_fixer", 
                                         group = "exp")$mod_table,
                             z = coef(summary(int_parea_pft))[4,3],
                             p = coef(summary(int_parea_pft))[4,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

#############
# Merge summary statistics into single data frame
#############

int_parea_pft_results <- int_parea_photo %>% 
  rbind(int_parea_myc) %>% 
  rbind(int_parea_nfix) %>%
  mutate(k = 79,
         estimate = round(estimate, digits = 3),
         across(z:upperCL, ~ round(.x, digits = 3)),
         p = as.character(ifelse(p < 0.001, "<0.001", p))) %>%
  dplyr::select(trait:comp, k, estimate:upperCL)

##############################################################################
# Leaf N:P - plant functional type
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "leaf_np" & !is.na(photo_path)) %>%
  ggplot(aes(x = photo_path, y = dNPi)) +
  geom_point()

# Model
int_leafnp_pft <- rma.mv(yi = dNPi,
                         V = vNPi,
                         method = "REML", 
                         random = ~ 1 | exp, 
                         mods = ~ photo_path + myc_nas + n_fixer,
                         slab = exp, 
                         control = list(stepadj = 0.3), 
                         data = meta_results_int %>% 
                           filter(response == "leaf_np" & 
                                    !is.na(photo_path)))

# Extract photosynthetic pathway summary statistics
int_leafnp_photo <- data.frame(trait = "leaf_np", 
                               nut_add = "int",
                               mod = "photo",
                               mod_results(int_leafnp_pft, 
                                           mod = "photo_path", 
                                           group = "exp")$mod_table,
                               z = coef(summary(int_leafnp_pft))[2,3],
                               p = coef(summary(int_leafnp_pft))[2, 4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract mycorrhizal acquisition strategy summary statistics
int_leafnp_myc <- data.frame(trait = "leaf_np", 
                             nut_add = "int",
                             mod = "myc_nas",
                             mod_results(int_leafnp_pft, 
                                         mod = "myc_nas", 
                                         group = "exp")$mod_table,
                             z = coef(summary(int_leafnp_pft))[3,3],
                             p = coef(summary(int_leafnp_pft))[3,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract N-fixation ability summary statistics
int_leafnp_nfix <- data.frame(trait = "leaf_np", 
                              nut_add = "int",
                              mod = "nfix",
                              mod_results(int_leafnp_pft, 
                                          mod = "n_fixer", 
                                          group = "exp")$mod_table,
                              z = coef(summary(int_leafnp_pft))[4,3],
                              p = coef(summary(int_leafnp_pft))[4,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

#############
# Merge summary statistics into single data frame
#############

int_leafnp_pft_results <- int_leafnp_photo %>% 
  rbind(int_leafnp_myc) %>% 
  rbind(int_leafnp_nfix) %>%
  mutate(k = 115,
         estimate = round(estimate, digits = 3),
         across(z:upperCL, ~ round(.x, digits = 3)),
         p = as.character(ifelse(p < 0.001, "<0.001", p))) %>%
  dplyr::select(trait:comp, k, estimate:upperCL)

##############################################################################
# Asat - plant functional type
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "asat" & !is.na(photo_path) & dNPi < 5) %>%
  ggplot(aes(x = photo_path, y = dNPi)) +
  geom_point()

# Model
int_asat_pft <- rma.mv(yi = dNPi,
                       V = vNPi,
                       method = "REML", 
                       random = ~ 1 | exp, 
                       mods = ~ photo_path + myc_nas + n_fixer,
                       slab = exp, 
                       control = list(stepadj = 0.3), 
                       data = meta_results_int %>% 
                         filter(response == "asat" & 
                                  !is.na(photo_path) & dNPi < 5))

# Extract photosynthetic pathway summary statistics
int_asat_photo <- data.frame(trait = "asat", 
                             nut_add = "int",
                             mod = "photo",
                             mod_results(int_asat_pft, 
                                         mod = "photo_path", 
                                         group = "exp")$mod_table,
                             z = coef(summary(int_asat_pft))[2,3],
                             p = coef(summary(int_asat_pft))[2, 4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract mycorrhizal acquisition strategy summary statistics
int_asat_myc <- data.frame(trait = "asat", 
                           nut_add = "int",
                           mod = "myc_nas",
                           mod_results(int_asat_pft, 
                                       mod = "myc_nas", 
                                       group = "exp")$mod_table,
                           z = coef(summary(int_asat_pft))[3,3],
                           p = coef(summary(int_asat_pft))[3,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract N-fixation ability summary statistics
int_asat_nfix <- data.frame(trait = "asat", 
                            nut_add = "int",
                            mod = "nfix",
                            mod_results(int_asat_pft, 
                                        mod = "n_fixer", 
                                        group = "exp")$mod_table,
                            z = coef(summary(int_asat_pft))[4,3],
                            p = coef(summary(int_asat_pft))[4,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

#############
# Merge summary statistics into single data frame
#############

int_asat_pft_results <- int_asat_photo %>% 
  rbind(int_asat_myc) %>% 
  rbind(int_asat_nfix) %>%
  mutate(k = 90,
         estimate = round(estimate, digits = 3),
         across(z:upperCL, ~ round(.x, digits = 3)),
         p = as.character(ifelse(p < 0.001, "<0.001", p))) %>%
  dplyr::select(trait:comp, k, estimate:upperCL)

##############################################################################
# gsw - plant functional type
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "gsw" & !is.na(photo_path)) %>%
  ggplot(aes(x = photo_path, y = dNPi)) +
  geom_point()

# Model
int_gsw_pft <- rma.mv(yi = dNPi,
                      V = vNPi,
                      method = "REML", 
                      random = ~ 1 | exp, 
                      mods = ~ photo_path + myc_nas + n_fixer,
                      slab = exp, 
                      control = list(stepadj = 0.3), 
                      data = meta_results_int %>% 
                        filter(response == "gsw" & 
                                 !is.na(photo_path)))

# Extract photosynthetic pathway summary statistics
int_gsw_photo <- data.frame(trait = "gsw", 
                            nut_add = "int",
                            mod = "photo",
                            mod_results(int_gsw_pft, 
                                        mod = "photo_path", 
                                        group = "exp")$mod_table,
                            z = coef(summary(int_gsw_pft))[2,3],
                            p = coef(summary(int_gsw_pft))[2, 4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract mycorrhizal acquisition strategy summary statistics
int_gsw_myc <- data.frame(trait = "gsw", 
                          nut_add = "int",
                          mod = "myc_nas",
                          mod_results(int_gsw_pft, 
                                      mod = "myc_nas", 
                                      group = "exp")$mod_table,
                          z = coef(summary(int_gsw_pft))[3,3],
                          p = coef(summary(int_gsw_pft))[3,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract N-fixation ability summary statistics
int_gsw_nfix <- data.frame(trait = "gsw", 
                           nut_add = "int",
                           mod = "nfix",
                           mod_results(int_gsw_pft, 
                                       mod = "n_fixer", 
                                       group = "exp")$mod_table,
                           z = coef(summary(int_gsw_pft))[4,3],
                           p = coef(summary(int_gsw_pft))[4,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

#############
# Merge summary statistics into single data frame
#############

int_gsw_pft_results <- int_gsw_photo %>% 
  rbind(int_gsw_myc) %>% 
  rbind(int_gsw_nfix) %>%
  mutate(k = 47,
         estimate = round(estimate, digits = 3),
         across(z:upperCL, ~ round(.x, digits = 3)),
         p = as.character(ifelse(p < 0.001, "<0.001", p))) %>%
  dplyr::select(trait:comp, k, estimate:upperCL)

##############################################################################
# Rd - plant functional type
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "rd" & !is.na(photo_path)) %>%
  ggplot(aes(x = photo_path, y = dNPi)) +
  geom_point()

# Model
int_rd_pft <- rma.mv(yi = dNPi,
                     V = vNPi,
                     method = "REML", 
                     random = ~ 1 | exp, 
                     mods = ~ photo_path + myc_nas + n_fixer,
                     slab = exp, 
                     control = list(stepadj = 0.3), 
                     data = meta_results_int %>% 
                       filter(response == "rd" & 
                                !is.na(photo_path)))

# Extract photosynthetic pathway summary statistics
int_rd_photo <- data.frame(trait = "rd", 
                           nut_add = "int",
                           mod = "photo",
                           mod_results(int_rd_pft, 
                                       mod = "photo_path", 
                                       group = "exp")$mod_table,
                           z = coef(summary(int_rd_pft))[2,3],
                           p = coef(summary(int_rd_pft))[2, 4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract mycorrhizal acquisition strategy summary statistics
int_rd_myc <- data.frame(trait = "rd", 
                         nut_add = "int",
                         mod = "myc_nas",
                         mod_results(int_rd_pft, 
                                     mod = "myc_nas", 
                                     group = "exp")$mod_table,
                         z = coef(summary(int_rd_pft))[3,3],
                         p = coef(summary(int_rd_pft))[3,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract N-fixation ability summary statistics
int_rd_nfix <- data.frame(trait = "rd", 
                          nut_add = "int",
                          mod = "nfix",
                          mod_results(int_rd_pft, 
                                      mod = "n_fixer", 
                                      group = "exp")$mod_table,
                          z = coef(summary(int_rd_pft))[4,3],
                          p = coef(summary(int_rd_pft))[4,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

#############
# Merge summary statistics into single data frame
#############

int_rd_pft_results <- int_rd_photo %>% 
  rbind(int_rd_myc) %>% 
  rbind(int_rd_nfix) %>%
  mutate(k = 32,
         estimate = round(estimate, digits = 3),
         across(z:upperCL, ~ round(.x, digits = 3)),
         p = as.character(ifelse(p < 0.001, "<0.001", p))) %>%
  dplyr::select(trait:comp, k, estimate:upperCL)

##############################################################################
# Vcmax - plant functional type
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "vcmax" & !is.na(photo_path)) %>%
  ggplot(aes(x = photo_path, y = dNPi)) +
  geom_point()

# Model
int_vcmax_pft <- rma.mv(yi = dNPi,
                        V = vNPi,
                        method = "REML", 
                        random = ~ 1 | exp, 
                        mods = ~ photo_path + myc_nas + n_fixer,
                        slab = exp, 
                        control = list(stepadj = 0.3), 
                        data = meta_results_int %>% 
                          filter(response == "vcmax" & 
                                   !is.na(photo_path)))

# Extract photosynthetic pathway summary statistics
int_vcmax_photo <- data.frame(trait = "vcmax", 
                              nut_add = "int",
                              mod = "photo",
                              mod_results(int_vcmax_pft, 
                                          mod = "photo_path", 
                                          group = "exp")$mod_table,
                              z = coef(summary(int_vcmax_pft))[2,3],
                              p = coef(summary(int_vcmax_pft))[2, 4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract mycorrhizal acquisition strategy summary statistics
int_vcmax_myc <- data.frame(trait = "vcmax", 
                            nut_add = "int",
                            mod = "myc_nas",
                            mod_results(int_vcmax_pft, 
                                        mod = "myc_nas", 
                                        group = "exp")$mod_table,
                            z = coef(summary(int_vcmax_pft))[3,3],
                            p = coef(summary(int_vcmax_pft))[3,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract N-fixation ability summary statistics
int_vcmax_nfix <- data.frame(trait = "vcmax", 
                             nut_add = "int",
                             mod = "nfix",
                             mod_results(int_vcmax_pft, 
                                         mod = "n_fixer", 
                                         group = "exp")$mod_table,
                             z = coef(summary(int_vcmax_pft))[4,3],
                             p = coef(summary(int_vcmax_pft))[4,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

#############
# Merge summary statistics into single data frame
#############

int_vcmax_pft_results <- int_vcmax_photo %>% 
  rbind(int_vcmax_myc) %>% 
  rbind(int_vcmax_nfix) %>%
  mutate(k = 42,
         estimate = round(estimate, digits = 3),
         across(z:upperCL, ~ round(.x, digits = 3)),
         p = as.character(ifelse(p < 0.001, "<0.001", p))) %>%
  dplyr::select(trait:comp, k, estimate:upperCL)

##############################################################################
# Jmax - plant functional type
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "jmax" & !is.na(photo_path)) %>%
  ggplot(aes(x = photo_path, y = dNPi)) +
  geom_point()

# Model
int_jmax_pft <- rma.mv(yi = dNPi,
                       V = vNPi,
                       method = "REML", 
                       random = ~ 1 | exp, 
                       mods = ~ photo_path + myc_nas + n_fixer,
                       slab = exp, 
                       control = list(stepadj = 0.3), 
                       data = meta_results_int %>% 
                         filter(response == "jmax" & 
                                  !is.na(photo_path)))

# Extract photosynthetic pathway summary statistics
int_jmax_photo <- data.frame(trait = "jmax", 
                             nut_add = "int",
                             mod = "photo",
                             mod_results(int_jmax_pft, 
                                         mod = "photo_path", 
                                         group = "exp")$mod_table,
                             z = coef(summary(int_jmax_pft))[2,3],
                             p = coef(summary(int_jmax_pft))[2, 4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract mycorrhizal acquisition strategy summary statistics
int_jmax_myc <- data.frame(trait = "jmax", 
                           nut_add = "int",
                           mod = "myc_nas",
                           mod_results(int_vcmax_pft, 
                                       mod = "myc_nas", 
                                       group = "exp")$mod_table,
                           z = coef(summary(int_jmax_pft))[3,3],
                           p = coef(summary(int_jmax_pft))[3,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract N-fixation ability summary statistics
int_jmax_nfix <- data.frame(trait = "jmax", 
                            nut_add = "int",
                            mod = "nfix",
                            mod_results(int_jmax_pft, 
                                        mod = "n_fixer", 
                                        group = "exp")$mod_table,
                            z = coef(summary(int_jmax_pft))[4,3],
                            p = coef(summary(int_jmax_pft))[4,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

#############
# Merge summary statistics into single data frame
#############

int_jmax_pft_results <- int_jmax_photo %>% 
  rbind(int_jmax_myc) %>% 
  rbind(int_jmax_nfix) %>%
  mutate(k = 39,
         estimate = round(estimate, digits = 3),
         across(z:upperCL, ~ round(.x, digits = 3)),
         p = as.character(ifelse(p < 0.001, "<0.001", p))) %>%
  dplyr::select(trait:comp, k, estimate:upperCL)

##############################################################################
# Jmax:Vcmax - plant functional type
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "jmax_vcmax" & !is.na(photo_path)) %>%
  ggplot(aes(x = photo_path, y = dNPi)) +
  geom_point()

# Model
int_jmaxvcmax_pft <- rma.mv(yi = dNPi,
                            V = vNPi,
                            method = "REML", 
                            random = ~ 1 | exp, 
                            mods = ~ photo_path + myc_nas + n_fixer,
                            slab = exp, 
                            control = list(stepadj = 0.3), 
                            data = meta_results_int %>% 
                              filter(response == "jmax_vcmax" & 
                                       !is.na(photo_path)))

# Extract photosynthetic pathway summary statistics
int_jmaxvcmax_photo <- data.frame(trait = "jmax_vcmax", 
                                  nut_add = "int",
                                  mod = "photo",
                                  mod_results(int_jmaxvcmax_pft, 
                                              mod = "photo_path", 
                                              group = "exp")$mod_table,
                                  z = coef(summary(int_jmaxvcmax_pft))[2,3],
                                  p = coef(summary(int_jmaxvcmax_pft))[2, 4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract mycorrhizal acquisition strategy summary statistics
int_jmaxvcmax_myc <- data.frame(trait = "jmax_vcmax", 
                                nut_add = "int",
                                mod = "myc_nas",
                                mod_results(int_jmaxvcmax_pft, 
                                            mod = "myc_nas", 
                                            group = "exp")$mod_table,
                                z = coef(summary(int_jmaxvcmax_pft))[3,3],
                                p = coef(summary(int_jmaxvcmax_pft))[3,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract N-fixation ability summary statistics
int_jmaxvcmax_nfix <- data.frame(trait = "jmax_vcmax", 
                                 nut_add = "int",
                                 mod = "nfix",
                                 mod_results(int_jmaxvcmax_pft, 
                                             mod = "n_fixer", 
                                             group = "exp")$mod_table,
                                 z = coef(summary(int_jmaxvcmax_pft))[4,3],
                                 p = coef(summary(int_jmaxvcmax_pft))[4,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

#############
# Merge summary statistics into single data frame
#############

int_jmaxvcmax_pft_results <- int_jmaxvcmax_photo %>% 
  rbind(int_jmaxvcmax_myc) %>% 
  rbind(int_jmaxvcmax_nfix) %>%
  mutate(k = 32,
         estimate = round(estimate, digits = 3),
         across(z:upperCL, ~ round(.x, digits = 3)),
         p = as.character(ifelse(p < 0.001, "<0.001", p))) %>%
  dplyr::select(trait:comp, k, estimate:upperCL)

##############################################################################
# PNUE - plant functional type
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "leaf_pnue" & !is.na(photo_path)) %>%
  ggplot(aes(x = photo_path, y = dNPi)) +
  geom_point()

# Model
int_pnue_pft <- rma.mv(yi = dNPi,
                       V = vNPi,
                       method = "REML", 
                       random = ~ 1 | exp, 
                       mods = ~ photo_path + myc_nas + n_fixer,
                       slab = exp, 
                       control = list(stepadj = 0.3), 
                       data = meta_results_int %>% 
                         filter(response == "leaf_pnue" & 
                                  !is.na(photo_path)))

# Extract photosynthetic pathway summary statistics
int_pnue_photo <- data.frame(trait = "pnue", 
                             nut_add = "int",
                             mod = "photo",
                             mod_results(int_pnue_pft, 
                                         mod = "photo_path", 
                                         group = "exp")$mod_table,
                             z = coef(summary(int_pnue_pft))[2,3],
                             p = coef(summary(int_pnue_pft))[2, 4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract mycorrhizal acquisition strategy summary statistics
int_pnue_myc <- data.frame(trait = "pnue", 
                           nut_add = "int",
                           mod = "myc_nas",
                           mod_results(int_pnue_pft, 
                                       mod = "myc_nas", 
                                       group = "exp")$mod_table,
                           z = coef(summary(int_pnue_pft))[3,3],
                           p = coef(summary(int_pnue_pft))[3,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract N-fixation ability summary statistics
int_pnue_nfix <- data.frame(trait = "pnue", 
                            nut_add = "int",
                            mod = "nfix",
                            mod_results(int_pnue_pft, 
                                        mod = "n_fixer", 
                                        group = "exp")$mod_table,
                            z = coef(summary(int_pnue_pft))[4,3],
                            p = coef(summary(int_pnue_pft))[4,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

#############
# Merge summary statistics into single data frame
#############

int_pnue_pft_results <- int_pnue_photo %>% 
  rbind(int_pnue_myc) %>% 
  rbind(int_pnue_nfix) %>%
  mutate(k = 65,
         estimate = round(estimate, digits = 3),
         across(z:upperCL, ~ round(.x, digits = 3)),
         p = as.character(ifelse(p < 0.001, "<0.001", p))) %>%
  dplyr::select(trait:comp, k, estimate:upperCL)

##############################################################################
# PPUE - plant functional type
##############################################################################

# Visualize responses
meta_results_int %>% 
  filter(response == "leaf_ppue" & !is.na(photo_path)) %>%
  ggplot(aes(x = photo_path, y = dNPi)) +
  geom_point()

# Model
int_ppue_pft <- rma.mv(yi = dNPi,
                       V = vNPi,
                       method = "REML", 
                       random = ~ 1 | exp, 
                       mods = ~ photo_path + myc_nas + n_fixer,
                       slab = exp, 
                       control = list(stepadj = 0.3), 
                       data = meta_results_int %>% 
                         filter(response == "leaf_ppue" & 
                                  !is.na(photo_path)))

# Extract photosynthetic pathway summary statistics
int_ppue_photo <- data.frame(trait = "ppue", 
                             nut_add = "int",
                             mod = "photo",
                             mod_results(int_ppue_pft, 
                                         mod = "photo_path", 
                                         group = "exp")$mod_table,
                             z = coef(summary(int_ppue_pft))[2,3],
                             p = coef(summary(int_ppue_pft))[2, 4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract mycorrhizal acquisition strategy summary statistics
int_ppue_myc <- data.frame(trait = "ppue", 
                           nut_add = "int",
                           mod = "myc_nas",
                           mod_results(int_ppue_pft, 
                                       mod = "myc_nas", 
                                       group = "exp")$mod_table,
                           z = coef(summary(int_ppue_pft))[3,3],
                           p = coef(summary(int_ppue_pft))[3,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

# Extract N-fixation ability summary statistics
int_ppue_nfix <- data.frame(trait = "ppue", 
                            nut_add = "int",
                            mod = "nfix",
                            mod_results(int_ppue_pft, 
                                        mod = "n_fixer", 
                                        group = "exp")$mod_table,
                            z = coef(summary(int_ppue_pft))[4,3],
                            p = coef(summary(int_ppue_pft))[4,4]) %>%
  dplyr::select(trait, nut_add, mod, comp = name, estimate, z, p, lowerCL, upperCL)

#############
# Merge summary statistics into single data frame
#############

int_ppue_pft_results <- int_ppue_photo %>% 
  rbind(int_ppue_myc) %>% 
  rbind(int_ppue_nfix) %>%
  mutate(k = 64,
         estimate = round(estimate, digits = 3),
         across(z:upperCL, ~ round(.x, digits = 3)),
         p = as.character(ifelse(p < 0.001, "<0.001", p))) %>%
  dplyr::select(trait:comp, k, estimate:upperCL)

##############################################################################
# Merge moderator results and write to .csv
##############################################################################
int_marea_pft_results %>%
  full_join(int_nmass_pft_results) %>%
  full_join(int_narea_pft_results) %>%
  full_join(int_pmass_pft_results) %>%
  full_join(int_parea_pft_results) %>%
  full_join(int_leafnp_pft_results) %>%
  full_join(int_asat_pft_results) %>%
  full_join(int_rd_pft_results) %>%
  full_join(int_gsw_pft_results) %>%
  full_join(int_vcmax_pft_results) %>%
  full_join(int_jmax_pft_results) %>%
  full_join(int_jmaxvcmax_pft_results) %>%
  full_join(int_pnue_pft_results) %>%
  full_join(int_ppue_pft_results) %>%
  mutate(across(estimate:upperCL, ~round(as.numeric(.x), 3)),
         ci.range = str_c("[", sprintf("%.3f", lowerCL), ", ", sprintf("%.3f", upperCL), "]")) # %>%
  # write.csv("../data/CNPmeta_pft_moderators_int.csv", row.names = F)

##############################################################################
# Figure S18: PFT moderator plots
##############################################################################

# Leaf chemical traits - N fixation plot
int_leaf_nfix_plot <- ggplot(data = pft_mods_int %>% 
                            filter(trait %in% c("marea", "nmass", "narea",
                                                "pmass", "parea", "leaf_np") & 
                                     mod == "nfix"),
                          aes(x = factor(trait, 
                                         levels = c("leaf_np", "parea", "pmass", 
                                                    "narea", "nmass", "marea")),  
                              y = estimate, 
                              shape = factor(comp, levels = c("Yes", "No")),
                              fill = factor(comp, levels = c("Yes", "No")))) +
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  geom_text(aes(x = 1, y = 1.85, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 2, y = 1.85, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 3, y = 1.85, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 4, y = 1.85, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 5, y = 1.85, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 6, y = 1.85, label = "NS"), size = 7, fontface = "bold") +
  geom_errorbar(aes(ymin = lowerCL, ymax = upperCL),
                position = position_dodge(width = 0.75),
                width = 0.2, color = "black", size = 1) +
  geom_point(position = position_dodge(width = 0.75), size = 5) +
  scale_shape_manual(values = c(21, 23),
                     labels = c(expression("N"["2"]*"-fixer"),
                                "non-fixer")) +
  scale_fill_manual(values = c("ivory", "black"),
                    labels = c(expression("N"["2"]*"-fixer"),
                               "non-fixer")) +
  scale_x_discrete(labels = c("Leaf N:P",
                              expression(italic("P")["area"]),
                              expression(italic("P")["mass"]),
                              expression(italic("N")["area"]),
                              expression(italic("N")["mass"]),
                              expression(italic("M")["area"]))) +
  scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 1)) +
  coord_flip() +
  labs(title = expression(bold("Leaf interaction resps. to N"["2"]*"-fixation ability")),
       x = "",
       y = "",
       shape = expression(bold("N-fixation\nability"))) +   
  theme_classic(base_size = 20) +
  theme(legend.title = element_text(face = "bold", size = 20),
        legend.text = element_text(size = 18),
        axis.title.x = element_text(face = "bold", size = 22),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.text.x = element_text(size = 20, color = "black"),
        panel.grid = element_blank(),
        title = element_text(face = "bold", size = 16)) +
  guides(fill = "none")
int_leaf_nfix_plot


# Leaf chemical traits - mycorrhizal plot
int_leaf_myc_plot <- ggplot(data = pft_mods_int %>% 
                           filter(trait %in% c("marea", "nmass", "narea",
                                               "pmass", "parea", "leaf_np") & 
                                    mod == "myc_nas"),
                         aes(x = factor(trait, 
                                        levels = c("leaf_np", "parea", "pmass", 
                                                   "narea", "nmass", "marea")),  
                             y = estimate, 
                             shape = factor(comp, levels = c("Scavenging", "Mining")),
                             fill = factor(comp, levels = c("Scavenging", "Mining")))) +
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  geom_text(aes(x = 1, y = 1.85, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 2, y = 1.85, label = "†"), size = 7, fontface = "bold") +
  geom_text(aes(x = 3, y = 1.85, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 4, y = 1.85, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 5, y = 1.85, label = "*"), size = 7, fontface = "bold") +
  geom_text(aes(x = 6, y = 1.85, label = "NS"), size = 7, fontface = "bold") +
  geom_errorbar(aes(ymin = lowerCL, ymax = upperCL),
                position = position_dodge(width = 0.75),
                width = 0.2, color = "black", size = 1) +
  geom_point(position = position_dodge(width = 0.75), size = 5) +
  scale_shape_manual(values = c(21, 24),
                     labels = c("scavenging", "mining")) +
  scale_fill_manual(values = c("ivory", "black"),
                    labels = c("scavenging", "mining")) +
  scale_x_discrete(labels = c("Leaf N:P",
                              expression(italic("P")["area"]),
                              expression(italic("P")["mass"]),
                              expression(italic("N")["area"]),
                              expression(italic("N")["mass"]),
                              expression(italic("M")["area"]))) +
  scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 1)) +
  coord_flip() +
  labs(title = "Leaf interaction resps. to myc. acq. strategy",
       x = "",
       y = "",
       shape = expression(bold("Mycorrhizal acq.\nstrategy"))) +   
  theme_classic(base_size = 20) +
  theme(legend.title = element_text(face = "bold", size = 20),
        legend.text = element_text(size = 18),
        axis.title.x = element_text(face = "bold", size = 22),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.text.x = element_text(size = 20, color = "black"),
        panel.grid = element_blank(),
        title = element_text(face = "bold", size = 16)) +
  guides(fill = "none")
int_leaf_myc_plot

# Leaf chemical traits - photosynthetic pathway plot
int_leaf_photo_plot <- ggplot(data = pft_mods_int %>% 
                              filter(trait %in% c("marea", "nmass", "narea",
                                                  "pmass", "parea", "leaf_np") & 
                                       mod == "photo"),
                            aes(x = factor(trait, 
                                           levels = c("leaf_np", "parea", "pmass", 
                                                      "narea", "nmass", "marea")), 
                                y = estimate, 
                                shape = factor(comp, levels = c("C3", "C4")),
                                fill = factor(comp, levels = c("C3", "C4")))) +
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  geom_errorbar(aes(ymin = lowerCL, ymax = upperCL),
                position = position_dodge(width = 0.75), width = 0.2, color = "black", size = 1) +
  geom_point(position = position_dodge(width = 0.75), size = 5) +
  geom_text(aes(x = 1, y = 1.85, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 2, y = 1.85, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 3, y = 1.85, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 4, y = 1.85, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 5, y = 1.85, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 6, y = 1.85, label = "NS"), size = 7, fontface = "bold") +
  scale_shape_manual(values = c(21, 22),
                     labels = c(expression("C"["3"]),
                                expression("C"["4"]))) +
  scale_fill_manual(values = c("ivory", "black"),
                    labels = c(expression("C"["3"]),
                               expression("C"["4"]))) +
  scale_x_discrete(labels = c("Leaf N:P",
                              expression(italic("P")["area"]),
                              expression(italic("P")["mass"]),
                              expression(italic("N")["area"]),
                              expression(italic("N")["mass"]),
                              expression(italic("M")["area"]))) +
  scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 1)) +
  coord_flip() +
  labs(title = "Leaf interaction resps. to photo. pathway",
       x = "",
       y = expression(bold("Interaction effect size (")*bar(bolditalic("d")[bold("NP")])*bold(")")),
       shape = expression(bold("Photo.\npathway"))) +   
  theme_classic(base_size = 20) +
  theme(legend.title = element_text(face = "bold", size = 20),
        legend.text = element_text(size = 18),
        axis.title.x = element_text(face = "bold", size = 22),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.text.x = element_text(size = 20, color = "black"),
        panel.grid = element_blank(),
        title = element_text(face = "bold", size = 16)) +
  guides(fill = "none")
int_leaf_photo_plot

# Photosynthetic traits - N fixation plot
int_photo_nfix_plot <- ggplot(data = pft_mods_int %>% 
                                filter(trait %in% c("ppue", "pnue", "jmax", 
                                                    "vcmax", "rd", "asat") & 
                                         mod == "nfix"),
                              aes(x = factor(trait, 
                                             levels = c("ppue", "pnue", "jmax", 
                                                        "vcmax", "rd", "asat")),  
                                 y = estimate, 
                                 shape = factor(comp, levels = c("Yes", "No")),
                                 fill = factor(comp, levels = c("Yes", "No")))) +
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  geom_text(aes(x = 1, y = 1.55, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 2, y = 1.55, label = "†"), size = 7, fontface = "bold") +
  geom_text(aes(x = 3, y = 1.55, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 4, y = 1.55, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 5, y = 1.55, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 6, y = 1.55, label = "NS"), size = 7, fontface = "bold") +
  geom_errorbar(aes(ymin = lowerCL, ymax = upperCL),
                position = position_dodge(width = 0.75),
                width = 0.2, color = "black", size = 1) +
  geom_point(position = position_dodge(width = 0.75), size = 5) +
  scale_shape_manual(values = c(21, 23),
                     labels = c(expression("N"["2"]*"-fixer"),
                                "non-fixer")) +
  scale_fill_manual(values = c("ivory", "black"),
                    labels = c(expression("N"["2"]*"-fixer"),
                               "non-fixer")) +
  scale_x_discrete(labels = c(expression(italic("PPUE")),
                              expression(italic("PNUE")),
                              expression(italic("J")["max"]),
                              expression(italic("V")["cmax"]),
                              expression(italic("R")["d"]),
                              expression(italic("A")["sat"]))) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  coord_flip() +
  labs(title = expression(bold("Photo. interaction resps. to N"["2"]*"-fixation ability")),
       x = "",
       y = "",
       shape = expression(bold("N-fixation\nability"))) +   
  theme_classic(base_size = 20) +
  theme(legend.title = element_text(face = "bold", size = 20),
        legend.text = element_text(size = 18),
        axis.title.x = element_text(face = "bold", size = 22),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.text.x = element_text(size = 20, color = "black"),
        panel.grid = element_blank(),
        title = element_text(face = "bold", size = 16)) +
  guides(fill = "none")
int_photo_nfix_plot


# Photosynthetic traits - mycorrhizal plot
int_photo_myc_plot <- ggplot(data = pft_mods_int %>% 
                               filter(trait %in% c("ppue", "pnue", "jmax", 
                                                   "vcmax", "rd", "asat") & 
                                        mod == "myc_nas"),
                             aes(x = factor(trait, 
                                            levels = c("ppue", "pnue", "jmax", 
                                                       "vcmax", "rd", "asat")),  
                                y = estimate, 
                                shape = factor(comp, levels = c("Scavenging", "Mining")),
                                fill = factor(comp, levels = c("Scavenging", "Mining")))) +
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  geom_text(aes(x = 1, y = 1.55, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 2, y = 1.55, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 3, y = 1.55, label = "*"), size = 7, fontface = "bold") +
  geom_text(aes(x = 4, y = 1.55, label = "*"), size = 7, fontface = "bold") +
  geom_text(aes(x = 5, y = 1.55, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 6, y = 1.55, label = "NS"), size = 7, fontface = "bold") +
  geom_errorbar(aes(ymin = lowerCL, ymax = upperCL),
                position = position_dodge(width = 0.75),
                width = 0.2, color = "black", size = 1) +
  geom_point(position = position_dodge(width = 0.75), size = 5) +
  scale_shape_manual(values = c(21, 24),
                     labels = c("scavenging", "mining")) +
  scale_fill_manual(values = c("ivory", "black"),
                    labels = c("scavenging", "mining")) +
  scale_x_discrete(labels = c(expression(italic("PPUE")),
                              expression(italic("PNUE")),
                              expression(italic("J")["max"]),
                              expression(italic("V")["cmax"]),
                              expression(italic("R")["d"]),
                              expression(italic("A")["sat"]))) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  coord_flip() +
  labs(title = "Photo. interaction resps. to myc. acq. strategy",
       x = "",
       y = "",
       shape = expression(bold("Mycorrhizal acq.\nstrategy"))) +   
  theme_classic(base_size = 20) +
  theme(legend.title = element_text(face = "bold", size = 20),
        legend.text = element_text(size = 18),
        axis.title.x = element_text(face = "bold", size = 22),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.text.x = element_text(size = 20, color = "black"),
        panel.grid = element_blank(),
        title = element_text(face = "bold", size = 16)) +
  guides(fill = "none")
int_photo_myc_plot

# Photosynthetic traits - photosynthetic pathway plot
int_photo_photo_plot <- ggplot(data = pft_mods_int %>% 
                                filter(trait %in% c("ppue", "pnue", "jmax", 
                                                    "vcmax", "rd", "asat") & 
                                         mod == "photo"),
                              aes(x = factor(trait, 
                                             levels = c("ppue", "pnue", "jmax", 
                                                        "vcmax", "rd", "asat")),  
                                  y = estimate, 
                                  shape = factor(comp, levels = c("C3", "C4")),
                                  fill = factor(comp, levels = c("C3", "C4")))) +
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  geom_errorbar(aes(ymin = lowerCL, ymax = upperCL),
                position = position_dodge(width = 0.75), width = 0.2, color = "black", size = 1) +
  geom_point(position = position_dodge(width = 0.75), size = 5) +
  geom_text(aes(x = 1, y = 1.55, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 2, y = 1.55, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 3, y = 1.55, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 4, y = 1.55, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 5, y = 1.55, label = "NS"), size = 7, fontface = "bold") +
  geom_text(aes(x = 6, y = 1.55, label = "NS"), size = 7, fontface = "bold") +
  scale_shape_manual(values = c(21, 22),
                     labels = c(expression("C"["3"]),
                                expression("C"["4"]))) +
  scale_fill_manual(values = c("ivory", "black"),
                    labels = c(expression("C"["3"]),
                               expression("C"["4"]))) +
  scale_x_discrete(labels = c(expression(italic("PPUE")),
                              expression(italic("PNUE")),
                              expression(italic("J")["max"]),
                              expression(italic("V")["cmax"]),
                              expression(italic("R")["d"]),
                              expression(italic("A")["sat"]))) +
  scale_y_continuous(limits = c(-1.6, 1.6), breaks = seq(-1.6, 1.6, 0.8)) +
  coord_flip() +
  labs(title = "Photo. interaction resps. to photo. pathway",
       x = "",
       y = expression(bold("Interaction effect size (")*bar(bolditalic("d")[bold("NP")])*bold(")")),
       shape = expression(bold("Photo.\npathway"))) +   
  theme_classic(base_size = 20) +
  theme(legend.title = element_text(face = "bold", size = 20),
        legend.text = element_text(size = 18),
        axis.title.x = element_text(face = "bold", size = 22),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.text.x = element_text(size = 20, color = "black"),
        panel.grid = element_blank(),
        title = element_text(face = "bold", size = 16)) +
  guides(fill = "none")
int_photo_photo_plot


#####################
# Write plot
#####################
png("../plots/supplement/CNP_figS18_pft_int_responses.png", width = 18,
    height = 18, units = "in", res = 600)
(int_leaf_nfix_plot | int_photo_nfix_plot) /
  (int_leaf_myc_plot | int_photo_myc_plot) /
  (int_leaf_photo_plot | int_photo_photo_plot) +
  plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(legend.spacing.y = unit(12, "cm"))
dev.off()



