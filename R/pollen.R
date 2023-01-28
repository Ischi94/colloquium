library(tidyverse)
library(here)
library(tidybayes)

# plotting configurations
source(here("R", "config_file.R"))

# read raw data
dat_pollen <- read_rds(here("data",
                            "raw",
                            "pollen_data.rds"))

# read rope data
dat_rope <- read_rds(here("data",
                          "pollen_rope.rds"))

# ecozone data
dat_eco <- read_rds(here("data", 
                         "ecozone_data.rds"))


# ratios ------------------------------------------------------------------


# calculate ratios
dat_ratios <- dat_pollen %>% 
  ungroup() %>% 
  mutate(long_term = as.character(long_term), 
         long_term = as.double(long_term),
         pal_int = case_when(
           short_term < 0 & long_term < 0 ~ "CC", 
           short_term > 0 & long_term < 0 ~ "WC",
           short_term < 0 & long_term > 0 ~ "CW", 
           short_term > 0 & long_term > 0 ~ "WW")) %>% 
  drop_na(pal_int) %>% 
  mutate(dist_ratio = if_else(.epred <=  median(.epred), 
                              "equil", 
                              "disturb")) %>% 
  group_by(short_term, long_term, model, pal_int, dist_ratio) %>% 
  count() %>% 
  pivot_wider(names_from = dist_ratio, values_from =  n) %>% 
  mutate(dist_ratio = disturb/equil, 
         model = as_factor(model)) %>% 
  ungroup()

# save ratio data
write_rds(dat_ratios, 
          here("data", 
               "pollen_data.rds"))

# extract credible intervals
dat_ratios_ci <- dat_ratios %>% 
  group_by(pal_int) %>% 
  median_qi(dist_ratio) %>% 
  mutate(across(where(is.double), round, 2),
         text_lab = str_c(dist_ratio,
                          " [",
                          .lower,
                          ", ",
                          .upper,
                          "]"), 
         pal_int = case_when(
           pal_int == "CC" ~ "Cooling-Cooling", 
           pal_int == "WC" ~ "Warming-Cooling",
           pal_int == "CW" ~ "Cooling-Warming", 
           pal_int == "WW" ~ "Warming-Warming")) %>% 
  select(pal_int, text_lab)


# plot turnover ratio 
plot_ratios <- dat_ratios %>%
  mutate(pal_int = case_when(
    short_term < 0 & long_term < 0 ~ "Cooling-Cooling", 
    short_term > 0 & long_term < 0 ~ "Warming-Cooling",
    short_term < 0 & long_term > 0 ~ "Cooling-Warming", 
    short_term > 0 & long_term > 0 ~ "Warming-Warming")) %>% 
  mutate(pal_int = factor(pal_int, 
                          levels = c("Cooling-Warming",
                                     "Warming-Warming",
                                     "Warming-Cooling",
                                     "Cooling-Cooling"))) %>% 
  ggplot(aes(dist_ratio, pal_int)) +
  geom_rect(aes(xmin = dat_rope$ci_low[4], 
                xmax = dat_rope$ci_high[4], 
                ymin = -Inf, ymax = Inf), 
            fill = "grey90", alpha = 0.1, colour = NA) +
  geom_vline(xintercept = 1, 
             colour = colour_yellow) +
  stat_pointinterval(show.legend = FALSE,
                     point_size = 4,
                     point_fill = "white",
                     shape = 21,
                     point_alpha = 1,
                     interval_alpha = 1,
                     interval_colour = "grey20") +
  scale_x_continuous(breaks = c(0.7, 1, 1.3), 
                     labels = c("0.7", "1", "1.3")) +
  scale_y_discrete(expand = c(rep(0.2, 4))) +
  labs(y = NULL, x = "Turnover Ratio") +
  theme(axis.text.y = element_blank(), 
        panel.grid.major = element_line(colour = "grey97"), 
        text = element_text(colour = "grey20", size = 15), 
        axis.text = element_text(colour = "grey40", size = 15), 
        legend.position = "none", 
        axis.ticks.y = element_blank())


# save plot
ggsave(plot_ratios, filename = here("figures",
                                    "ratios.png"), 
       width = image_width/5,
       height = image_height*1.4,
       dpi = image_dpi,
       units = image_units, 
       bg = "white", device = ragg::agg_png)



# percentage change -------------------------------------------------------

# data wrangling
dat_risk_change <- dat_ratios %>% 
  mutate(total = disturb + equil, 
         IE = disturb, 
         CE = equil) %>% 
  ungroup() %>% 
  mutate(left = IE / total,
         right = CE / total,
         rd = left - right) %>% 
  mutate(pal_int = case_when(
    short_term < 0 & long_term < 0 ~ "Cooling-Cooling", 
    short_term > 0 & long_term < 0 ~ "Warming-Cooling",
    short_term < 0 & long_term > 0 ~ "Cooling-Warming", 
    short_term > 0 & long_term > 0 ~ "Warming-Warming")) %>% 
  drop_na(pal_int) %>% 
  mutate(pal_int = factor(pal_int, 
                          levels = c("Cooling-Warming",
                                     "Warming-Warming",
                                     "Warming-Cooling",
                                     "Cooling-Cooling")))

dat_risk_labels <- dat_risk_change %>% 
  group_by(pal_int) %>% 
  summarise(median_change = median(rd)) %>% 
  mutate(median_change_chr = round(median_change * 100, 1) %>% as.character(), 
         median_change_chr = paste0(median_change_chr, "%"))


# visualise
plot_risk_change <- dat_risk_change %>%
  mutate(model = factor(model, 
                        levels = c("long_term100",
                                   "long_term250",
                                   "long_term500",
                                   "long_term1000"))) %>% 
  ggplot(aes(rd, pal_int )) +
  geom_rect(aes(xmin = dat_rope$ci_low[3],
                xmax = dat_rope$ci_high[3],
                ymin = -Inf, ymax = Inf),
            fill = "grey90", alpha = 0.1, colour = NA) +
  geom_vline(xintercept = 0,
             colour = colour_yellow) +
  stat_dots(aes(fill = model),
            layout = "swarm",
            scale = 0.8,
            slab_size = 0.3,
            alpha = 0.6,
            quantiles = 15,
            slab_shape = 21,
            slab_colour = "white",
            side = "bottom",
            stroke = 0.2) +
  stat_slab(alpha = 0.45,
            fill = colour_grey,
            show.legend = FALSE,
            scale = 1.2) +
  stat_pointinterval(show.legend = FALSE,
                     point_size = 4,
                     point_fill = "white",
                     shape = 21,
                     position = position_nudge(y = 0.01),
                     point_alpha = 1,
                     interval_alpha = 1,
                     interval_colour = "grey20") +
  geom_text(aes(median_change, pal_int, label = median_change_chr),
            data = dat_risk_labels,
            colour = "grey10",
            position = position_nudge(y = 0.175),
            size = 14/.pt) +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_grey(name = NULL,
                  end = 0.1, 
                  start = 0.8,
                  breaks = c("long_term100",
                             "long_term250",
                             "long_term500",
                             "long_term1000"),
                  labels = c("Long-Term 100",
                             "Long-Term 250",
                             "Long-Term 500",
                             "Long-Term 1000")) +
  scale_y_discrete(expand = c(rep(0.03, 4))) +
  labs(y = NULL,
       x = "Change in Turnover Risk") +
  guides(fill = guide_legend(override.aes = list(slab_size = 1,
                                                 alpha = 1),
                             nrow = 1, byrow = TRUE)) +
  coord_cartesian(clip = 'off', ylim = c(0.5, 4.8)) +
  theme(panel.grid.major = element_line(colour = "grey97"), 
        text = element_text(colour = "grey20", size = 15), 
        axis.text = element_text(colour = "grey40", size = 15), 
        legend.position = "none", 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())

# save plot
ggsave(plot_risk_change, filename = here("figures",
                                         "risk_change_3.png"), 
       width = image_width,
       height = image_height*1.4,
       dpi = image_dpi,
       units = image_units, 
       bg = "white", device = ragg::agg_png)



# ecozones ----------------------------------------------------------------


# visualize
plot_eco <- dat_eco %>% 
  drop_na(ecozone) %>% 
  ggplot(aes(dist_ratio, ecozone)) +
  geom_rect(aes(xmin = dat_rope$ci_low[4], 
                xmax = dat_rope$ci_high[4], 
                ymin = -Inf, ymax = Inf), 
            fill = "grey90", alpha = 0.1, colour = NA) +
  geom_vline(xintercept = 1, 
             colour = colour_yellow) +
  stat_pointinterval(aes(colour = pal_int),
                     position = position_dodge(width = -0.3),
                     show.legend = FALSE,
                     point_size = 2,
                     point_fill = "white",
                     shape = 21,
                     point_alpha = 1,
                     alpha = 0.7) +
  annotate(geom = "label", 
           x = 2.8, y = 3.6, label = "ROPE", 
           colour = colour_grey, 
           size = 14/.pt, 
           label.size = 0) +
  annotate(geom = "curve",
           x = 2.7, xend = 1.2,
           y = 3.2, yend = 1.3,
           curvature = -0.25,
           colour = "grey70",
           arrow = arrow(length = unit(.2,"cm"))) +
  labs(y = NULL, x = "Turnover Ratio") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_manual(values = c(colour_blue, colour_red), 
                     name = "Paleoclimate Interaction", 
                     labels = c("Cooling-Cooling", 
                                "Warming-Warming")) +
  scale_x_continuous(trans = scales::pseudo_log_trans(base = exp(1)), 
                     breaks = c(0:30), 
                     labels = c("0.1", as.character(1:8),
                                "", "10", 
                                rep("", 4), "15", 
                                rep("", 9), "25", 
                                rep("", 5))) +
  coord_cartesian(xlim = c(0, 25), clip = "off") +
  theme(panel.grid.major = element_line(colour = "grey97"), 
        text = element_text(colour = "grey20", size = 15), 
        axis.text = element_text(colour = "grey40", size = 15), 
        legend.position = "none")

# save plot
ggsave(plot_eco, filename = here("figures",
                                 "eco_2.png"), 
       width = image_width*1.4,
       height = image_height*1.4,
       dpi = image_dpi,
       units = image_units, 
       bg = "white", device = ragg::agg_png)
