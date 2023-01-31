library(merTools)
library(tidyverse)
library(here)
library(lme4)

# plotting configurations
source(here("R", "config_file.R"))

# read effect size data
dat_cohens_d <- read_csv(here("data",
                          "cohens_d_effect.csv"))

# model as a mixed effect model
model_meta <- lmer(cohens_d ~ 1 + (1 | study),
                   weights = 1 / cohens_d_var, 
                   data = dat_cohens_d) 

# extract model estimates
dat_cohen_res <- predictInterval(model_meta, 
                                 newdata = dat_cohens_d %>% 
                                   distinct(study) %>% 
                                   as.data.frame(),
                                 which = "full", 
                                 include.resid.var = FALSE) %>% 
  as_tibble() %>% 
  add_column(study = dat_cohens_d %>% 
               distinct(study) %>% 
               pull()) %>% 
  select(study, mean_est = fit, lwr, upr)

# calculate overall effect
dat_cohen_ov <- FEsim(model_meta, 1e4) %>% 
  as_tibble() %>% 
  mutate(lwr = mean - 1.96*sd, 
         upr = mean + 1.96*sd) %>% 
  add_column(study = " ") %>% 
  select(study, mean_est = mean, lwr, upr) %>% 
  bind_rows(dat_cohen_res) %>% 
  # add scale of the focal climate legacy from spreadsheet
  add_column(scale = c(4000.5, 1, 1, 
                       8000, 10000000, 
                       1, 6000000))

# visualize as a forest plot
plot_meta <- dat_cohen_ov %>%
  mutate(col_lev = if_else(study == " ", 
                           "yes", "no")) %>% 
  ggplot() +
  geom_linerange(aes(xmin = lwr, 
                     xmax = upr, 
                     y = scale, 
                     colour = col_lev), 
                 position = position_nudge(y = c(0, -0.1, 0, 0, 
                                                 0, 0.1, 0)), 
                 alpha = 0.8) +
  geom_point(aes(mean_est, scale, 
                 fill = col_lev, 
                 colour = col_lev), 
             position = position_nudge(y = c(0, -0.1, 0, 0, 
                                             0, 0.1, 0)), 
             shape = 21, 
             size = 3) +
  annotate("text", x = 1.45, y = 1600,
           label = "Overall", size = 11/.pt,
           colour = "#de970bff") +
  annotate(geom = "curve",
           x = c(0, 0.51, 0.81, 1.21, 2.01),
           xend = c(0.49, 0.79, 1.19, 2, 3.3),
           y = 0.095, yend = 0.095,
           curvature = 0,
           arrow = arrow(length = unit(0.05, "inch"),
                         ends = c(rep("both", 4), "first")),
           colour = "grey40",
           linewidth = 0.3) +
  annotate("label", x = c(0.25, 0.65,
                          1, 1.58, 2.6), y = 0.2,
           label = c("small", "medium",
                     "large", "very large",
                     "huge"),
           size = 14/.pt, label.size = 0,
           label.padding = unit(0.1, "lines"),
           colour = "grey40") +
  labs(y = "Temporal scale of the climate legacy\nin years", 
       x = "Effect size expressed as Cohen's d") +
  coord_cartesian(xlim = c(0, 3.3)) +
  scale_color_manual(values = c("grey80", "#de970bff")) + 
  scale_fill_manual(values = c("white", "#de970bff")) +
  scale_y_continuous(trans = "log10", 
                     breaks = c(10e0, 10e2, 
                                10e4, 10e6), 
                     labels = c(expression("10e"^{"0"}), 
                                expression("10e"^{"2"}),
                                expression("10e"^{"4"}),
                                expression("10e"^{"6"}))) +
  theme(panel.grid.major = element_line(colour = "grey97"), 
        text = element_text(colour = "grey20", size = 15), 
        axis.text = element_text(colour = "grey40", size = 15), 
        legend.position = "none")

# save plot
ggsave(plot_meta, filename = here("figures",
                                 "meta_4.png"), 
       width = image_width*1.4,
       height = image_height*1.4,
       dpi = image_dpi,
       units = image_units, 
       bg = "white", device = ragg::agg_png)
