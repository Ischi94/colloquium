library(tidyverse)
library(here)
library(scales)

# plotting configurations
source(here("R", "config_file.R"))


# read extinctin risk data from my github repo
dat_ext <- read_csv(
  "https://raw.githubusercontent.com/Ischi94/pal-int-extinction/master/data/results/effect_intensity.csv"
  ) %>% 
  # add identifies
  rename(warm_1 = warm, warm_2 = warm_low, warm_3 = warm_high,
         cool_1 = cool, cool_2 = cool_low, cool_3 = cool_high) %>% 
  # reformat
  pivot_longer(cols = -taxon) %>% 
  mutate(name_id = str_extract(name, "\\d"), 
         pal_int = str_extract(name,"^[^_]+(?=_)")) %>% 
  select(-name) %>% 
  pivot_wider(names_from = name_id) %>% 
  rename(mean = "1",
         lower = "2", 
         upper = "3")
  

# same for the uncertainty interval/ null distribution
null_dist <- read_csv(
  "https://raw.githubusercontent.com/Ischi94/pal-int-extinction/master/data/results/simulation_results.csv"
) %>% 
  # summarise
  pivot_longer(-nr_observations) %>% 
  summarise(min_val = min(value), 
            max_val = max(value)) %>% 
  as.double()


# visualise
plot_ext <- dat_ext %>%
  mutate(is_sign = if_else(between(mean, null_dist[1], null_dist[2]), 
                           "non_sign", "sign")) %>% 
  ggplot(aes(y = fct_reorder(taxon, desc(taxon)))) +
  # add range of simulations/ null distribution
  geom_rect(xmin = null_dist[1], xmax = null_dist[2],
            ymin = 0, ymax = 8.5,
            alpha = 0.05,
            fill = "grey70") +
  # add reference line
  geom_vline(xintercept = 0, colour = colour_yellow) +
  # add 95% CI
  geom_errorbarh(aes(xmin = lower, xmax = upper, 
                     colour = pal_int, 
                     alpha = is_sign), 
                 height = 0.3, 
                 linewidth = 0.9) +
  # add dots for warm
  geom_point(aes(x = mean, 
                 fill = pal_int, 
                 alpha = is_sign), 
             shape = 21, 
             colour = "white", 
             size = 4, 
             stroke = 0.6) +
  # add arrow
  geom_segment(x = - 0.005, xend = - 0.065,
               y = 8.75, yend = 8.75,
               lineend = "round",
               linejoin = "round",
               linewidth = 0.3,
               arrow = arrow(length = unit(0.075, "inches")),
               colour = "grey40") +
  # and second arrow
  geom_segment(x = 0.005, xend = 0.065,
               y = 8.75, yend = 8.75,
               lineend = "round",
               linejoin = "round",
               linewidth = 0.3,
               arrow = arrow(length = unit(0.075, "inches")),
               colour = "grey40") +
  # add text for decrease
  annotate(geom = "text",
           label = "decreased risk",
           x = - 0.14,
           y = 8.75,
           size = 14/.pt,
           colour = "grey40") +
  # and for increase
  annotate(geom = "text",
           label = "increased risk",
           x = 0.14,
           y = 8.75,
           size = 14/.pt,
           colour = "grey40") +
  scale_alpha_discrete(range = c(0.3, 1)) +
  scale_colour_manual(values = c(colour_blue, colour_red)) +
  scale_fill_manual(values = c(colour_blue, colour_red)) +
  scale_x_continuous(limits = c(-0.2, 0.5), 
                     label = label_percent(suffix = "")) +
  scale_y_discrete(expand = expansion(mult = c(0.06, 0.14))) +
  labs(y = NULL, x = "Change in extinction risk [%]") +
  theme(panel.grid.major = element_line(colour = "grey97"), 
        text = element_text(colour = "grey20", size = 15), 
        axis.text = element_text(colour = "grey40", size = 15), 
        legend.position = "none")

# save plot
ggsave(plot_ext, filename = here("figures",
                                 "ext_6.png"), 
       width = image_width*1.4,
       height = image_height*1.4,
       dpi = image_dpi,
       units = image_units, 
       bg = "white", device = ragg::agg_png)
