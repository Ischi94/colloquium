library(tidyverse)
library(here)
library(readxl)
library(scales)

# plotting configurations
source(here("R", "config_file.R"))

# read song data
dat_song <- read_xlsx(here("data",
                           "raw",
                           "song_2021.xlsx")) %>% 
  # clean up colnames
  select(bin = "Bin", 
         abs_delta = "ΔT (°C)", 
         abs_delta_sd = "SD-ΔT", 
         abs_r = "R (°C /Myr)", 
         abs_r_sd = "SD-R", 
         gf = "GF", 
         three_timer = "3T")

# save that cleaned data
write_rds(dat_song, here("data", 
                         "song_data.rds"))

# visualise 
plot_song <- dat_song %>%
  mutate(mass_ext = if_else(bin %in% c("P5", 
                                       "O5", 
                                       "T5", 
                                       "K8", 
                                       "D4"), 
                            "yes", "no")) %>% 
  ggplot(aes(y = abs_delta, 
             x = abs_r)) +
  geom_hline(yintercept = 6,
             colour = "grey30",
             linetype = "dotted") +
  geom_vline(xintercept = 10,
             colour = "grey30",
             linetype = "dotted") +
  geom_linerange(aes(xmin = abs_r - abs_r_sd, 
                     xmax = abs_r + abs_r_sd), 
                 colour = colour_grey) +
  geom_linerange(aes(ymin = abs_delta - abs_delta_sd, 
                      ymax = abs_delta + abs_delta_sd), 
                colour = colour_grey) +
  geom_point(aes(fill = mass_ext), 
             size = 4,
             shape = 21, 
             colour = "grey40") +
  scale_fill_manual(values = c("grey90", colour_yellow)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) {10^x}),
                labels = trans_format("log10", math_format(10^.x))) +
  coord_cartesian(xlim = c(0.1, 300), 
                  ylim = c(-2, 12)) +
  labs(x = "Rate of temperature change [°C/Myr]", 
       y = "Temperature change [°C]", 
       title = "Palaeontology") +
  theme(panel.grid.major = element_line(colour = "grey97"), 
        text = element_text(colour = "grey20", size = 15), 
        plot.title = element_text(colour = "grey20", size = 15),
        axis.text = element_text(colour = "grey40", size = 15), 
        legend.position = "none", 
        axis.ticks.y = element_blank())

# save plot
ggsave(plot_song, filename = here("figures",
                                  "song_5.png"), 
       width = image_width/1.5,
       height = image_height,
       dpi = image_dpi,
       units = image_units, 
       bg = "white", device = ragg::agg_png)


                     