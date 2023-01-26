library(tidyverse)
library(here)
library(ggforce)

# plotting configurations
source(here("R", "config_file.R"))

# create data
set.seed(1708) # reproducibility

dat_palint <- tibble(time = 1:100,
                     pal_int = c(rep("Long-term Trend", 70),
                                 rep("Short-term Change", 30)),
                     temp = c(seq(0, 17, length.out = 70),
                              seq(17, 11, length.out = 30))) %>% 
  mutate(temp = temp + rnorm(length(temp), 
                             0, 1.4))  
  
# visualise
plot_palint <- dat_palint %>% 
  ggplot(aes(time, temp)) +
  geom_mark_ellipse(aes(colour = pal_int, label = pal_int),
                    label.fontsize = 15) +
  geom_point(shape = 21, 
             size = 3.5, 
             fill = colour_grey, 
             colour = "grey30") +
  scale_color_manual(values = c(colour_red, colour_blue)) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(y = "Temperature",
       x = "Time") +
  theme(legend.position = "none", 
        text = element_text(colour = "grey20", size = 15)) +
  coord_cartesian(xlim = c(-2, 100))


# save plot
ggsave(plot_palint, filename = here("figures",
                                    "palint_2.png"), 
       width = image_width*1.3,
       height = image_height*1.3,
       dpi = image_dpi,
       units = image_units, 
       bg = "white", device = ragg::agg_png)
