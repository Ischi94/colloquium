library(tidyverse)
library(here)


# save plot
ggsave(plot_tax, filename = here("figures",
                                 "effect_taxonomy.png"), 
       width = image_width,
       height = image_height,
       dpi = image_dpi,
       units = image_units, 
       bg = "white", device = ragg::agg_png)