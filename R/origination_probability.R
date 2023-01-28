library(tidyverse)
library(here)

# plotting configurations
source(here("R", "config_file.R"))

# load data called prob
load(here("data", 
          "origination_data.RData"))

# calculate average probability
av <- median(prob$ori.prob)

plot_ori <- ggplot(prob, aes(x = pal.int,
                 y = ori.prob,
                 fill = pal.int)) + 
  geom_hline(yintercept = av) +
  geom_violin() +
  scale_fill_manual(values = c(colour_blue, colour_blue, 
                               colour_red, colour_red))+
  scale_y_continuous(name = "Origination Probability", limits = c(0, 0.3), 
                     breaks = seq(0, 0.3, by = 0.05), 
                     labels = scales::percent_format(accuracy = 1)) +
  xlab(NULL) +
  # add half a violin to visualise palaeoclimate interactions
  ggnewscale::new_scale_fill() +
  see::geom_violinhalf(data = filter(prob, pal.int == "CW" | pal.int == "WC"),
                       aes(colour = pal.int, fill = pal.int)) +
  scale_fill_manual(values = c(colour_red, colour_blue)) +
  scale_colour_manual(values=c(colour_red, colour_blue)) +
  # add grey lines to visualise medians per group
  stat_summary(fun = median, fun.min = median, fun.max = median,
               geom = "crossbar", width = c(0.915, 0.35, 0.53, 0.42),
               colour = "grey55") +
  # add outer layer
  geom_violin(fill = NA) +
  # add arrows
  annotate(geom = "curve", x = 4, y = 0.22,  # overall
           xend = 4.45, yend = 0.138,
           curvature = -.325, arrow = arrow(length = unit(2.5, "mm")),
           colour = "grey40") +
  annotate(geom = "curve", x = 1.25, y = 0.055, # per group
           xend = 0.68, yend = 0.146,
           curvature = -.5, arrow = arrow(length = unit(2.5, "mm")),
           colour = "grey40") +
  # add lines
  annotate(geom = "segment", x = c(4, 1.25), y = c(0.22, 0.055), # overall
           xend = c(4.55, 0.575), yend = c(0.22, 0.055), colour = "grey40") +
  # add background box
  annotate(geom = "rect", xmin = 4, xmax = 4.5, # overall
           ymin = 0.235, ymax = 0.26, fill = "white") +
  # add text
  annotate(geom = "text", x = 4.275, y = c(0.25, 0.235), # overall
           colour = "grey30", label = c("Overall", "median"), size = 14/.pt) +
  annotate(geom = "text", x = 0.9, y = c(0.045, 0.03, 0.015), # per group
           colour = "grey30", label = c("Median", "per", "group"), size = 3.5) +
  scale_x_discrete(labels = c("Cooling-Cooling", "Cooling-Warming",
                              "Warming-Cooling", "Warming-Warming"),
                   guide = guide_axis(n.dodge = 2)) +
  theme(panel.grid.major = element_line(colour = "grey97"), 
        text = element_text(colour = "grey20", size = 15), 
        axis.text = element_text(colour = "grey40", size = 15), 
        legend.position = "none")
  
# save plot
ggsave(plot_ori, filename = here("figures",
                                 "ori_3.png"), 
       width = image_width*1.4,
       height = image_height*1.4,
       dpi = image_dpi,
       units = image_units, 
       bg = "white", device = ragg::agg_png)
