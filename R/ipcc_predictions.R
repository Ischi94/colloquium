library(tidyverse)
library(here)
library(colorspace)

# plotting configurations
source(here("R", "config_file.R"))

# load raw data
dat_ipcc <- tibble(filename = list.files(here("data",
                                              "raw",
                                              "ipcc")) %>%
                     str_remove("tas_global_") %>%
                     str_remove(".csv")) %>% 
  mutate(file_contents = map(list.files(here("data",
                                             "raw",
                                             "ipcc"),
                                        full.names = TRUE),  
                             ~ read_csv(.x))) %>% 
  unnest(file_contents)
  

# save that joined data
write_rds(dat_ipcc, here("data", 
                         "ipcc_data.rds"))


# create plot
plot_ipcc <- dat_ipcc %>% 
  mutate(filename = str_replace(filename, "_", "-"), 
         filename = str_replace(filename, "_", ".")) %>% 
  # filter(filename %in% c("Historical", "SSP5-8.5", 
  #                        "SSP3-7.0", "SSP2-4.5", 
  #                        "SSP1-2.6")) %>%
  ggplot(aes(Year,
             y = Mean)) +
  geom_ribbon(aes(ymin = `5%`, 
                  ymax = `95%`, 
                  fill = filename), 
              alpha = 0.2) +
  geom_line(aes(colour = filename)) +
  scale_color_manual(values = c(colour_grey, 
                                lighten(colour_blue, 0.1),
                                lighten(colour_blue, 0.1),
                                lighten(colour_red, 0.1),
                                lighten(colour_red, 0.1),
                                colour_red)) +
  scale_fill_manual(values = c(colour_grey, 
                               lighten(colour_blue, 0.1),
                               lighten(colour_blue, 0.1),
                               lighten(colour_red, 0.1),
                               lighten(colour_red, 0.1),
                               colour_red)) +
  scale_y_continuous(breaks = seq(0, 6, by = 2), 
                     labels = paste0(seq(0, 6, by = 2), "°C")) +
  coord_cartesian(ylim = c(-0.2, 6), 
                  xlim = c(1950, 2100)) +
  labs(y = NULL, 
       x = NULL, 
       title = "Global surface temperature change", 
       subtitle = "Increase relative to 1850-1900") +
  theme(legend.title = element_blank(), 
        legend.position = "none")

# save plot
ggsave(plot_ipcc, filename = here("figures",
                                  "ipcc_7.png"), 
       width = image_width/1.5,
       height = image_height,
       dpi = image_dpi,
       units = image_units, 
       bg = "white", device = ragg::agg_png)

