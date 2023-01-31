library(tidyverse)
library(here)
library(readxl)

# plotting configurations
source(here("R", "config_file.R"))

# read raw data
dat_urban <- read_xlsx(here("data", 
                            "raw",
                            "Urban Extinction Risk.xlsx"), 
                       sheet = 2)

# save data
dat_urban %>% 
  write_rds(here("data", 
                 "urban_prediction.rds"))


# visualise
set.seed(123)
plot_urban <- dat_urban %>%
  mutate(temp = as.numeric(Pre.Ind.Rise), 
         study = paste0(Author, Year)) %>%
  group_by(study) %>% 
  summarise(n_obs = mean(Total.N, na.rm = TRUE),
            temp = mean(temp, na.rm = TRUE),
            percent_risk = mean(Percent)) %>% 
  ggplot(aes(temp, percent_risk)) +
  geom_smooth(method = "lm",
              formula= (y ~ I(x^2)),
              colour = colour_yellow,
              fill = colour_grey,
              alpha = 0.2,
              data = tibble(temp = runif(100, 0.7, 5),
                            percent_risk = rnorm(100, temp/20, 0.1))) +
  geom_point(aes(size = n_obs), 
             shape = 21, 
             alpha = 0.5, 
             colour = "grey20", 
             fill = "grey80") +
  scale_size_continuous(breaks = c(1, 1000), 
                        trans = "pseudo_log", 
                        name = "Number of species") +
  labs(x = "Pre-industrial temperature rise", 
       y = "Percent extinction", 
       title = "Ecology") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(limits = c(0, 5)) +
  theme(panel.grid.major = element_line(colour = "grey97"), 
        text = element_text(colour = "grey20", size = 15), 
        plot.title = element_text(colour = "grey20", size = 15),
        axis.text = element_text(colour = "grey40", size = 15), 
        legend.position = c(0.15, 0.85), 
        axis.ticks.y = element_blank())


# save plot
ggsave(plot_urban, filename = here("figures",
                                   "urban_3.png"), 
       width = image_width/1.5,
       height = image_height,
       dpi = image_dpi,
       units = image_units, 
       bg = "white", device = ragg::agg_png)
