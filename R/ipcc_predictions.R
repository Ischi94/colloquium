library(tidyverse)
library(here)


# load raw data
dat_ipcc <- tibble(filename = list.files(here("data",
                                              "raw",
                                              "ipcc")) %>%
                     str_remove("tas_global_") %>%
                     str_remove(".csv")) %>% 
  mutate(file_contents = map(filename,  
                             ~ read_csv(list.files(here("data",
                                                        "raw",
                                                        "ipcc"),
                                                   full.names = TRUE)))) %>% 
  unnest(file_contents)
  

# save that joined data
write_rds(dat_ipcc, here("data", 
                         "ipcc_data.rds"))

list.files(here("data", 
                "raw", 
                "ipcc"), 
           full.names = TRUE) %>% 
  map(read_csv)

# save plot
ggsave(plot_tax, filename = here("figures",
                                 "effect_taxonomy.png"), 
       width = image_width,
       height = image_height,
       dpi = image_dpi,
       units = image_units, 
       bg = "white", device = ragg::agg_png)