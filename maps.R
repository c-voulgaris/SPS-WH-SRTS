library(tidyverse)
library(rosm)
library(sf)
library(here)
library(ggspatial)

boundary <- here("CityLimits") |>
  st_read()

points <- tibble(name = c("Edgerly\nEducation Center",
                          "Capuano Early\nChildhood Center",
                          "Winter Hill\nSchool",
                          "Benjamin G.\nBrown School"),
                 lat = c(42.38761605023747,
                         42.38280854024114,
                         42.39183545449675,
                         42.39741417782396),
                 lon = c(-71.0872668997065,
                         -71.08717793614842,
                         -71.09879324716499, 
                         -71.11420580271012),
                 hJust = c(0,
                           0,
                           1.1,
                           1.1),
                 vJust = c(-0.25, 
                           1.1,
                           -0.25,
                           -0.25)) |>
  st_as_sf(coords = c("lon", "lat"),
           crs = "WGS84")

ggplot(boundary) +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(fill = NA, color = "red",
          linetype = "dashed",
          linewidth = 0.5) +
  geom_sf(data = points, color = "red") +
  geom_sf_text(data = points, 
               fontface = "bold",
               color = "red",
               aes(label = name,
                   hjust = hJust,
                   vjust = vJust)) +
  theme_void()

here("figures",
     "Edgerly_WH_Brown.png") |>
  ggsave(width = 6, height = 6, dpi = 300)
