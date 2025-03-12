options(java.parameters = "-Xmx2G")

library(tidyverse)
library(r5r)

schools <- tibble(school = factor(c("WHCIS (115 Sycamore Street)",
                                    "Edgerly Education Center",
                                    "Trum Field",
                                    "Brown School",
                                    "Kennedy School", 
                                    rep("Other K-8 school", 4)),
                                  levels = c("WHCIS (115 Sycamore Street)",
                                             "Edgerly Education Center",
                                             "Trum Field",
                                             "Brown School", 
                                             "Kennedy School", "Other K-8 school")),
                  lat = c(42.391501258379805,
                          42.3876187927341, 
                          42.397961975144824, 
                          42.397431825014976, 
                          42.38924933782342, 
                          42.406118297446874,
                          42.37905427619121,
                          42.39744670040332,
                          42.38584202754295),
                  lon = c(-71.09888308341662,
                          -71.08720578602485,
                          -71.10724327216819,
                          -71.11412655504013,
                          -71.11555456799908,
                          -71.12633659736886,
                          -71.09860349311977,
                          -71.09547133152927,
                          -71.08622473784972)) |>
  st_as_sf(coords = c("lon", "lat"), crs = "WGS84")

student_points <- st_read(here("student_points.geojson")) |>
  st_transform("WGS84")

points <- student_points |>
  mutate(id = seq(1, nrow(student_points))) 

schools <- schools |>
  mutate(id = seq(1, nrow(schools))) 

somer_core <- setup_r5(here("network"))

tt <- travel_time_matrix(somer_core, 
                         origins = points, 
                         destinations = schools)


stop_r5()

tt_wide <- tt |> pivot_wider(names_from = to_id,
                             values_from = travel_time_p50)

tt_prior <- tt |>
  mutate(id = as.numeric(from_id),
         toId = as.numeric(to_id)) |>
  filter(toId != 2, toId != 3) |>
  group_by(id) |>
  mutate(min_time_prior = min(travel_time_p50)) |>
  mutate(closest_school = min_time_prior == travel_time_p50) |>
  filter(closest_school) |>
  group_by(id) |>
  summarise(min_time_prior = mean(travel_time_p50),
            close_school_prior = min(to_id))

tt_now <- tt |>
  mutate(id = as.numeric(from_id),
         toId = as.numeric(to_id)) |>
  filter(toId != 1, toId != 3) |>
  group_by(id) |>
  mutate(min_time_now = min(travel_time_p50)) |>
  mutate(closest_school = min_time_now == travel_time_p50) |>
  filter(closest_school) |>
  group_by(id) |>
  summarise(min_time_now = mean(travel_time_p50),
            close_school_now = min(to_id))

tt_01_01B <- tt |>
  mutate(id = as.numeric(from_id),
         toId = as.numeric(to_id)) |>
  filter(toId != 2, toId != 3, toId != 4) |>
  group_by(id) |>
  mutate(min_time_now = min(travel_time_p50)) |>
  mutate(closest_school = min_time_now == travel_time_p50) |>
  filter(closest_school) |>
  group_by(id) |>
  summarise(min_time_01_01B = mean(travel_time_p50),
            close_school_01_01B = min(to_id))

tt_04 <- tt |>
  mutate(id = as.numeric(from_id),
         toId = as.numeric(to_id)) |>
  filter(toId != 1, toId != 2, toId != 4) |>
  group_by(id) |>
  mutate(min_time_now = min(travel_time_p50)) |>
  mutate(closest_school = min_time_now == travel_time_p50) |>
  filter(closest_school) |>
  group_by(id) |>
  summarise(min_time_04 = mean(travel_time_p50),
            close_school_04 = min(to_id))

points <- points |>
  left_join(tt_prior) |>
  left_join(tt_now) |>
  left_join(tt_01_01B) |>
  left_join(tt_04)

st_write(points, here("student-walk-times.geojson"), append = FALSE)
